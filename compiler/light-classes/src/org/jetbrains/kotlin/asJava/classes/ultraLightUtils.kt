/*
 * Copyright 2010-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.asJava.classes

import com.intellij.psi.*
import com.intellij.psi.impl.cache.TypeInfo
import com.intellij.psi.impl.compiled.ClsTypeElementImpl
import com.intellij.psi.impl.compiled.SignatureParsing
import com.intellij.psi.impl.compiled.StubBuildingVisitor
import com.intellij.psi.impl.light.LightReferenceListBuilder
import com.intellij.psi.impl.light.LightTypeParameterBuilder
import org.jetbrains.kotlin.asJava.LightClassGenerationSupport
import org.jetbrains.kotlin.asJava.elements.KotlinLightTypeParameterListBuilder
import org.jetbrains.kotlin.codegen.ClassBuilderMode
import org.jetbrains.kotlin.codegen.signature.BothSignatureWriter
import org.jetbrains.kotlin.codegen.state.IncompatibleClassTracker
import org.jetbrains.kotlin.codegen.state.KotlinTypeMapper
import org.jetbrains.kotlin.config.JvmTarget
import org.jetbrains.kotlin.descriptors.CallableDescriptor
import org.jetbrains.kotlin.descriptors.ClassifierDescriptor
import org.jetbrains.kotlin.descriptors.TypeParameterDescriptor
import org.jetbrains.kotlin.descriptors.ValueDescriptor
import org.jetbrains.kotlin.descriptors.annotations.AnnotationDescriptor
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.load.kotlin.TypeMappingMode
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.psi.*
import org.jetbrains.kotlin.psi.psiUtil.hasExpectModifier
import org.jetbrains.kotlin.resolve.BindingContext
import org.jetbrains.kotlin.resolve.descriptorUtil.fqNameSafe
import org.jetbrains.kotlin.resolve.descriptorUtil.getAllSuperClassifiers
import org.jetbrains.kotlin.types.KotlinType
import java.text.StringCharacterIterator

internal fun findTooComplexDeclaration(kt: KtClassOrObject): PsiElement? {
    fun KtAnnotationEntry.seemsNonTrivial(): Boolean {
        val name = shortName
        return name == null || name.asString().startsWith("Jvm") && name.asString() != "JvmStatic" //todo aliases?
    }

    fun KtAnnotated.hasNonTrivialAnnotations() = annotationEntries.any(KtAnnotationEntry::seemsNonTrivial)

    if (kt.isAnnotation() ||
        kt.hasModifier(KtTokens.INLINE_KEYWORD) ||
        kt.hasModifier(KtTokens.DATA_KEYWORD) ||
        kt.hasModifier(KtTokens.ENUM_KEYWORD) ||
        kt.hasNonTrivialAnnotations()
    ) {
        return kt
    }

    kt.superTypeListEntries.find { it is KtDelegatedSuperTypeEntry }?.let { return it }

    for (c in kt.allConstructors) {
        if (c.hasNonTrivialAnnotations()) {
            return c
        }
        c.valueParameters.find(KtAnnotated::hasNonTrivialAnnotations)?.let { return it }
    }

    for (d in kt.declarations) {
        if (d.hasNonTrivialAnnotations()) {
            return d
        }
        if (d.hasExpectModifier() || d.hasModifier(KtTokens.SUSPEND_KEYWORD)) {
            return d
        }
        if (d is KtCallableDeclaration) {
            d.valueParameters
                .find { it.hasNonTrivialAnnotations() || it.typeReference?.hasModifier(KtTokens.SUSPEND_KEYWORD) == true }
                ?.let { return it }
        }
        if (d is KtObjectDeclaration && d.isCompanion()) {
            findTooComplexDeclaration(d)?.let { return it }
        }
    }

    if (implementsKotlinCollection(kt)) {
        return kt.getSuperTypeList()
    }

    return null

}

private fun implementsKotlinCollection(kt: KtClassOrObject): Boolean {
    val implementsInterfaces = kt.superTypeListEntries.any { it is KtSuperTypeEntry }
    return implementsInterfaces &&
            (kt.resolve() as? ClassifierDescriptor)?.getAllSuperClassifiers()?.any {
                it.fqNameSafe.asString().startsWith("kotlin.collections.")
            } == true
}


internal fun buildTypeParameterList(decl: KtTypeParameterListOwner, owner: PsiTypeParameterListOwner): PsiTypeParameterList {
    val tpList = KotlinLightTypeParameterListBuilder(owner)
    for ((i, ktParam) in decl.typeParameters.withIndex()) {
        tpList.addParameter(object : LightTypeParameterBuilder(ktParam.name.orEmpty(), owner, i) {
            private val _extendsList : LightReferenceListBuilder by lazyPub {
                val boundList = LightReferenceListBuilder(manager, PsiReferenceList.Role.EXTENDS_BOUNDS_LIST)
                if (ktParam.extendsBound != null || decl.typeConstraints.isNotEmpty()) {
                    for (bound in (ktParam.resolve() as? TypeParameterDescriptor)?.upperBounds.orEmpty()) {
                        val psiType = bound.asPsiType(ktParam, TypeMappingMode.DEFAULT, this)
                        if (psiType is PsiClassType && !psiType.equalsToText(CommonClassNames.JAVA_LANG_OBJECT)) {
                            boundList.addReference(psiType)
                        }
                    }
                }
                boundList
            }
            override fun getExtendsList(): LightReferenceListBuilder = _extendsList

            override fun getParent(): PsiElement = tpList
            override fun getContainingFile(): PsiFile = owner.containingFile
        })
    }
    return tpList
}

internal fun KtAnnotated.hasAnnotation(fqName: FqName) = findAnnotation(fqName) != null

internal fun KtAnnotated.findAnnotation(fqName: FqName): AnnotationDescriptor? = annotationEntries
    .filter { it.shortName == fqName.shortName() } //todo aliases?
    .mapNotNull { LightClassGenerationSupport.getInstance(this.project).analyze(it).get(BindingContext.ANNOTATION, it) }
    .find { it.fqName == fqName }

internal fun KtDeclaration.getKotlinType(): KotlinType? {
    val descriptor = resolve()
    return when (descriptor) {
        is ValueDescriptor -> descriptor.type
        is CallableDescriptor -> descriptor.returnType
        else -> null
    }
}

internal fun KtDeclaration.resolve() = LightClassGenerationSupport.getInstance(project).resolveToDescriptor(this)

// copy-pasted from kotlinInternalUastUtils.kt and post-processed
internal fun KotlinType.asPsiType(decl: KtDeclaration, mode: TypeMappingMode, psiContext: PsiElement = decl): PsiType {
    val typeFqName = constructor.declarationDescriptor?.fqNameSafe?.asString()
    if (typeFqName == "kotlin.Unit" && decl is KtFunction) return PsiType.VOID

    val signatureWriter = BothSignatureWriter(BothSignatureWriter.Mode.TYPE)
    KotlinTypeMapper(
        BindingContext.EMPTY, ClassBuilderMode.LIGHT_CLASSES,
        IncompatibleClassTracker.DoNothing, moduleName(decl),
        JvmTarget.JVM_1_8,
        true, false
    ).mapType(this, signatureWriter, mode)
    val signature = StringCharacterIterator(signatureWriter.toString())

    val javaType = SignatureParsing.parseTypeString(signature, StubBuildingVisitor.GUESSING_MAPPER)
    val typeInfo = TypeInfo.fromString(javaType, false)
    val typeText = TypeInfo.createTypeText(typeInfo) ?: return PsiType.NULL

    val type = ClsTypeElementImpl(psiContext, typeText, '\u0000').type
    if (type is PsiArrayType && decl is KtParameter && decl.isVarArg) {
        return PsiEllipsisType(type.componentType, type.annotationProvider)
    }
    return type
}

internal fun moduleName(context: KtModifierListOwner) = LightClassGenerationSupport.getInstance(context.project).getModuleName(context)
