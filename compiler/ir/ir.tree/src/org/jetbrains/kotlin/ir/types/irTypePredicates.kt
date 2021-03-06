/*
 * Copyright 2010-2018 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
 * that can be found in the license/LICENSE.txt file.
 */

package org.jetbrains.kotlin.ir.types

import org.jetbrains.kotlin.builtins.KotlinBuiltIns
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.ir.symbols.IrClassSymbol
import org.jetbrains.kotlin.name.FqNameUnsafe
import org.jetbrains.kotlin.resolve.DescriptorUtils.getFqName

private fun IrType.isBuiltInClassType(descriptorPredicate: (ClassDescriptor) -> Boolean, hasQuestionMark: Boolean): Boolean {
    if (this !is IrSimpleType) return false
    if (this.hasQuestionMark != hasQuestionMark) return false
    val classSymbol = this.classifier as? IrClassSymbol ?: return false
    return descriptorPredicate(classSymbol.descriptor)
}

private fun IrType.isNotNullClassType(fqName: FqNameUnsafe) = isClassType(fqName, hasQuestionMark = false)
private fun IrType.isNullableClassType(fqName: FqNameUnsafe) = isClassType(fqName, hasQuestionMark = true)

private fun IrType.isClassType(fqName: FqNameUnsafe, hasQuestionMark: Boolean): Boolean {
    if (this !is IrSimpleType) return false
    if (this.hasQuestionMark != hasQuestionMark) return false
    val classSymbol = this.classifier as? IrClassSymbol ?: return false
    return classFqNameEquals(classSymbol.descriptor, fqName)
}

private fun classFqNameEquals(descriptor: ClassDescriptor, fqName: FqNameUnsafe): Boolean =
    descriptor.name == fqName.shortName() && fqName == getFqName(descriptor)

fun IrType.isAny(): Boolean = isBuiltInClassType(KotlinBuiltIns::isAny, hasQuestionMark = false)
fun IrType.isNullableAny(): Boolean = isBuiltInClassType(KotlinBuiltIns::isAny, hasQuestionMark = true)

fun IrType.isString(): Boolean = isNotNullClassType(KotlinBuiltIns.FQ_NAMES.string)
fun IrType.isNullableString(): Boolean = isNullableClassType(KotlinBuiltIns.FQ_NAMES.string)
fun IrType.isArray(): Boolean = isNotNullClassType(KotlinBuiltIns.FQ_NAMES.array)
fun IrType.isNothing(): Boolean = isNotNullClassType(KotlinBuiltIns.FQ_NAMES.nothing)
fun IrType.isPrimitiveType(): Boolean =
    isBuiltInClassType(KotlinBuiltIns::isPrimitiveClass, hasQuestionMark = false)

fun IrType.isNullablePrimitiveType(): Boolean =
    isBuiltInClassType(KotlinBuiltIns::isPrimitiveClass, hasQuestionMark = true)

fun IrType.isMarkedNullable() = (this as? IrSimpleType)?.hasQuestionMark ?: false

fun IrType.isUnit() = isNotNullClassType(KotlinBuiltIns.FQ_NAMES.unit)
fun IrType.isNullableUnit() = isNullableClassType(KotlinBuiltIns.FQ_NAMES.unit)
fun IrType.isUnitOrNullableUnit() = this.isUnit() || this.isNullableUnit()

fun IrType.isBoolean(): Boolean = isNotNullClassType(KotlinBuiltIns.FQ_NAMES._boolean)
fun IrType.isChar(): Boolean = isNotNullClassType(KotlinBuiltIns.FQ_NAMES._char)
fun IrType.isByte(): Boolean = isNotNullClassType(KotlinBuiltIns.FQ_NAMES._byte)
fun IrType.isShort(): Boolean = isNotNullClassType(KotlinBuiltIns.FQ_NAMES._short)
fun IrType.isInt(): Boolean = isNotNullClassType(KotlinBuiltIns.FQ_NAMES._int)
fun IrType.isLong(): Boolean = isNotNullClassType(KotlinBuiltIns.FQ_NAMES._long)
fun IrType.isFloat(): Boolean = isNotNullClassType(KotlinBuiltIns.FQ_NAMES._float)
fun IrType.isDouble(): Boolean = isNotNullClassType(KotlinBuiltIns.FQ_NAMES._double)
fun IrType.isNumber(): Boolean = isNotNullClassType(KotlinBuiltIns.FQ_NAMES.number)

fun IrType.isNullableBoolean(): Boolean = isNullableClassType(KotlinBuiltIns.FQ_NAMES._boolean)
fun IrType.isNullableLong(): Boolean = isNullableClassType(KotlinBuiltIns.FQ_NAMES._long)
fun IrType.isNullableChar(): Boolean = isNullableClassType(KotlinBuiltIns.FQ_NAMES._char)
