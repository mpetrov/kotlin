// "Add '<*, *>'" "false"
// "Add '<*>'" "false"
// ERROR: 2 type arguments expected
public fun foo(a: Any) {
    a is <caret>Map<Int>
}