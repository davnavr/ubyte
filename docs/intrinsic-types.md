# Instrinsic Types

The types not defined in any module are known as intrinsic types and they:
- Do not define any methods
- Do not inherit any types
- Have instructions that operate directly on their instances

Instrinsic types currently consist of:
- Primitive Types
  * Consists of all the numeric types including `u32`, `f32`, `s64`, `snative`, `bool`, `char16`, etc.
  * Has corresponding arithmetic instructions such as `add`, `mul`, `incr`, `or`, `rotl`, etc.
- Pointer Types
  * Consists of all pointer types
  * Pointer arithmetic is possible with certain instructions such as `add`, `mul`, `div`, or `sub`
- `any` object reference
  * Represents an untyped object reference, all object references can be converted to `any`
  * Converting from `any` to a specific object reference may be unsafe without runtime checks
- Array Types
  * Consists of all array types (e.g. `any[]`, `u32[]`, `class MyClass[]`, `char32[][]`)
  * Has corresponding instructions (`obj.arr.`)
