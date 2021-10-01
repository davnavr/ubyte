# Planned Features

- Module imports
- Unsigned arithimetic operations (maybe just have registers keep track of whether they are signed or unsigned)
- Overflow checking arithmetic operations
- Exception handling (`try`, `catch`, and `finally`)
- Inline `name`, signature (`type` and `method`), and `code` in text format
- Methods marked as being implemented elsewhere (such as an interpreter or being provided at transpile time)
- Virtual method calls
- Single class inheritance
- Multiple class inheritance to allow for interfaces/mixins/traits?
- Abstract classes
- Proper array types (could make it important like the primitive types, or could just declare a generic type in a standard library module)
- Figure out generics (advantages of reification and erasure)
- Some way of specifiying which method overrides what explicitly for all methods (essentially preventing implicit interface implementations like in C#)
- Tail calls
- Interactive runtime to help with debugging
- Debug format
- Transpilation to CIL bytecode and metadata
