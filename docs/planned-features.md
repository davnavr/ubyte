# Planned Features

Features further down the list are ones I think are more difficult or will take more time to implement.

- Comparison instructions
- Importing fields
- Labels in text format
- Unsigned arithimetic operations (maybe just have registers keep track of whether they are signed or unsigned)
- Overflow checking arithmetic operations
- Exception handling (`try`, `catch`, and `finally`)
- Inline `name`, signature (`type` and `method`), and `code` in text format
- Methods marked as being implemented elsewhere (such as an interpreter or being provided at transpile time)
- Virtual method calls
- Single class inheritance
- Multiple class inheritance to allow for interfaces/mixins/traits?
- Abstract classes
- Figure out generics (advantages of reification and erasure)
- Some way of specifiying which method overrides what explicitly for all methods (essentially preventing implicit interface implementations like in C#)
- Tail calls
- Allowing main method to accept an array of character arrays
- Interactive runtime to help with debugging
- Transpilation to CIL bytecode and metadata
- Interpreter translating hot paths with [`System.Linq.Expressions.Expression`](https://docs.microsoft.com/en-us/dotnet/api/system.linq.expressions.expression)
- Debug format
