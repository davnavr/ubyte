# Planned Features

Features further down the list are ones I think are more difficult or will take more time to implement.

- Update runtime to have a simpler way to represent registers (just use RuntimeStruct)
- Multiple class inheritance to allow for interfaces/mixins/traits? (still need to fix field lookup problem in runtime)
- Comparison instructions
- Exception handling (`try`, `catch`, and `finally`)
- Static Single Assignment
- Inline `name`, signature (`type` and `method`), and `code` in text format
- Figure out generics (advantages of reification and erasure)
- Interactive runtime to help with debugging
- Debug format
- Transpilation to CIL bytecode and metadata
