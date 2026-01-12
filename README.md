<div align="center">
    <h1>kernel-lang</h1>
    <h2>Compiler for the Kernel language</h2>
</div>

<div align="center">
  <img alt="Stargazers" src="https://img.shields.io/github/stars/budchirp/kernel-lang?style=for-the-badge&colorA=0b1221&colorB=ff8e8e" />
  <img alt="Last commit" src="https://img.shields.io/github/last-commit/budchirp/kernel-lang?style=for-the-badge&colorA=0b1221&colorB=BDB0E4" />
  <img alt="Issues" src="https://img.shields.io/github/issues/budchirp/kernel-lang?style=for-the-badge&colorA=0b1221&colorB=FBC19D" />
</div>

# TODO

## ✅ Completed Features

### Core Language Features
- [x] **AST & Parser** - Complete Abstract Syntax Tree with all node types
- [x] **Type System** - Integer, Float, String, Boolean, Void, Pointer, Array, Struct, Function types
- [x] **Symbol Table** - Scope management with variables, functions, and types
- [x] **Semantic Analysis** - Type checking, variable resolution, function validation
- [x] **Code Generation** - LLVM backend with complete IR generation

### Advanced Features
- [x] **Generic Functions** - Template-like functionality with type parameters
- [x] **Monomorphization** - Compile-time specialization of generic functions
- [x] **Function Overloading** - Multiple functions with same name but different signatures
- [x] **Variadic Functions** - Functions accepting variable number of arguments as arrays
- [x] **Type Inheritance** - Struct inheritance with field merging
- [x] **Generic Constraints** - Type bounds for generic parameters (e.g., `<T: Numeric>`)
- [x] **Type Coercion** - Implicit upcasting, explicit casting, strict type compatibility
- [x] **Memory Management** - Heap allocation, pointer operations, array indexing
- [x] **Control Flow** - If expressions, while/for loops, return statements
- [x] **Operators** - Arithmetic, comparison, logical, assignment operators

## 🚧 Potential Future Features

### Language Extensions
- [ ] **Struct fields** - Object-oriented programming support
- [ ] **Enums & Pattern Matching** - Sum types with exhaustive matching
- [ ] **Modules & Imports** - Multi-file projects and dependency management
- [ ] **Closures** - Anonymous functions with captures

### Compiler Improvements
- [ ] **Optimization Passes** - LLVM optimization pipeline integration
- [ ] **Debug Information** - Source-level debugging support
- [ ] **Incremental Compilation** - Faster rebuilds for changed files
- [ ] **Cross-Compilation** - Support for multiple target architectures
- [ ] **Error Recovery** - Continue compilation after syntax errors
- [ ] **Language Server** - IDE integration and autocomplete

### Standard Library
- [ ] **Core Library** - Basic data structures (vectors, hashmaps, strings)
- [ ] **I/O Operations** - File system, networking, console I/O
- [ ] **Math Library** - Advanced mathematical functions
- [ ] **Memory Management** - RAII

### Tools & Ecosystem
- [ ] **Package Manager** - Dependency resolution and project management
- [ ] **Build System** - Native build tool integration
