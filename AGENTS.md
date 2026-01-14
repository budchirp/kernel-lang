# Rules for Kernel Language Project

## Naming Conventions

- Always use `snake_case` for function and variable names.
- Use `PascalCase` for enums and structs.
- Do not shorten names (e.g., use `expected` not `exp`, use `token` not `tok`). EXCEPT `ptr` for pointer.
- Keep names short but readable.

## Import Style

- Always use the pattern: `const filename_zig = @import("filename")` and then use its contents.
- Example:
  ```zig
  const token_type_zig = @import("token_type.zig");
  const TokenType = token_type_zig.TokenType;
  ```

## Code Style

- DO NOT use comments.

