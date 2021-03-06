#ifndef BYTECODE_H
#define BYTECODE_H

--- Eagerly evaluate a primitive operation.
---
--- The offset in the constant table pointing to the primitive must be located
--- juste after this opcode.
#define BYTECODE_PRIM 0

--- Move the instruction pointer to a new location in the CODE section.
---
--- The offset (from the beginning of the CODE section) must be found right
--- after.
---
--- This opcode adds a new entry on top of the call stack corresponding to its
--- current offset + 4 (to accomodate for the additional argument).
#define BYTECODE_JUMP 1

--- Push a new constant on top of the data stack.
---
--- The offset in the constant table must be found right after this opcode.
#define BYTECODE_PUSH 2

--- Return to the last caller address found on the call stack.
#define BYTECODE_RET 3

--- Unquote (evaluate) the last quote identifier found on the data stack.
#define BYTECODE_UNQUOTE 4

--- We arrived at the end of our bytecode list.
---
--- This is used to explicitly break the evaluation loop instead of checking the size.
#define BYTECODE_EXIT 5

#endif
