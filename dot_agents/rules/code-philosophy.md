# Code Philosophy

Applies when writing or modifying code.

## Outcome

Code stays minimal. Each abstraction, indirection, and compatibility shim earns its place by removing more cost than it adds.

## Constraints

- Avoid over-engineering. Solve the problem in front of you, not the problem you imagine arriving next.
- No unnecessary abstractions. Do not introduce helpers, base classes, or indirection without an existing duplication or pain they remove.
- Delete unused code completely. Do not leave it commented out, behind feature flags, or wrapped in dead branches as a "just in case" measure.
- No backward-compatibility hacks for code paths the user has not asked to preserve.
