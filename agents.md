# F# Coding Style Guidelines

These guidelines apply to all F# code in this repository.

## Core Principles

- Prefer immutable data and pure functions by default.
- Prefer modules and functions over classes in most cases.
- Minimize mutable state and keep mutation scope as small as possible.
- Favor explicit domain modeling with records and discriminated unions.
- Optimize for readability and long-term maintainability over cleverness.

## Data Structures

- Use immutable F# collections first: `list`, `Map`, `Set`, and immutable arrays when appropriate.
- For specialized persistent or high-performance functional collections, prefer `FSharpX.Collections`.
- Do not expose mutable collections (for example, `Dictionary`) in public module APIs.
- If mutable structures are needed internally for performance, wrap them behind an immutable functional interface.
- Where possible, use rich F# Discriminated Unions instead of simple enums as flags to represent case-oriented data. Example:

```fsharp
// *avoid*
type AnEnum = Bool | Text
type ARec =  {flag:AnEnum; boolValue:bool; textValue:string}

// *instead use*
type AnEnum = Bool of bool | Text of string
```

## Modules vs Classes

- Use modules as the default organization unit for business logic and transformations.
- Use classes when they are clearly beneficial or required:
  - dependency injection boundaries
  - encapsulating stateful resources (for example, sockets, DB handles, caches)
  - implementing .NET interfaces and framework integration points
- Keep classes thin and delegate core logic to pure module functions.

## Mutability Policy

- Prefer immutable `let` bindings.
- When mutation is required, use `let mutable` in the narrowest possible scope.
- Avoid `ref` cells unless there is a compelling interoperability reason.
- Do not spread mutation across multiple functions when one local mutable loop/state machine is sufficient.

## Error Handling and Domain Modeling

- Model domain success/failure with discriminated unions where feasible.
- Use `Result` for expected business-level failures.
- Use exceptions for truly exceptional/runtime cases (I/O, infrastructure, invalid external state).
- Avoid stringly-typed error handling.

## API and Naming Conventions

- Prefer explicit parameter and return types for public functions.
- Keep internal/local functions concise and allow type inference where it improves readability.
- Use descriptive names for domain concepts; avoid cryptic abbreviations.
- Prefer single-case discriminated unions over primitive type aliases for domain values.

## Formatting and Layout

Based on Microsoft F# style guidance and common community conventions:

- Use spaces, not tabs.
- Use consistent indentation (4 spaces recommended).
- Use one blank line between top-level declarations.
- Keep pipelines vertically aligned and readable:

  ```fsharp
  let result =
      input
      |> stepOne
      |> stepTwo
      |> stepThree
  ```

- Avoid alignment that depends on identifier length.
- Prefer multiline formatting for long argument lists and complex match/if expressions.
- Keep `open` statements topologically ordered by dependency layers.

## Object-Oriented Features

- Prefer composition over inheritance.
- Use object expressions for small interface implementations when a full class adds no value.
- Avoid inheritance-heavy hierarchies unless required by framework constraints.

## Null and Interop Boundaries

- Avoid nulls in domain logic.
- Convert nullable/null-returning .NET APIs to `option` at the boundary.
- Validate external inputs early and fail fast with clear exceptions.

## Tooling

- Use Fantomas as the standard formatter.
- Keep formatting rules consistent with Fantomas defaults unless the repo defines a different profile.
- Run formatting before finalizing changes that touch F# source.

## Performance Guidance

- Default to immutable design first, then optimize based on measurement.
- Introduce local mutation or specialized structures only when profiling indicates a real bottleneck.
- Encapsulate performance-oriented mutable implementations behind stable functional APIs.

## Practical Rule of Thumb

- Start pure and module-first.
- Add mutation only where measured and justified.
- Introduce classes only for integration/stateful boundaries.
- Keep the public surface area simple, explicit, and domain-oriented.


# Experimental Coding
- Put temporary, exploratory and/or experimental code in the /temp directory of the project.
- ß
## Parallelization
For parallel processing, if feasible use FSharp.Control.AsyncSeq as in:
```fsharp
#r "nuget: FSharp.Control.AsyncSeq"
open FSharp.Control

let workOnChunk data = async {
    return data
}

let data = [1;2;3]
data 
|> AsyncSeq.ofSeq
|> AsyncSeq.bufferByCountAndTime 16 1
|> AsyncSeq.mapAsyncParallelThrottled 3 workOnChunk
```
