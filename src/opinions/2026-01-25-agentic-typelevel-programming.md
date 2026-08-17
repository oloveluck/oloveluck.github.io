---
title: Agentic Coding and the Resurgence of Type-Level Programming
date: 2026-01-25
---

*How AI code generation changes the calculus on language complexity*

Functional programming and type-level languages have always offered a tradeoff: steeper learning curve upfront, fewer bugs and better composability long-term. Scala, OCaml, and Haskell proved this at scale—Twitter, Databricks, Jane Street—yet stayed niche, because writing code in them is harder.

Coding agents change this calculus fundamentally.

## The New Bottleneck

For decades, the bottleneck in software was writing code, and languages optimized for it. Companies that chose Scala got real technical benefits and chronic hiring problems; the organizational headwinds often mattered more.

Agents make writing code dramatically easier. But here's the key insight: **AI hasn't improved our ability to verify that code is correct.** We now have more code to verify, written by no one, with less understanding of it. The bottleneck has shifted from generation to verification—and that changes which language features matter.

## Types as Verification

A strong type system enforces contracts at compile time, making whole bug categories impossible. This matters more with AI-generated code: LLMs generate plausible code that may violate invariants only you know about. Types make those invariants explicit, and agents don't need full context to work in typed code—they satisfy the types, and violations are caught immediately.

This also dissolves the classic objection. The case against complex languages was hiring difficulty; when AI writes most of the code, learning curves matter less than whether the compiler catches AI mistakes.

## Complexity Stays Linear

> **With mutable state, cognitive complexity grows superlinearly. With referential transparency, it grows linearly.**

In mutable code, any function might depend on hidden state, so understanding means tracing whole call graphs. Referentially transparent functions can be reasoned about in isolation—and agents face the same limits humans do: bounded context, no tacit knowledge. Reducing complexity pays twice.

## Libraries Matter Less

Python and JavaScript won partly on ecosystem size, back when code was expensive. When code is cheap, build-vs-depend shifts: a focused implementation can beat a bloated dependency with its version conflicts and abandonment risk. Established ML frameworks and cloud SDKs still win—but the tradeoff between strong types with small ecosystems and weak types with large ones has moved.

## My Take: Why Scala 3

I believe Scala 3 is the best language foundation for LLM-assisted development.

**Mature functional ecosystem.** Cats and Cats Effect provide the most complete libraries for referential transparency. Effects tracked in types, errors explicit, composition principled.

**Concise and readable for LLMs.** Functional Scala expresses programs in less code—fewer tokens, more codebase in context. The declarative style describes *what*, not *how*, and the properties that help humans reason help LLMs pattern-match correctly.

**Practical flexibility.** Unlike Haskell or OCaml, Scala supports OO and procedural patterns when appropriate. Teams adopt FP incrementally, and full Java interop keeps decades of battle-tested libraries accessible.

**Runtime flexibility.** Scala compiles to JVM, JavaScript, or native—with a type system more expressive than TypeScript or Rust.

The industry is moving toward compile-time guarantees. Scala is an established, production-proven point further along that trajectory—with the flexibility to actually adopt it.

---

*The question isn't whether AI will write our code. It's whether we can verify AI output at the speed we need. Type systems are one of the best tools we have.*
