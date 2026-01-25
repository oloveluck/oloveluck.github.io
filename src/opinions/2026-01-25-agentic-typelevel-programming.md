---
title: Agentic Coding and the Resurgence of Type-Level Programming
date: 2026-01-25
---

*How AI code generation changes the calculus on language complexity*

Functional programming and type-level languages have always offered a tradeoff: steeper learning curve upfront, fewer bugs and better composability long-term. Scala, OCaml, and Haskell have proven this at scale—Twitter, Databricks, Jane Street. Yet they remain niche. Not because they don't work, but because writing code in them is harder, and that compounds when scaling engineering teams.

Coding agents change this calculus fundamentally.

## Why Complex Languages Stayed Niche

For decades, the bottleneck in software was writing code. Python, JavaScript, and Java optimized for that—easy to write, accessible across skill levels.

Companies that chose Scala learned this the hard way. Twitter and Databricks built remarkable systems, but faced chronic hiring problems. The talent pool for idiomatic Scala was tiny compared to Java or Python. Technical benefits were real; organizational headwinds often mattered more.

Under the old economics, optimizing for ease of writing made sense.

## The New Bottleneck

Coding agents make writing code dramatically easier. What took hours now takes minutes.

But here's the key insight: **AI hasn't improved our ability to verify that code is correct.**

It hasn't made it easier to understand codebases, reason about edge cases, or ensure systems match intent. We now have more code to verify with less understanding of it.

The bottleneck has shifted from generation to verification. This changes which language features matter.

## Languages Encode Ways of Thinking

If AI generates code in any language, does choice still matter? Yes—languages aren't interchangeable syntax. They encode different mental models:

- **C** — direct memory, manual resource management
- **Rust** — ownership as compile-time guarantees  
- **Go** — simplicity, goroutine concurrency
- **Haskell** — enforced purity, explicit effects
- **Python** — readability, minimal ceremony
- **TypeScript** — gradual typing, multi-paradigm
- **Scala** — functional-first with OO support, runs on JVM/JS/Native

When verification is the bottleneck, languages that make verification easier become more valuable.

## Type Systems as Verification Tools

A strong type system enforces contracts at compile time:

```scala
def processPayment(amount: PositiveInt, account: ValidatedAccount): Either[PaymentError, Receipt]
```

The compiler won't allow negative amounts or unvalidated accounts. Whole bug categories become impossible.

This matters more with AI-generated code. LLMs pattern-match on training data—they generate plausible code that may violate invariants only you know about. Type systems make invariants explicit and catch AI mistakes before runtime.

## The Maintenance Problem

AI-built codebases create new challenges. No one wrote the code in the traditional sense—they prompted it. No one deeply understands why implementations were chosen.

When you write code by hand, writing creates understanding. You can't implement an algorithm without knowing how it works. AI severs this link: you can have working code without understanding why it works.

## What Agents Enable

For system-level thinkers, agents remove incidental friction. Senior engineers shouldn't spend cycles on whether Python uses `self` or JavaScript uses `this`. What matters is reasoning about consistency, failure modes, and scale.

Developer roles shift toward higher abstraction: less syntax, more system design; less debugging details, more specifying constraints.

## Library Ecosystems Matter Less

Python and JavaScript won partly on ecosystem size. Need ML? It's in Python.

Building yourself was wrong because code was expensive. Libraries represent thousands of hours you don't pay for.

AI changes this. When code is cheap to write, build-vs-depend shifts. Dependencies carry hidden costs—version conflicts, security issues, abandoned maintenance. A focused implementation can beat a bloated library.

For ML frameworks and cloud SDKs, established libraries still win. But for teams weighing strong types with small ecosystems against weak types with large ones, the tradeoff has shifted.

## Cognitive Complexity at Scale

Pure FP, effect systems, and referential transparency exist to manage cognitive complexity as codebases grow.

> **With mutable state, cognitive complexity grows superlinearly. With referential transparency, it grows linearly.**

In mutable code, any function might depend on global state or perform hidden IO. Understanding requires tracing entire call graphs while tracking state changes.

Referential transparency breaks this: same inputs always produce same outputs, no side effects. You reason about functions in isolation. Effect systems like Cats Effect make effects visible in types without eliminating them—an `IO[A]` describes an effect that can be composed and reasoned about before execution.

### The AI Multiplier

AI agents face the same complexity challenges humans do—limited context windows, no tacit knowledge. If understanding code requires tracing mutable state, agents struggle just like humans, but with less intuition.

Reducing complexity pays compound returns: developers understand faster, agents make fewer errors, testing is easier (pure functions are deterministic), refactoring is safer (referential transparency guarantees substitutability).

A signature like `IO[Either[PaymentError, Receipt]]` communicates everything. No searching for hidden state mutations or implicit database calls. The types rule out everything they don't declare.

## Type-Level Programming for AI

Rust and TypeScript adoption reflects industry movement toward compile-time guarantees. When you write every line, you carry implicit knowledge. When AI generates code, that knowledge never forms. Compile-time guarantees offset the uncertainty.

Scala's Typelevel ecosystem—Cats, Cats Effect, fs2, http4s—tracks effects in type signatures. Types tell you not just what data flows through functions, but what they *do*.

Agents working with typed code don't need full context—they satisfy the types. Violations are caught immediately by the compiler. With dynamic typing, agents infer behavior from patterns and guess wrong more often.

## The Hiring Argument Weakens

The case against complex languages was hiring difficulty. But when AI writes most of the code, learning curves matter less. What matters is whether the type system catches AI mistakes.

As AI-built codebases grow, developers report declining reliability—agents lack context, hallucinate signatures, violate patterns. Strong type systems address exactly these problems.

## Rethinking Language Choice

The cost of writing complex code has dropped. The cost of verifying correctness hasn't.

For organizations where correctness matters—healthcare, finance, infrastructure—type-level programming is stronger than ever. Not despite AI, but because of it.

---

## My Take: Why Scala 3

I believe Scala 3 is the best language foundation for LLM-assisted development.

**Mature functional ecosystem.** Cats and Cats Effect provide the most complete libraries for referential transparency. Effects tracked in types, errors explicit, composition principled. This is practical infrastructure for systems both humans and AI can reason about.

**Concise and readable for LLMs.** Functional Scala expresses programs in less code—fewer tokens, more codebase in context. The declarative style reads like natural language: `users.filter(_.isActive).map(_.email)` describes *what*, not *how*. The same properties that help humans reason—explicit data flow, no hidden state—help LLMs pattern-match correctly.

**Practical flexibility.** Unlike Haskell or OCaml, Scala supports OO and procedural patterns when appropriate. Teams adopt FP incrementally. Full Java interop means decades of battle-tested libraries remain accessible.

**Runtime flexibility.** Scala compiles to JVM for enterprise stability, JavaScript for frontends, or native via LLVM. Combined with a type system more expressive than TypeScript or Rust, it's uniquely positioned for the AI era.

The industry is moving toward compile-time guarantees. Scala is an established, production-proven point further along that trajectory—with the flexibility to actually adopt it.

---

*The question isn't whether AI will write our code. It's whether we can verify AI output at the speed we need. Type systems are one of the best tools we have.*
