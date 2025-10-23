---
layout: post
title:  "Morel Rust release 0.2.0"
date:   2025-10-23 01:30:00 -0800
author: Julian Hyde
image:  /assets/img/OldDesignShop_MushroomSpringMorel-240x240.jpg
tweet:  https://x.com/julianhyde/status/1981440836467642880
---

I am pleased to announce
[release 0.2.0](https://github.com/hydromatic/morel-rust/blob/main/CHANGELOG.md#020--2025-10-23)
of [Morel Rust](https://github.com/hydromatic/morel-rust/).

The Morel language has an existing implementation in Java
([Morel Java](https://github.com/hydromatic/morel/) version 0.7 was
[released in June]({% post_url 2025-06-08-morel-release-0-7-0 %}) and
0.8 is coming soon) but this is the beginning of a brand-new Rust
runtime.

### What's in release 0.2.0

This release focuses on Morel's underpinnings as a functional
programming language. It can parse any program, and execute
simple programs that consist of expressions, function
declarations, and lambdas (closures).

Here's a quick example showing what works today.
First, use `cargo` to build Morel and start a shell:

```bash
$ cargo run
morel-rust version 0.2.0 (rust version 1.90.0)
-
```

Next, you can enter some commands:

```sml
(* Define a recursive function *)
fun factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1);

(* Use lambdas and higher-order functions *)
val squares = List.map (fn x => x * x) [1, 2, 3, 4, 5];

(* Compose functions *)
val sumOfSquares =
  List.foldl (fn (x, y) => x + y) 0 (List.map (fn x => x * x) [1, 2, 3, 4, 5]);
```

This demonstrates core functional programming: recursion,
higher-order functions, and composition. Support for programs
that contain queries---the `from`, `exists`, and `forall`
keywords---and user-defined types will come later.

A caveat: this is pre-alpha software. Expect bugs, crashes, and
minimal error handling. We've focused on getting the foundations
right---the Hindley-Milner type deduction algorithm, an
evaluation environment that handles recursive functions and
closures---rather than polish. If you'd like to
contribute---fixing bugs, adding features, or improving
documentation---please join us!

### Why Rust?

Why create a Rust runtime for Morel when there is already a
Java runtime? Rust brings significant advantages for data
processing workloads.

Rust processes in-memory data at exceptional speed, with
zero-cost abstractions and no garbage collection pauses. It
integrates naturally with modern data infrastructure: Apache
DataFusion for query execution, Arrow for columnar processing,
and Parquet for efficient storage. Memory safety comes without
runtime overhead, and the resulting binaries are ideal for
cloud-native deployments.

Having multiple runtimes also underscores a key design principle: when
writing a Morel program, you don't need to think about implementation
details. Your choice of runtime is separate from your choice of
language. Choose Java for its ecosystem and JVM integration, or Rust
for performance and modern infrastructure. Programs are portable
across both.

But the most important "runtime" is wherever your data already
lives---Iceberg tables on object storage, Kafka topics, or SQL
engines like Snowflake, BigQuery, or Postgres. That's why query
planning, federation, and SQL dialect translation are central to
Morel's design. The compiler can push computation to the data,
regardless of which Morel implementation you're using.

### Morel is a language, not a framework

Morel is a complete language, not a framework. When you have a
data problem, you can solve it entirely in Morel---no jumping
between languages, no glue code, and no framework boundaries to
cross.

With a framework, you're constantly context-switching: Python
for orchestration, SQL for queries, Java for business logic,
and Spark for transformations. Morel lets you express the entire
solution in one language, bringing the benefits of functional
programming---type safety, composability, and refactoring---to data
engineering.

### Choose your runtime, keep your code

Because Morel is a language with multiple implementations, your
Morel programs are portable across runtimes. Write your code
once, and run it on either Java or Rust---whichever fits your
deployment needs. Users shouldn't notice---and don't need to
care---which implementation they're running.

One reason that Morel Rust has developed quickly is that we can
run Morel Java's test scripts unchanged. We are gradually
enabling tests as functionality comes online, proving
portability in practice.

### Learn more

To find out more about Morel, read about its
[goals]({% post_url 2020-02-25-morel-a-functional-language-for-data %})
and [basic language]({% post_url 2020-03-03-morel-basics %}),
and find a full definition of the language in the
[query reference](https://github.com/hydromatic/morel/blob/main/docs/query.md)
and the
[language reference](https://github.com/hydromatic/morel/blob/main/docs/reference.md).

If you have comments, please reply on
[Bluesky @julianhyde.bsky.social](https://bsky.app/profile/julianhyde.bsky.social)
or Twitter:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>

<!--
This article
[has been updated](https://github.com/julianhyde/share/commits/main/blog/{{ page.path }}).
-->

<small>Apache Arrow, Apache DataFusion, Apache Iceberg, Apache
Parquet, and Apache Kafka are trademarks of the Apache Software
Foundation.</small>
