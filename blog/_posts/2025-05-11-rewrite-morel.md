---
layout: post
title:  "Should Morel be rewritten in Rust?"
date:   2025-05-11 12:00:00 -0800
author: Julian Hyde
image:  /assets/img/OldDesignShop_MushroomSpringMorel-240x240.jpg
tweet:  https://x.com/julianhyde/status/1897425406279737401
---

There are many excellent and innovative projects happening in the Rust
data ecosystem. I am frequently asked whether
[Morel](https://github.com/hydromatic/morel) should be one of
them. Thinking about this question gave me some insights into Morel's
strengths, and where our priorities should be.

# Rust and its ecosystem

Rust is an excellent language for building a data processing
system. Though it is a high-level language with type safety and memory
protection, Rust avoids the tax that is paid by most high-level
languages, namely garbage collection and the unpredictable pauses that
it can cause.

Rust also has a vibrant ecosystem; enthusiasts are rebuilding the
whole stack because it seems like fun. That's mostly a good thing,
because a rebuild is a chance to do things better. But sometimes it's
a chance to make *different* mistakes. (I say this having been around
for the [last revolution](https://hadoop.apache.org/), rewriting the
DBMS as a Java-based distributed system.)

Rewriting in Rust would incur an opportunity cost. It would take away
resources from other goals, which I describe below. (If there were no
opportunity cost -- if one or two engineers were prepared to spend 3-6
months of their spare time translating Morel's 50K-or-so lines of Java
to high-quality Rust -- then I would probably welcome such a
contribution. However, such dedicated volunteer efforts are
understandably rare.) Let's remind ourselves of the goal.

# Morel and its goals

Morel is a language, not a framework or a library. It necessarily has a
reference implementation -- the only implementation, at present -- and
that happens to be written in Java. Building a language -- as opposed
to a framework or a library -- means focusing on the design of that
language, as opposed to its initial reference implementation.  Key
elements of the design are syntax, type system, and semantics.
Semantics are especially important because they determine what
execution plans are valid for its programs, and in what ways its
compiler/optimizer is allowed to transform that program.

Think of some great languages --
[C](https://web.archive.org/web/20250115055354/https://www.bell-labs.com/usr/dmr/www/chist.html),
Lisp,
[SQL](https://stackoverflow.com/questions/16020999/was-the-original-sql-written-in-assembly-or-c),
Java, Python, Standard ML, and
[Rust](https://en.wikipedia.org/wiki/Rust_(programming_language)#:~:text=During%20the%20early%20years%2C%20the,about%2038%2C000%20lines%20of%20OCaml)
are some examples -- and tell me what language their original compiler
was written in. Probably you can't. The initial implementation served
its purpose -- letting the language evolve to something useful and
usable -- and in time was discarded.

So, what is Morel for? Morel is a functional query language. It has
the expressive power and type system of a functional programming
language, and as a query language has built-in support for relational
algebra. Its optimizer uses algebraic laws to radically transform
programs to exploit parallelism, distributed processing, data
organization, and pre-computed results.

Some implications of this:
* **Morel must support multiple runtimes**. Programs can run locally
  (using the interpreter written in Java), but also on a distributed
  framework such as Apache Spark. It also supports federated
  execution, translating programs (or portions of programs) into the
  language of the system that contains the data (such as SQL).  Tying
  Morel tightly to a particular runtime, even an excellent one such as
  Rust-based [Apache DataFusion](https://datafusion.apache.org/),
  could detract from that goal.
* **Morel's optimizer must be extensible in Morel**. When Morel
  connects to new data sources, these data sources have their own
  algebra (operators, transformation rules, constraints, and
  statistics). To effectively optimize programs on these data sources,
  Morel's optimizer must understand that algebra. In Morel's current
  optimizer, rules are defined as Java classes that implement
  interfaces defined by the
  [Apache Calcite](https://calcite.apache.org/) planning engine.
  That means that to add a data source, bulk data type (say vector,
  matrix or geospatial polygon), operator (say multiplying matrices,
  or finding sets of overlapping polygons), or transformation rule
  (pushing filters into matrices or polygon sets), you need to leave
  Morel.  Those things are all more difficult to write without Morel's
  expressive power; Morel should allow these kinds of extensibility
  without leaving the language.
* **Morel should bootstrap its compiler but not its runtime**. Every
  programming language aspires to implement its own compiler and
  runtime, but those aspirations need to be matched to the actual
  strengths of the language. An efficient runtime is not a major goal
  of Morel (see first point) and therefore the runtime should be written
  in another language. But Morel, in the same family of languages as
  [Standard ML](https://en.wikipedia.org/wiki/ML_(programming_language)),
  [OCaml](https://en.wikipedia.org/wiki/OCaml) and
  [Rocq](https://en.wikipedia.org/wiki/Rocq), is a good language for
  building compilers and optimizers. Defining the abstract syntax tree
  (AST), algebra, and transformation rules in Morel will make
  extensibility possible (see second point).

# Convergence

These features put Morel in a good position to take advantage of an
exciting trend in databases and programming languages: the convergence
of compilation, query optimization, and query execution.

A few years ago
[functional programming language compilers](https://www.microsoft.com/en-us/research/wp-content/uploads/2002/07/inline.pdf)
started to look a bit like query optimizers, deciding (albeit without
statistics) whether to inline computations.

Around 2010, the programming language community discovered
[equality saturation](https://arxiv.org/abs/1012.1802), a program
optimization technique that fires transformation rules and maintains
equivalence sets of expressions that are semantically equivalent. They
were apparently unaware of
[Cascades](https://www.cse.iitb.ac.in/infolab/Data/Courses/CS632/Papers/Cascades-graefe.pdf),
a virtually identical algorithm that has powered most query optimizers
for three decades (including Calcite).

More recent work has
[drawn parallels](https://arxiv.org/abs/2304.04332) between equality
saturation and Datalog-style query execution, in particular
[worst-case optimal joins](https://en.wikipedia.org/wiki/Worst-case_optimal_join_algorithm).

Morel's support for
[Datalog-like recursion](https://github.com/hydromatic/morel/discussions/106)
allows it to take on these hard problems. Its functional programming
language roots, in particular its strong type system with
[algebraic data types](https://en.wikipedia.org/wiki/Algebraic_data_type)
and polymorphism, make it a delightful language in which to build
compilers and optimization rules.

# Conclusion

Although the Rust language is a compelling ecosystem, re-implementing
Morel in Rust would be a distraction from its core goals: enabling
queries and data-intensive programs on a variety of runtimes.  Morel's
promise will be realized when it, not the underlying language,
provides the tools for extensibility.

If you have comments, please reply on
[Bluesky @julianhyde.bsky.social](https://bsky.app/profile/julianhyde.bsky.social)
or Twitter:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>

This article
[has been updated](https://github.com/julianhyde/share/commits/main/blog/{{ page.path }}).
