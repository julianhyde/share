---
layout: post
title:  "Morel on Go"
date:   2026-08-01 12:00:00 -0800
author: Julian Hyde
image:  /assets/img/OldDesignShop_MushroomSpringMorel-240x240.jpg
tweet:  https://x.com/julianhyde/status/XXXXXXXXXXXXXXXXX
---

This week, we completed the port of Morel to a third platform, Go,
following Java and Rust. So can we now stop talking about platforms?

## One, two, three... Go?

You may be asking whether Morel needs another platform: Morel already
runs on Java and Rust, so why does it need Go?

The answer is that platforms don't matter.

More precisely, platforms shouldn't matter to you, the user. They
matter to us, writing the code (although thanks to Claude and its
friends, and techniques described later in this article, maintaining
code with a single specification across multiple languages isn't as
hard as it used to be).

We want you to think of Morel the way you think of regular
expressions: a small language that's available wherever your code is
running, whether that's in a Java Virtual Machine (JVM), in a server
written in Rust, or a command-line tool implemented in Go. You can
call Morel's WebAssembly build in the browser, and your agent can
invoke Morel in-process or via quick-starting tools.

The language has the same semantics on each platform, so you don't
need to change your Morel code, or even think about which platform it
is running on.

As a data language, Morel can address large data sets stored in SQL
databases and object stores, in addition to data in memory and the
local file system. Morel generates execution plans so that the
location of the data doesn't matter.

## Java to Rust

A year ago, I ruminated on whether to translate Morel to Rust. I
initially decided not to. I was concerned whether Rust was an easier
platform to develop on, the size of the community, what I would be
losing by abandoning Java.

Those concerns turned out to be moot, because we were adding Rust, but
not abandoning Java.

The translation process to Rust went fairly smoothly. It took a couple
of months &mdash; using Claude to plan and execute tasks but with a
good deal of oversight from me on the plan, design, and review &mdash;
to get the Rust port to 80% feature parity, and about another six
months to get to 100%.

The big surprise was that incremental translation turned out to be
almost free. The plan was to implement each new feature in Java, give
it a couple of weeks to stabilize, and then move it to Rust.

## Propagation

The process of moving features from one repo to another is called
propagation. It has the following steps:

1. Pick a commit in the morel-java repo that has not been propagated.
2. All changes to `.smli` test scripts must be propagated verbatim.
3. When the tests pass, commit the changes with a line that records
   the source commit and issue number.

For example, morel-rust commit [948fd0fa][morel-rust-948fd0fa]
contains the line "Propagates hydromatic/morel#387 commit 113d3ba0"
and adds the same 181 lines to `relational.smli` as morel-java commit
113d3ba0.

It is important that test changes are copied exactly. (A commit may
propagate additional test lines, but it may not make even cosmetic
changes to the core lines.) After each propagation step, the corpus of
`.smli` test files in the target repository gets closer to that in the
source repository, and so repositories tend to converge.

The `.smli` file format, whose suffix stands for "Standard ML,
Idempotent", makes this possible. The language is Standard ML/Morel,
and therefore each port can run the same tests unmodified.
"Idempotent" means that if a run of a script succeeds, it outputs an
exact copy of itself. Idempotent scripts are easier to propagate than
scripts in a "golden file" scheme, where each script is paired with a
reference output. The matcher [ignores][morel-9430af3f] changes in
whitespace and ordering, which is useful in accommodating
nondeterminism (e.g. the ordering of bags) and drift in
pretty-printing algorithms.

## And now Go

The Rust and Go versions are close to parity with the Java version. Go
does not yet have [highlighting][morel-413] and [command-line
editing][morel-414] in the shell, added last month to Java and Rust,
or the [file reader][morel-209].

Go and Rust are both missing the capability to convert queries to
relational algebra and push them down to SQL. Java gets this
capability from Apache Calcite, which is only available as a Java
library. Longer term, we plan to [implement a replacement for Calcite
in Morel][morel-359], adding `Plan` and `Sql` structures for
relational algebra, rewrite rules and SQL parsing.

## More Morel

There is a fourth language that could be used to implement library
functions: Morel itself. If a feature can be implemented in pure
Morel, this is a clear win, because we don't need to repeat the
implementations in native code (Java, Rust or Go).

Until now the standard library has been entirely native code, but
during the Go port, we started the process of [converting some simple
library functions][morel-either-bool] from native to Morel. While
Morel is generally a less efficient language, this can be a
performance win, because Morel's compiler can inline a function if it
knows its definition.

When the `Plan` structure is available, there will be an opportunity
for users to author query rewrite rules in Morel that mirror Calcite's
query optimization capabilities.

## Conclusion

Morel now runs on Go, in addition to the JVM and Rust. Go may not be
the last platform that Morel runs on, but it's the last platform we
will make a fuss about. From now on, you can think of Morel as your
data platform.

[morel-rust-948fd0fa]: https://github.com/hydromatic/morel-rust/commit/948fd0fa8aa9a0e15f162cbd11f54dde3ec4a39c
[morel-9430af3f]: https://github.com/hydromatic/morel/commit/9430af3f1a2428a2e2384547e1138c24b9fec282
[morel-209]: https://github.com/hydromatic/morel/issues/209
[morel-359]: https://github.com/hydromatic/morel/issues/359
[morel-413]: https://github.com/hydromatic/morel/issues/413
[morel-414]: https://github.com/hydromatic/morel/issues/414
[morel-either-bool]: https://github.com/hydromatic/morel/issues/TODO

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
