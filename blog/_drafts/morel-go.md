---
layout: post
title:  "Morel on Go, and why platforms shouldn't matter"
date:   2026-08-01 12:00:00 -0800
author: Julian Hyde
image:  /assets/img/OldDesignShop_MushroomSpringMorel-240x240.jpg
tweet:  https://x.com/julianhyde/status/XXXXXXXXXXXXXXXXX
---

![The morel-go shell, computing the prefixes of "Hello, world!"](/assets/img/morel-go-shell.png)

This week, we got [Morel][morel] running on Go (following Java and
Rust). Because a new platform is useless unless the language is the
same everywhere, we designed a porting process driven by a shared
corpus of tests.

## One, two, three... Go?

You may be asking why Morel needs a third platform.

The answer is that platforms don't matter.

More precisely, platforms shouldn't matter to you, the user. They
matter to us, who write the code (though thanks to Claude and its
friends, and the techniques described later, less than they used to).

We want you to think of Morel the way you think of regular
expressions: a small language that's available wherever your code is
running, whether that's in a Java Virtual Machine (JVM), in a server
written in Rust, or a command-line tool implemented in Go. You can
call Morel's WebAssembly build (compiled from the Rust port) in the
browser, and your agent can invoke Morel in-process, or as a
command-line tool that starts in milliseconds.

The language has the same semantics on each platform, so you don't
need to change your Morel code, or even think about which platform it
is running on.

Morel is a [data language][morel-query]. The goal is to access data
everywhere: large data sets stored in SQL databases and object stores,
in addition to data in memory and the local file system. On Java,
Morel generates execution plans so that the location of the data
doesn't matter; bringing that to Rust and Go is work in progress.

## Java to Rust

A year ago, I ruminated on
[whether to translate Morel to Rust][rewrite-morel]. I initially
decided not to. I had three concerns: whether Rust would really be an
easier platform to develop on, how large the community was, and what
I would lose by abandoning Java.

Those concerns turned out to be moot, because we were adding Rust, but
not abandoning Java.

Porting to Rust went fairly smoothly. It took a couple of months
&mdash; using Claude to plan and execute tasks but with a good deal of
oversight from me on the plan, design, and review &mdash; to get the
Rust port to 80% feature parity, and about another six months to
complete.

The big surprise was that, after that point, incremental translation
turned out to be almost free. After implementing a feature in Java, we
would give it a couple of weeks to stabilize, and then port it to Rust
by a process called *propagation*.

## Propagation

Propagation moves a feature from one repo to another, and has the
following steps:

1. Pick a commit in the [morel-java][morel] repo that has not been
   propagated.
2. Copy verbatim all changes that commit made to `.smli` test
   scripts.
3. Modify the code to make the tests pass.
4. Commit the changes with a line that records the source commit and
   issue number.

For example, [morel-rust][morel-rust] commit
[948fd0fa][morel-rust-948fd0fa] contains the line "Propagates
hydromatic/morel#387 commit 113d3ba0" and adds the same 181 lines to
`relational.smli` as morel-java commit [113d3ba0][morel-113d3ba0].

It is important that test changes are copied exactly. (A commit may
propagate additional test lines, but it may not make even cosmetic
changes to the core lines.) After each propagation step, the corpus of
`.smli` test files in the target repository gets closer to that in the
source repository, and so repositories tend to converge.

The `.smli` file format, whose suffix stands for "Standard ML,
Idempotent", makes this possible. The language is Standard ML/Morel,
and therefore each port can run the same tests unmodified.

"Idempotent" means that if a run of a script succeeds, it outputs an
exact copy of itself. Such scripts are easier to propagate than
scripts in a "golden file" scheme, where each script is paired with a
reference output: there is one file to copy, not two, and no way for
the pair to get out of step.

The matcher that compares a script's output against the script itself
[ignores][morel-9430af3f] changes in whitespace and ordering. That
tolerance accommodates nondeterminism (such as the ordering of bags)
and drift in pretty-printing algorithms.

While step 3 does not require Claude to read the Java source code, it
generally does. But the code also needs to conform to the idioms of
the target language &mdash; for example, Rust and Go use `switch`
statements where Java might use a visitor class. Claude is good at
following the component design in the target project, so a little
manual intervention, early in the porting process, to establish the
right patterns pays off later.

Creating [lint rules][morel-go-lint] &mdash; tests that read the
source code and check for invariants, such as that every built-in
Morel structure has a `.sig` file &mdash; is a particularly effective
way to create policies. If the test fails with a descriptive message,
Claude will enforce the policy without any further intervention.

## And now Go

Having just applied the same process to Go ([morel-go][morel-go]),
here is how the three implementations compare:

| Metric | Java | Rust | Go |
| --- | ---: | ---: | ---: |
| First commit | 2019-01-15 | 2025-08-14 | 2026-07-13 |
| Latest commit | 2026-07-31 | 2026-08-03 | 2026-07-31 |
| Elapsed | 7 yr 6 mo | 11 months | 18 days |
| Commits | 580 | 228 | 173 |
| Source lines (excl. tests) | 79,941 | 61,968 | 38,878 |
| `.smli` test files | 65 | 65 | 62 |
| `.smli` test lines | 28,626 | 25,876 | 25,347 |
| Complete (vs. Java `.smli`) | 100% | 90% | 89% |

`.smli` test lines counts only lines that the port evaluates; lines
disabled by `set("mode", "validate")`, described below, are excluded.

The Go port took 18 days, compared with 11 months for Rust. It reaches
much the same completeness, measured by lines of tests enabled, with
fewer lines of source code.

One improvement to the process was how we dealt with lines of a test
that were not yet working in the target language. In Rust, the `mode`
property has default value `evaluate`, but can be set to `parse`
(parse statements only) or `validate` (parse and type-check, but do
not execute). We disable sections of tests by setting `mode` to
`validate`. For example, the following lines from morel-rust's
[type.smli][rust-type-smli] are disabled because `typeof` is not
implemented yet:

<!-- morel skip
val m = 5;
> val m = 5 : int

set("mode", "validate");
> val it = () : unit
let
  val rec fact =
    fn n : typeof m =>
      if n = 0 then 1
      else n * fact (n - 1)
in
  fact 5
end;
> val it = 120 : int

set("mode", "evaluate");
> val it = () : unit
-->

<div class="code-block">
<div class="code-input"><span class="kr">val</span> <span class="nv">m</span> <span class="p">=</span> <span class="mi">5</span><span class="p">;</span></div>
<div class="code-output">val m = 5 : int</div>
<div class="code-input">
<span class="n">set</span><span class="p">(</span><span class="s2">"mode"</span><span class="p">,</span> <span class="s2">"validate"</span><span class="p">);</span></div>
<div class="code-output">val it = () : unit</div>
<div class="code-input"><span class="kr">let</span>
  <span class="kr">val</span> <span class="kr">rec</span> <span class="nv">fact</span> <span class="p">=</span>
    <span class="kr">fn</span> <span class="n">n</span> <span class="p">:</span> <span class="n">typeof</span> <span class="n">m</span> <span class="o">=&gt;</span>
      <span class="kr">if</span> <span class="n">n</span> <span class="p">=</span> <span class="mi">0</span> <span class="kr">then</span> <span class="mi">1</span>
      <span class="kr">else</span> <span class="n">n</span> <span class="o">*</span> <span class="n">fact</span> <span class="p">(</span><span class="n">n</span> <span class="o">-</span> <span class="mi">1</span><span class="p">)</span>
<span class="kr">in</span>
  <span class="n">fact</span> <span class="mi">5</span>
<span class="kr">end</span><span class="p">;</span></div>
<div class="code-output">val it = 120 : int</div>
<div class="code-input">
<span class="n">set</span><span class="p">(</span><span class="s2">"mode"</span><span class="p">,</span> <span class="s2">"evaluate"</span><span class="p">);</span></div>
<div class="code-output">val it = () : unit</div>
</div>

During the Go port, we would simply omit disabled lines:

<!-- morel skip
val m = 5;
> val m = 5 : int
-->

<div class="code-block">
<div class="code-input"><span class="kr">val</span> <span class="nv">m</span> <span class="p">=</span> <span class="mi">5</span><span class="p">;</span></div>
<div class="code-output">val m = 5 : int</div>
</div>

The Go approach was simpler and more effective. The Rust approach
achieved our goal of having the whole test corpus in Rust, but our
tooling had trouble keeping track of enabled and disabled
sections. It's hard to merge two versions of a test file when the two
have `set("mode", "validate");` lines at different points. In Go, the
test file is always a subsequence of the Java original, so merging
means inserting the missing lines in order &mdash; the
[shortest common supersequence][supersequence] of the two files.

The Rust and Go versions are close to parity with the Java version on
the language itself. The remaining gaps are in tooling and in data
access. Go does not yet have [highlighting][morel-413] and
[command-line editing][morel-414] in the shell, added last month to
Java and Rust, or the [file reader][morel-209].

Go and Rust are both missing the capability to convert queries to
relational algebra and push them down to SQL. Java gets this
capability from [Apache Calcite][calcite], which is only available
as a Java library. Longer term, we plan to
[implement a replacement for Calcite in Morel][morel-359], adding a
`Plan` structure for relational algebra and rewrite rules, and a
`Sql` structure for parsing and generating SQL.

## More Morel

There is a fourth language that could be used to implement library
functions: Morel itself. If a feature can be implemented in pure
Morel, this is a clear win, because we don't need to repeat the
implementations in native code (Java, Rust, or Go).

Until now the standard library has been entirely native code, but
during the Go port, we started the process of
[converting some simple library functions][morel-either-bool] from
native to Morel. While Morel is generally a less efficient language,
this can be a performance win, because Morel's compiler can inline a
function if it knows its definition.

When the `Plan` structure is available, there will be an opportunity
for users to author query rewrite rules in Morel that mirror Calcite's
query optimization capabilities.

## Conclusion

What is the source code of the Morel language? When I tell people I
am creating Morel, they generally assume that I am working on the Java
code of the reference implementation, or the Rust or Go of the ported
versions, but the [corpus of `.smli` tests][smli-corpus] is the real
source code. That corpus defines the syntax and semantics of Morel,
and completion of the Go port proves that it is sufficient to create
a good implementation.

Go may not be the last platform that Morel runs on, but I promise that
it's the last platform we will make a fuss about.

[morel-rust-948fd0fa]: https://github.com/hydromatic/morel-rust/commit/948fd0fa8aa9a0e15f162cbd11f54dde3ec4a39c
[morel-9430af3f]: https://github.com/hydromatic/morel/commit/9430af3f1a2428a2e2384547e1138c24b9fec282
[morel-113d3ba0]: https://github.com/hydromatic/morel/commit/113d3ba076567a71c30e93d6a5fed382abc510ac
[morel-209]: https://github.com/hydromatic/morel/issues/209
[morel-359]: https://github.com/hydromatic/morel/issues/359
[morel-413]: https://github.com/hydromatic/morel/issues/413
[morel-414]: https://github.com/hydromatic/morel/issues/414
[morel-either-bool]: https://github.com/hydromatic/morel-go/commit/1c72e903d51e98f1ca61f8ccd4a2769839387014
[rewrite-morel]: {% post_url 2025-05-11-rewrite-morel %}
[morel]: https://github.com/hydromatic/morel
[morel-rust]: https://github.com/hydromatic/morel-rust
[morel-go]: https://github.com/hydromatic/morel-go
[calcite]: https://calcite.apache.org
[morel-query]: https://github.com/hydromatic/morel/blob/main/docs/query.md
[morel-go-lint]: https://github.com/hydromatic/morel-go/blob/main/lint_test.go
[supersequence]: https://en.wikipedia.org/wiki/Shortest_common_supersequence_problem
[smli-corpus]: https://github.com/hydromatic/morel/tree/main/src/test/resources/script
[rust-type-smli]: https://github.com/hydromatic/morel-rust/blob/dd95da3b1e05ae36fa2175b130c31b0c45c311bf/tests/script/type.smli#L269-L286

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
