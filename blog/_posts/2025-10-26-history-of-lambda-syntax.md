---
layout: post
title:  "History of lambda syntax"
date:   2025-10-26 12:00:00 -0800
author: Julian Hyde
tweet:  https://x.com/julianhyde/status/1950681730568143094
---

Lambda syntax varies widely across languages; more widely, I think, than
other language features. I wish it weren't so. It's difficult to see the
elegance in a new language if the syntax is unfamiliar.

| Language | Year | Syntax | Alternate(s) |
|----------|------|--------|--------------|
| Lisp | 1960 | `(lambda (x) (+ x 1))` | |
| ML | 1973 | `fn x => x + 1` | |
| Erlang | 1987 | `fun(X) -> X + 1 end` | |
| Haskell | 1990 | `\x -> x + 1` | `(+ 1)` |
| Python | 1993 | `lambda x: x + 1` | |
| Perl | 1994 | `sub { $_[0] + 1 }` | `sub { my $x = shift; $x + 1 }` |
| JavaScript | 1995 | `function(x) { return x + 1; }` | `x => x + 1` |
| Ruby | 1995 | `proc { |x| x + 1 }` | `lambda { |x| x + 1 }`<br/>`->(x) { x + 1 }` |
| OCaml | 1996 | `fun x -> x + 1` | `(+) 1` |
| C# | 2000 | `x => x + 1` | |
| Groovy | 2003 | `{ x -> x + 1 }` | |
| Scala | 2003 | `x => x + 1` | `_ + 1` |
| MATLAB | 2004 | `@(x) x + 1` | |
| Clojure | 2007 | `(fn [x] (+ x 1))` | `#(+ % 1)`<br/>`(partial + 1)` |
| Go | 2009 | `func(x int) int { return x + 1 }` | |
| Rust | 2010 | `|x| x + 1` | |
| Dart | 2011 | `(x) => x + 1` | |
| Elixir | 2011 | `fn x -> x + 1 end` | `&(&1 + 1)` |
| Kotlin | 2011 | `{ x -> x + 1 }` | |
| C++ (11+) | 2011 | `[](int x) { return x + 1; }` | |
| Julia | 2012 | `x -> x + 1` | |
| Swift | 2014 | `{ x in x + 1 }` | `{$0 + 1}` |
| Java (8+) | 2014 | `x -> x + 1` | |

(Please let me know if there are mistakes in syntax or year of
introduction. Claude was my research assistant. I omitted languages in
the same family with the same syntax, e.g. Lisp-Scheme-Racket,
OCaml-F#. Did I miss any early, major languages?)

Here is the original tweet:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>

<!--
This article
[has been updated](https://github.com/julianhyde/share/commits/main/blog/{{ page.path }}).
-->
