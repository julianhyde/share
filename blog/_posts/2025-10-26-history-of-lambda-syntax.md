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

The following table lists the year that various programming languages
introduced lambda syntax (not always the year in which the language
was born). If a language introduced an alternate syntax at a different
date, I have noted the year of introduction.

| Language | Year     | Syntax                  | Alternate(s)       |
|----------|----------|-------------------------|--------------------|
| Lambda calculus | 1930s[^32] | `λx.x + 1`     |                    |
| Lisp     | 1960[^1] | `(lambda (x) (+ x 1))`  |                    |
| ML       | 1973[^2] | `fn x => x + 1`         |                    |
| Smalltalk | 1981[^35] | `[ :x | x + 1 ]`      |                    |
| Erlang   | 1987[^3] | `fun(X) -> X + 1 end`   |                    |
| Haskell  | 1990[^4] | `\x -> x + 1`           | `(+ 1)` (1999[^5]) |
| Python   | 1991[^6] | `lambda x: x + 1`       |                    |
| Lua      | 1993[^33] | `function (x) return x + 1 end` |           |
| Perl     | 1994[^7] | `sub { $_[0] + 1 }`     | `sub { my $x = shift; $x + 1 }` |
| JavaScript | 1995[^8] | `function(x) { return x + 1; }` | `x => x + 1` (2015 [^9]) |
| Ruby     | 1995[^10] | `Proc.new { | x | x + 1 }`<br/>`proc { | x | x + 1 }` | `lambda { |x| x + 1 }` (2003 [^11])<br/>`->(x) { x + 1 }` (2007 [^12]) |
| OCaml    | 1996[^13] | `fun x -> x + 1`       | `(+) 1` (1996 [^14]) |
| APL      | 1996[^20] | <code>{&omega;+1}</code> | `+∘1` (1978 [^21]) |
| Groovy   | 2003[^16] | `{ x -> x + 1 }`       |                    |
| Scala    | 2003[^17] | `x => x + 1`           | `_ + 1` (2007 [^18]) |
| MATLAB   | 2004[^19] | `@(x) x + 1`           |                    |
| C#       | 2007[^15] | `x => x + 1`           |                    |
| Clojure  | 2007[^22] | `(fn [x] (+ x 1))`     | `#(+ % 1)`<br/>`(partial + 1)` |
| Go       | 2009[^23] | `func(x int) int { return x + 1 }` |        |
| Delphi    | 2009[^34] | `f := function(x: Integer): Integer begin Result := x + 1; end;` | |
| Rust     | 2010[^24] | `|x| x + 1`            |                    |
| Dart     | 2011[^25] | `(x) => x + 1`         |                    |
| Elixir   | 2011[^26] | `fn x -> x + 1 end`    | `&(&1 + 1)`        |
| Kotlin   | 2011[^27] | `{ x -> x + 1 }`       |                    |
| C++      | 2011[^28] | `[](int x) { return x + 1; }` |             |
| Julia    | 2012[^29] | `x -> x + 1`           |                    |
| Swift    | 2014[^30] | `{ x in x + 1 }`       | `{$0 + 1}`         |
| Java     | 2014[^31] | `x -> x + 1`           |                    |

(Please let me know if there are mistakes in syntax or year of
introduction. Claude was my research assistant. I omitted languages in
the same family with the same syntax, e.g. Lisp-Scheme-Racket,
OCaml-F#. Did I miss any early, major languages?)

Here is the original tweet:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>

This article
[has been updated](https://github.com/julianhyde/share/commits/main/blog/{{ page.path }}).

### Footnotes

[^32]: The lambda calculus was invented in the 1930s by Alonzo Church.
       The original notation used a Greek letter lambda (λ) to denote
       anonymous functions. It is a mathematical formalism rather than
       a programming language.

[^1]: Lisp was invented in 1958, but the lambda syntax appeared in the
      1960 paper "Recursive Functions of Symbolic Expressions and
      Their Computation by Machine, Part I" by John McCarthy.

[^2]: ML was invented in 1973 by Robin Milner et al. The syntax
      appeared in the 1978 paper "The Definition of Standard ML" by
      Milner, Tofte, and Harper.

[^3]: Erlang was created in 1987 by Joe Armstrong et al. The syntax
      appeared in the 1993 book "Erlang Programming" by Armstrong.

[^4]: Haskell was first defined in 1990 by a committee. The syntax
      appeared in the 1990 paper "Haskell: A Non-strict, Purely
      Functional Language" by Simon Peyton Jones et al.

[^5]: The operator section syntax `(+ 1)` was introduced in the
      Haskell 98 Report (1999).

[^6]: Python introduced the `lambda` syntax in version 1.0, released
      in January 1994. The syntax was present in the 1991 "Python
      Tutorial" by Guido van Rossum.

[^7]: Perl introduced anonymous subroutines in version 5.0, released
      in 1994. The syntax was documented in the "Programming Perl"
      book by Larry Wall et al. The use of `my $x = shift`; within
      such a block is a standard way to access arguments passed to the
      subroutine.

[^8]: JavaScript was created in 1995 by Brendan Eich. The `function`
      syntax appeared in the original specification "JavaScript
      Language Specification" by Netscape.

[^9]: The arrow function syntax `x => x + 1` was introduced in
      ECMAScript 6 (2015). Prior to that, JavaScript did not have a
      concise lambda syntax.

[^10]: Ruby was created in 1995 by Yukihiro Matsumoto. The initial
       release, Ruby 0.95 contained the `Proc` class and block
       syntax. The `Kernel#proc` method was equivalent to `Proc.new`.

[^11]: Ruby introduced `lambda` in Ruby 1.8 (2003) as a way to create
       lambda functions with stricter argument checking.

[^12]: The stabby lambda syntax `->` was introduced in Ruby 1.9 (2007)
       as a more concise way to define lambdas. `Kernel#proc` was
       changed to be equivalent to `Proc.new`, which has slightly
       different behavior than a lambda.

[^13]: OCaml was created in 1996 by Xavier Leroy et al. The syntax
       appeared in the 1996 paper "The Objective Caml System" by
       Leroy.

[^14]: OCaml supports partial application of functions, so `(+) 1` is
       valid syntax for a function that adds 1.

[^15]: While C# had `delegate` in version 2.0 (2005), lambda
       expressions did not arrive until version 3.0 (2007). The syntax
       appeared in the "C# Language Specification" by Anders Hejlsberg
       et al.

[^16]: Groovy was created in 2003 by James Strachan. The closure
       syntax appeared in the original Groovy documentation.

[^17]: Scala was created in 2003 by Martin Odersky. The syntax
       appeared in the 2004 paper "The Scala Language Specification"
       by Odersky et al.

[^18]: Scala "placeholder syntax" was introduced around 2007, and
       appears in the 2008 "Programming in Scala" book by Odersky,
       Spoon, and Venners.

[^19]: MATLAB introduced function handles in release R12 (MATLAB 6.0),
       which was released in November 2000. However, in this version,
       calling them still required the use of the `feval`
       function. Anonymous and nested functions, which expanded the
       capabilities related to function handles, were introduced later
       in release R14 (MATLAB 7.0), released in June 2004.

[^20]: [John Scholes](https://en.wikipedia.org/wiki/John_M._Scholes)
       invented direct functions or dfns (pronounced "dee funs") in
       1996.

[^21]: The tacit programming style (also known as point-free style)
       was introduced by Kenneth E. Iverson in the 1978 book "APL: An
       Interactive Approach" co-authored with Philip S. Abrams.  See
       also [Bind](https://aplwiki.com/wiki/Bind).

[^22]: Clojure was created in 2007 by Rich Hickey. The `fn` syntax,
       function literal syntax `#(+ % 1)`, and `partial` all
       appeared in the original Clojure documentation.

[^23]: Go was created in 2009 by Robert Griesemer, Rob Pike, and Ken
       Thompson.  The `func` syntax appeared in the original Go
       specification.

[^24]: Rust was created in 2010 by Graydon Hoare. The closure syntax
       appeared in the original Rust documentation.

[^25]: Dart was created in 2011 by Lars Bak and Kasper Lund. The arrow
       syntax appeared in the original Dart language specification.

[^26]: Elixir was created in 2011 by José Valim. The `fn` syntax and
       capture operator `&` appeared in the original Elixir
       documentation.

[^27]: Kotlin was created in 2011 by JetBrains. The lambda syntax
       appeared in the original Kotlin documentation.

[^28]: C++ introduced lambda expressions in C++11 (2011). The syntax
       appeared in the "C++11 Standard" by the ISO/IEC JTC1/SC.

[^29]: Julia was created in 2012 by Jeff Bezanson, Stefan Karpinski,
       Viral B. Shah, and Alan Edelman. The arrow syntax appeared in
       the original Julia documentation.

[^30]: Swift was created in 2014 by Apple Inc. The closure syntax, and
       shorthand argument names like `$0`, have been part of Swift
       since version 1.0.

[^31]: Java 8 introduced lambda expressions in 2014. The syntax
       appeared in the "Java Language Specification, Java SE 8
       Edition" by James Gosling et al.

[^33]: Lua was created in 1993 by Roberto Ierusalimschy, Luiz Henrique
       de Figueiredo, and Waldemar Celes. The function syntax appeared
       in the original Lua documentation.

[^34]: Delphi introduced anonymous methods in Delphi 2009. The syntax
       appeared in the "Delphi Language Guide" by Embarcadero. Anonymous
       methods must be used immediately (assigned to a variable, passed
       as a parameter, or applied to arguments).

[^35]: Smalltalk was created in the early 1970s at Xerox PARC by Alan
       Kay, Dan Ingalls, Adele Goldberg, and others. Smalltalk-76
       added block literals with no arguments. Smalltalk-80 (1981)
       allowed code blocks to have arguments. See
       "[Smalltalk-80: The Language and its implementation](http://stephane.ducasse.free.fr/FreeBooks/BlueBook/Bluebook.pdf)"
       by Adele Goldberg and David Robson, page 35.
