---
layout: post
title:  "Morel release 0.7.0"
date:   2025-06-08 12:00:00 -0800
author: Julian Hyde
image:  /assets/img/OldDesignShop_MushroomSpringMorel-240x240.jpg
tweet:  https://x.com/julianhyde/status/1931931352729079968
---

I am pleased to announce Morel
[release 0.7.0](https://github.com/hydromatic/morel/blob/main/HISTORY.md#070--2025-06-07),
just one month after
[release 0.6.0](https://github.com/hydromatic/morel/blob/main/HISTORY.md#060--2025-05-02).

This release has actually been under development for a long time.
[Ordered and unordered collections and queries](#1-ordered-and-unordered-collections-and-queries),
which are the centerpiece of this release, required major changes to
the type inference algorithm, not to mention a new
[data type](https://github.com/hydromatic/morel/issues/235) (`bag`),
[query step](https://github.com/hydromatic/morel/issues/277) (`unorder`),
and
[expression](https://github.com/hydromatic/morel/issues/276) (`ordinal`).
The type inference changes have been under development for six months
(during which time there were two other Morel releases), and were so
extensive that we got
[function overloading](#2-function-overloading) practically for free.

There are other changes to query syntax:
[sorting on expressions](#3-sorting-on-expressions),
[atomic `yield` steps](#4-atomic-yield-steps), and
[set operators in pipelines](#5-set-operators-in-pipelines).

Morel aims to be a solid implementation of Standard ML and good
general-purpose programming language, in addition to being a
revolutionary query language, which means gradually completing our
implementation of Standard ML's
[Basis Library](https://smlfamily.github.io/Basis/). This release we
have completed the
[`String` and `Char` structures](#6-string-and-char-structures).

Let's explore the key features. For complete details, see the
[official release notes](https://github.com/hydromatic/morel/blob/main/HISTORY.md#070--2025-06-07).

## 1. Ordered and unordered collections and queries

The biggest change in 0.7.0 is the introduction of
[ordered and unordered collections and queries](https://github.com/hydromatic/morel/issues/273).
Previously, every query was over a `list` type, whose elements were
ordered and duplicates were allowed.

But saying that every collection and query is over a `list` type
is a white lie. Consider this query:

<!-- morel skip
from e in scott.emps
  where e.sal > 1000.0
  yield e.ename;
-->

<div class="code-block">
<div class="code-input"><span class="kr">from</span> <span class="nv">e</span> <span class="kr">in</span> <span class="nn">scott</span><span class="p">.</span><span class="n">emps</span>
  <span class="kr">where</span> <span class="nn">e</span><span class="p">.</span><span class="n">sal</span> <span class="o">&gt;</span> <span class="mi">1000</span><span class="p">.</span><span class="mi">0</span>
  <span class="kr">yield</span> <span class="nn">e</span><span class="p">.</span><span class="n">ename</span><span class="p">;</span></div>
</div>


The collection `scott.emps` maps to the `EMP` table in the `scott`
database, and Morel's goal is to push as much of the processing as
possible to where the data resides. In this case, Morel can generate
the SQL query

```sql
SELECT ENAME
FROM SCOTT.EMP
WHERE SAL > 1000.0;
```

SQL makes no guarantees about the order of results. If you execute
the query twice, a DBMS is free to return the results in a different
order each time. So Morel is being dishonest if it says that result
is a `list`.

Could we redefine `list` so that its iteration order is undefined?
Yes, but then we would be short-changing queries such as

<!-- morel
from i in ["a", "b"],
    j in [1, 2, 3]
  yield (i, j);
> val it = [("a",1),("a",2),("a",3),("b",1),("b",2),("b",3)]
>   : (string * int) list
-->

<div class="code-block">
<div class="code-input"><span class="kr">from</span> <span class="nv">i</span> <span class="kr">in</span> <span class="p">[</span><span class="s2">"a"</span><span class="p">,</span> <span class="s2">"b"</span><span class="p">],</span>
    <span class="nv">j</span> <span class="kr">in</span> <span class="p">[</span><span class="mi">1</span><span class="p">,</span> <span class="mi">2</span><span class="p">,</span> <span class="mi">3</span><span class="p">]</span>
  <span class="kr">yield</span> <span class="p">(</span><span class="n">i</span><span class="p">,</span> <span class="n">j</span><span class="p">);</span></div>
<div class="code-output">val it = [("a",1),("a",2),("a",3),("b",1),("b",2),("b",3)]
  : (string * int) list</div>
</div>

which do have a defined order.

The fact is -- even though the relational model tells us it ain't so
-- some data sets are ordered, and some are unordered. Adding distinct
`bag` and `list` types, relational operators that can work on both,
and relational operators to convert between them, was the way to go.

The features that we implemented are described in the article
"[Ordered and unordered data](http://blog.hydromatic.net/2025/06/06/ordered-unordered.html)".

## 2. Function overloading

In Standard ML, and in Morel until recently, a name could only have
one binding.  Functions are values, and therefore inhabit the same
namespace as regular values.  If I declare `x` to be an `int` value

<!-- morel skip
val x = 42;
-->

<div class="code-block">
<div class="code-input"><span class="kr">val</span> <span class="nv">x</span> <span class="p">=</span> <span class="mi">42</span><span class="p">;</span></div>
</div>


and then later try to declare `x` to be a function

<!-- morel skip
val x = fn y => y + 1;
-->

<div class="code-block">
<div class="code-input"><span class="kr">val</span> <span class="nv">x</span> <span class="p">=</span> <span class="kr">fn</span> <span class="n">y</span> <span class="o">=&gt;</span> <span class="n">y</span> <span class="o">+</span> <span class="mi">1</span><span class="p">;</span></div>
</div>


then the previous declaration of `x` is no longer accessible.

<!-- morel fail
val z = x - 2;
> stdIn:1.9 Error: unbound variable or constructor: x
>   raised at: stdIn:1.9
-->

<div class="code-block">
<div class="code-input"><span class="kr">val</span> <span class="nv">z</span> <span class="p">=</span> <span class="n">x</span> <span class="o">-</span> <span class="mi">2</span><span class="p">;</span></div>
<div class="code-error">stdIn:1.9 Error: unbound variable or constructor: x
  raised at: stdIn:1.9</div>
</div>


To create
[overloaded functions](https://github.com/hydromatic/morel/issues/237),
we need declare that an identifier is special; we do this using the
new `over` keyword:

<!-- morel
over f;
> over f
-->

<div class="code-block">
<div class="code-input"><span class="kr">over</span> <span class="n">f</span><span class="p">;</span></div>
<div class="code-output">over f</div>
</div>


Now we can define several instances of `f`:

<!-- morel
val inst f = fn (x : int, y : int) => x + y;
> val f = fn : int * int -> int
val inst f = fn list => length list;
> val f = fn : 'a list -> int
val inst f = fn SOME x => x ^ "!" | NONE => ":(";
> val f = fn : string option -> string
-->

<div class="code-block">
<div class="code-input"><span class="kr">val</span> <span class="kr">inst</span> <span class="nv">f</span> <span class="p">=</span> <span class="kr">fn</span> <span class="p">(</span><span class="n">x</span> <span class="p">:</span> <span class="n">int</span><span class="p">,</span> <span class="n">y</span> <span class="p">:</span> <span class="n">int</span><span class="p">)</span> <span class="o">=&gt;</span> <span class="n">x</span> <span class="o">+</span> <span class="n">y</span><span class="p">;</span></div>
<div class="code-output">val f = fn : int * int -&gt; int</div>
<div class="code-input"><span class="kr">val</span> <span class="kr">inst</span> <span class="nv">f</span> <span class="p">=</span> <span class="kr">fn</span> <span class="n">list</span> <span class="o">=&gt;</span> <span class="n">length</span> <span class="n">list</span><span class="p">;</span></div>
<div class="code-output">val f = fn : 'a list -&gt; int</div>
<div class="code-input"><span class="kr">val</span> <span class="kr">inst</span> <span class="nv">f</span> <span class="p">=</span> <span class="kr">fn</span> <span class="n">SOME</span> <span class="n">x</span> <span class="o">=&gt;</span> <span class="n">x</span> ^ <span class="s2">"!"</span> <span class="p">|</span> <span class="n">NONE</span> <span class="o">=&gt;</span> <span class="s2">":("</span><span class="p">;</span></div>
<div class="code-output">val f = fn : string option -&gt; string</div>
</div>


All must be functions, because the overloads are resolved based on
the type of the first argument.

Calls to `f` will be resolved based on the types of the arguments:

<!-- morel
(* Call the "int * int -> int" overload. *)
f (7, 8);
> val it = 15 : int
(* Call the "'a list -> int" overload. *)
f ["a", "b", "c"];
> val it = 3 : int
f [1, 2, 3, 4];
> val it = 4 : int
f [];
> val it = 0 : int
(* Call the "string option -> string" overload. *)
f (SOME "happy");
> val it = "happy!" : string
f NONE;
> val it = ":(" : string
-->

<div class="code-block">
<div class="code-input"><span class="c">(*</span><span class="cm"> Call the "int * int -&gt; int" overload. *)</span>
<span class="n">f</span> <span class="p">(</span><span class="mi">7</span><span class="p">,</span> <span class="mi">8</span><span class="p">);</span></div>
<div class="code-output">val it = 15 : int</div>
<div class="code-input"><span class="c">(*</span><span class="cm"> Call the "'a list -&gt; int" overload. *)</span>
<span class="n">f</span> <span class="p">[</span><span class="s2">"a"</span><span class="p">,</span> <span class="s2">"b"</span><span class="p">,</span> <span class="s2">"c"</span><span class="p">];</span></div>
<div class="code-output">val it = 3 : int</div>
<div class="code-input"><span class="n">f</span> <span class="p">[</span><span class="mi">1</span><span class="p">,</span> <span class="mi">2</span><span class="p">,</span> <span class="mi">3</span><span class="p">,</span> <span class="mi">4</span><span class="p">];</span></div>
<div class="code-output">val it = 4 : int</div>
<div class="code-input"><span class="n">f</span> <span class="p">[];</span></div>
<div class="code-output">val it = 0 : int</div>
<div class="code-input"><span class="c">(*</span><span class="cm"> Call the "string option -&gt; string" overload. *)</span>
<span class="n">f</span> <span class="p">(</span><span class="n">SOME</span> <span class="s2">"happy"</span><span class="p">);</span></div>
<div class="code-output">val it = "happy!" : string</div>
<div class="code-input"><span class="n">f</span> <span class="n">NONE</span><span class="p">;</span></div>
<div class="code-output">val it = ":(" : string</div>
</div>

<!-- morel fail
(* No overloads match "int option" or "(int, int, int)" arguments. *)
f (SOME 42);
> 0.0-0.0 Error: Cannot deduce type: no valid overloads
>   raised at: 0.0-0.0
f (1, 2, 3);
> 0.0-0.0 Error: Cannot deduce type: no valid overloads
>   raised at: 0.0-0.0
-->

<div class="code-block">
<div class="code-input"><span class="c">(*</span><span class="cm"> No overloads match "int option" or "(int, int, int)" arguments. *)</span>
<span class="n">f</span> <span class="p">(</span><span class="n">SOME</span> <span class="mi">42</span><span class="p">);</span></div>
<div class="code-error">0.0-0.0 Error: Cannot deduce type: no valid overloads
  raised at: 0.0-0.0</div>
<div class="code-input"><span class="n">f</span> <span class="p">(</span><span class="mi">1</span><span class="p">,</span> <span class="mi">2</span><span class="p">,</span> <span class="mi">3</span><span class="p">);</span></div>
<div class="code-error">0.0-0.0 Error: Cannot deduce type: no valid overloads
  raised at: 0.0-0.0</div>
</div>

## 3. Sorting on expressions

There are only a few places in Morel syntax where you do not use an
expression, and the `order` step used to be one of them.  Previously,
`order` was followed by a list of "order items", each an expression
optionally followed by `desc`. The items were separated by commas, and
the list could not be empty.

The commas were a problem. In the expression

<!-- morel skip
foo (from i in [1, 2, 3] order i desc, j);
-->

<div class="code-block">
<div class="code-input"><span class="n">foo</span> <span class="p">(</span><span class="kr">from</span> <span class="nv">i</span> <span class="kr">in</span> <span class="p">[</span><span class="mi">1</span><span class="p">,</span> <span class="mi">2</span><span class="p">,</span> <span class="mi">3</span><span class="p">]</span> <span class="kr">order</span> <span class="n">i</span> <span class="kr">desc</span><span class="p">,</span> <span class="n">j</span><span class="p">);</span></div>
</div>

it is not clear whether `j` is a second argument for the call to the
function `foo` or the second item in the `order` clause.

Another problem was the fact that the `order` clause could not be
empty. The
[ordered/unordered collections](#1-ordered-and-unordered-collections-and-queries)
feature introduced an `unorder` step to convert a `list` to a `bag`,
and we need the opposite of that, a trivial sort whose
key has the same value for every element.

The answer was to
[make the argument to `order` an expression](https://github.com/hydromatic/morel/issues/244).
A composite sort specification is now a tuple, still separated by
commas, but now enclosed in parentheses.  If a sort key is descending,
you now wrap it in the `Descending` data type by preceding it with the
`DESC`.  Thus:

<!-- morel skip
(* Old syntax *)
from e in scott.emps
  order e.job, e.sal desc;

(* New syntax *)
from e in scott.emps
  order (e.job, DESC e.sal);
-->

<div class="code-block">
<div class="code-input"><span class="c">(*</span><span class="cm"> Old syntax *)</span>
<span class="kr">from</span> <span class="nv">e</span> <span class="kr">in</span> <span class="nn">scott</span><span class="p">.</span><span class="n">emps</span>
  <span class="kr">order</span> <span class="nn">e</span><span class="p">.</span><span class="n">job</span><span class="p">,</span> <span class="nn">e</span><span class="p">.</span><span class="n">sal</span> <span class="kr">desc</span><span class="p">;</span>

<span class="c">(*</span><span class="cm"> New syntax *)</span>
<span class="kr">from</span> <span class="nv">e</span> <span class="kr">in</span> <span class="nn">scott</span><span class="p">.</span><span class="n">emps</span>
  <span class="kr">order</span> <span class="p">(</span><span class="nn">e</span><span class="p">.</span><span class="n">job</span><span class="p">,</span> <span class="n">DESC</span> <span class="nn">e</span><span class="p">.</span><span class="n">sal</span><span class="p">);</span></div>
</div>

You can now sort by any data type, including tuples, records,
sum-types such as `Option` and `Descending`, lists, bags, and any
combination thereof.

To achieve the trivial sort, you can sort by any constant value, such
as the integer `0` or the `Option` constructor `NONE`, but
conventionally you would sort by the empty tuple `()`:

<!-- morel
from e in scott.emps
  yield e.ename
  order ();
> val it =
>   ["SMITH","ALLEN","WARD","JONES","MARTIN","BLAKE","CLARK","SCOTT","KING",
>    "TURNER","ADAMS","JAMES",...] : string list
-->

<div class="code-block">
<div class="code-input"><span class="kr">from</span> <span class="nv">e</span> <span class="kr">in</span> <span class="nn">scott</span><span class="p">.</span><span class="n">emps</span>
  <span class="kr">yield</span> <span class="nn">e</span><span class="p">.</span><span class="n">ename</span>
  <span class="kr">order</span> <span class="p">();</span></div>
<div class="code-output">val it =
  ["SMITH","ALLEN","WARD","JONES","MARTIN","BLAKE","CLARK","SCOTT","KING",
   "TURNER","ADAMS","JAMES",...] : string list</div>
</div>


The key thing is that the result is a `list`.  The elements are in
arbitrary order (because any order is consistent with the empty sort
key) but in converting the collection to a `list` the arbitrary order
has become frozen and repeatable.

## 4. Atomic yield steps

At any step in a Morel query, there are generally several named fields
you can use to reference parts of the current row.  For example, the
`where` step in the following query refers to both fields, `i` and
`j`.

<!-- morel silent
Sys.set ("output", "tabular");
> val it = () : unit
-->
<!-- morel
from i in [1, 2, 3],
    j in [4, 5, 6]
  where i + j > 7;
> i j
> - -
> 2 6
> 3 5
> 3 6
>
> val it : {i:int, j:int} list
-->

<div class="code-block">
<div class="code-input"><span class="kr">from</span> <span class="nv">i</span> <span class="kr">in</span> <span class="p">[</span><span class="mi">1</span><span class="p">,</span> <span class="mi">2</span><span class="p">,</span> <span class="mi">3</span><span class="p">],</span>
    <span class="nv">j</span> <span class="kr">in</span> <span class="p">[</span><span class="mi">4</span><span class="p">,</span> <span class="mi">5</span><span class="p">,</span> <span class="mi">6</span><span class="p">]</span>
  <span class="kr">where</span> <span class="n">i</span> <span class="o">+</span> <span class="n">j</span> <span class="o">&gt;</span> <span class="mi">7</span><span class="p">;</span></div>
<div class="code-output">i j
- -
2 6
3 5
3 6

val it : {i:int, j:int} list</div>
</div>


But there is one circumstance where a step does not produce any named
fields: a `yield` whose expression is not a record, what we call an
"atomic yield". Here is an example:

<!-- morel skip
from i in [1, 2, 3],
    j in [4, 5, 6]
  yield i + j;
-->

<div class="code-block">
<div class="code-input"><span class="kr">from</span> <span class="nv">i</span> <span class="kr">in</span> <span class="p">[</span><span class="mi">1</span><span class="p">,</span> <span class="mi">2</span><span class="p">,</span> <span class="mi">3</span><span class="p">],</span>
    <span class="nv">j</span> <span class="kr">in</span> <span class="p">[</span><span class="mi">4</span><span class="p">,</span> <span class="mi">5</span><span class="p">,</span> <span class="mi">6</span><span class="p">]</span>
  <span class="kr">yield</span> <span class="n">i</span> <span class="o">+</span> <span class="n">j</span><span class="p">;</span></div>
</div>


That query is valid, but suppose we wished to sort or filter the
results.  If we added an `order` or `where` step it would have no way
to refer to the current row. We allowed atomic yields because we
needed queries with non-record elements, but we made a rule that the
atomic yield had to be the last step.

That restriction was becoming more of a burden, and the final straw
was ordered/unordered queries, which often end in `order` or
`unorder`. So we decided to fix the problem.

We
[added a new expression, `current`](https://github.com/hydromatic/morel/issues/265),
that refers to the current element. (It is only available in query
steps, but you can use it inside a sub-expression or sub-query.)  If
the value is atomic, `current` is that value; if there are named
fields, `current` is a record consisting of those fields. (In the
previous example, `current` would be equivalent to `{i, j}`.)

If a `yield` is atomic but the expression has a clear name, as in
`yield i` or `yield e.deptno`, you can also use that name.  (The
expression is still considered atomic, and the result of the query
will be a collection of that type, not a collection of records.)

Here are some examples of `current` in action.

<!-- morel
from i in [1, 2, 3],
    j in [4, 5, 6]
  yield i + j
  order DESC current;
> val it = [9,8,8,7,7,7,6,6,5] : int list
-->

<div class="code-block">
<div class="code-input"><span class="kr">from</span> <span class="nv">i</span> <span class="kr">in</span> <span class="p">[</span><span class="mi">1</span><span class="p">,</span> <span class="mi">2</span><span class="p">,</span> <span class="mi">3</span><span class="p">],</span>
    <span class="nv">j</span> <span class="kr">in</span> <span class="p">[</span><span class="mi">4</span><span class="p">,</span> <span class="mi">5</span><span class="p">,</span> <span class="mi">6</span><span class="p">]</span>
  <span class="kr">yield</span> <span class="n">i</span> <span class="o">+</span> <span class="n">j</span>
  <span class="kr">order</span> <span class="n">DESC</span> <span class="kr">current</span><span class="p">;</span></div>
<div class="code-output">val it = [9,8,8,7,7,7,6,6,5] : int list</div>
</div>


<!-- morel
from maker in ["ford", "ferrari"],
    color in ["red", "green"]
  order current.color;
> color maker
> ----- -------
> green ford
> green ferrari
> red   ford
> red   ferrari
>
> val it : {color:string, maker:string} list
-->

<div class="code-block">
<div class="code-input"><span class="kr">from</span> <span class="nv">maker</span> <span class="kr">in</span> <span class="p">[</span><span class="s2">"ford"</span><span class="p">,</span> <span class="s2">"ferrari"</span><span class="p">],</span>
    <span class="nv">color</span> <span class="kr">in</span> <span class="p">[</span><span class="s2">"red"</span><span class="p">,</span> <span class="s2">"green"</span><span class="p">]</span>
  <span class="kr">order</span> <span class="kr">current</span><span class="p">.</span><span class="n">color</span><span class="p">;</span></div>
<div class="code-output">color maker
----- -------
green ford
green ferrari
red   ford
red   ferrari

val it : {color:string, maker:string} list</div>
</div>


<!-- morel
from i in [1, 2, 3, 4]
  yield 4 * (i mod 2) + (i div 2)
  order current;
> val it = [1,2,4,5] : int list
-->

<div class="code-block">
<div class="code-input"><span class="kr">from</span> <span class="nv">i</span> <span class="kr">in</span> <span class="p">[</span><span class="mi">1</span><span class="p">,</span> <span class="mi">2</span><span class="p">,</span> <span class="mi">3</span><span class="p">,</span> <span class="mi">4</span><span class="p">]</span>
  <span class="kr">yield</span> <span class="mi">4</span> <span class="o">*</span> <span class="p">(</span><span class="n">i</span> <span class="kr">mod</span> <span class="mi">2</span><span class="p">)</span> <span class="o">+</span> <span class="p">(</span><span class="n">i</span> <span class="kr">div</span> <span class="mi">2</span><span class="p">)</span>
  <span class="kr">order</span> <span class="kr">current</span><span class="p">;</span></div>
<div class="code-output">val it = [1,2,4,5] : int list</div>
</div>


<!-- morel
from e in scott.emps
  yield e.deptno
  distinct
  order current;
> val it = [10,20,30] : int list
-->

<div class="code-block">
<div class="code-input"><span class="kr">from</span> <span class="nv">e</span> <span class="kr">in</span> <span class="nn">scott</span><span class="p">.</span><span class="n">emps</span>
  <span class="kr">yield</span> <span class="nn">e</span><span class="p">.</span><span class="n">deptno</span>
  <span class="kr">distinct</span>
  <span class="kr">order</span> <span class="kr">current</span><span class="p">;</span></div>
<div class="code-output">val it = [10,20,30] : int list</div>
</div>


<!-- morel
from e in scott.emps
  yield e.deptno
  distinct
  order deptno;
> val it = [10,20,30] : int list
-->

<div class="code-block">
<div class="code-input"><span class="kr">from</span> <span class="nv">e</span> <span class="kr">in</span> <span class="nn">scott</span><span class="p">.</span><span class="n">emps</span>
  <span class="kr">yield</span> <span class="nn">e</span><span class="p">.</span><span class="n">deptno</span>
  <span class="kr">distinct</span>
  <span class="kr">order</span> <span class="n">deptno</span><span class="p">;</span></div>
<div class="code-output">val it = [10,20,30] : int list</div>
</div>

## 5. Set operators in pipelines

The set operators (`union`, `intersect` and `except`) were previously
available via functions but now have
[dedicated steps](https://github.com/hydromatic/morel/issues/253) in
the query pipeline.

The steps have slightly different semantics for ordered and unordered
collections, and have an optional `distinct` keyword to eliminate
duplicates.

For example, here is a query that finds all employees in departments
10 and 20, but excludes those who are managers or clerks:

<!-- morel skip
from e in scott.emps
  where e.deptno = 10
  union (from e in scott.emps where e.deptno = 20)
  except (from e in scott.emps where e.job = "MANAGER"),
     (from e in scott.emps where e.job = "CLERK");
-->

<div class="code-block">
<div class="code-input"><span class="kr">from</span> <span class="nv">e</span> <span class="kr">in</span> <span class="nn">scott</span><span class="p">.</span><span class="n">emps</span>
  <span class="kr">where</span> <span class="nn">e</span><span class="p">.</span><span class="n">deptno</span> <span class="p">=</span> <span class="mi">10</span>
  <span class="kr">union</span> <span class="p">(</span><span class="kr">from</span> <span class="nv">e</span> <span class="kr">in</span> <span class="nn">scott</span><span class="p">.</span><span class="n">emps</span> <span class="kr">where</span> <span class="nn">e</span><span class="p">.</span><span class="n">deptno</span> <span class="p">=</span> <span class="mi">20</span><span class="p">)</span>
  <span class="kr">except</span> <span class="p">(</span><span class="kr">from</span> <span class="nv">e</span> <span class="kr">in</span> <span class="nn">scott</span><span class="p">.</span><span class="n">emps</span> <span class="kr">where</span> <span class="nn">e</span><span class="p">.</span><span class="n">job</span> <span class="p">=</span> <span class="s2">"MANAGER"</span><span class="p">),</span>
     <span class="p">(</span><span class="kr">from</span> <span class="nv">e</span> <span class="kr">in</span> <span class="nn">scott</span><span class="p">.</span><span class="n">emps</span> <span class="kr">where</span> <span class="nn">e</span><span class="p">.</span><span class="n">job</span> <span class="p">=</span> <span class="s2">"CLERK"</span><span class="p">);</span></div>
</div>

If you have ever wondered about the semantics of `intersect` and
`except` with duplicates, wonder no more!
[INTERSECT ALL, EXCEPT ALL, and the arithmetic of fractions]({% post_url 2025-06-03-intersect-fractions %})
explains everything using a fun example.

## 6. String and Char structures

Morel now includes complete
[`String`](https://github.com/hydromatic/morel/issues/279) and
[`Char`](https://github.com/hydromatic/morel/issues/264) structures
following the
[Standard ML Basis Library](https://smlfamily.github.io/Basis/)
specification.

This gives you comprehensive text manipulation capabilities:

<!-- morel
String.size "hello world";
> val it = 11 : int

String.substring ("hello world", 6, 5);
> val it = "world" : string

String.tokens (fn c => c = #" ") "hello world morel";
> val it = ["hello","world","morel"] : string list

Char.isAlpha #"a";
> val it = true : bool

Char.toUpper #"a";
> val it = #"A" : char

String.map Char.toUpper "hello";
> val it = "HELLO" : string
-->

<div class="code-block">
<div class="code-input"><span class="nn">String</span><span class="p">.</span><span class="n">size</span> <span class="s2">"hello world"</span><span class="p">;</span></div>
<div class="code-output">val it = 11 : int</div>
<div class="code-input">
<span class="nn">String</span><span class="p">.</span><span class="n">substring</span> <span class="p">(</span><span class="s2">"hello world"</span><span class="p">,</span> <span class="mi">6</span><span class="p">,</span> <span class="mi">5</span><span class="p">);</span></div>
<div class="code-output">val it = "world" : string</div>
<div class="code-input">
<span class="nn">String</span><span class="p">.</span><span class="n">tokens</span> <span class="p">(</span><span class="kr">fn</span> <span class="n">c</span> <span class="o">=&gt;</span> <span class="n">c</span> <span class="p">=</span> #<span class="s2">" "</span><span class="p">)</span> <span class="s2">"hello world morel"</span><span class="p">;</span></div>
<div class="code-output">val it = ["hello","world","morel"] : string list</div>
<div class="code-input">
<span class="nn">Char</span><span class="p">.</span><span class="n">isAlpha</span> #<span class="s2">"a"</span><span class="p">;</span></div>
<div class="code-output">val it = true : bool</div>
<div class="code-input">
<span class="nn">Char</span><span class="p">.</span><span class="n">toUpper</span> #<span class="s2">"a"</span><span class="p">;</span></div>
<div class="code-output">val it = #"A" : char</div>
<div class="code-input">
<span class="nn">String</span><span class="p">.</span><span class="n">map</span> <span class="nn">Char</span><span class="p">.</span><span class="n">toUpper</span> <span class="s2">"hello"</span><span class="p">;</span></div>
<div class="code-output">val it = "HELLO" : string</div>
</div>

These structures provide everything you need for serious text
processing, from basic operations like substring extraction to
advanced features like tokenization and character classification.

## 7. Breaking changes

This release includes some breaking changes to be aware of.

### Database schema updates

The `scott` sample database now uses
[pluralized table names](https://github.com/hydromatic/morel/issues/255),
mapping the `emps` value maps to the `EMP` table, and `depts` to the
`DEPT` table.

<!-- morel skip
(* Old *)
from e in scott.emp
  join d in scott.dept on e.deptno = d.deptno;

(* New *)
from e in scott.emps
  join d in scott.depts on e.deptno = d.deptno;
-->

<div class="code-block">
<div class="code-input"><span class="c">(*</span><span class="cm"> Old *)</span>
<span class="kr">from</span> <span class="nv">e</span> <span class="kr">in</span> <span class="nn">scott</span><span class="p">.</span><span class="n">emp</span>
  <span class="kr">join</span> <span class="nv">d</span> <span class="kr">in</span> <span class="nn">scott</span><span class="p">.</span><span class="n">dept</span> <span class="kr">on</span> <span class="nn">e</span><span class="p">.</span><span class="n">deptno</span> <span class="p">=</span> <span class="nn">d</span><span class="p">.</span><span class="n">deptno</span><span class="p">;</span>

<span class="c">(*</span><span class="cm"> New *)</span>
<span class="kr">from</span> <span class="nv">e</span> <span class="kr">in</span> <span class="nn">scott</span><span class="p">.</span><span class="n">emps</span>
  <span class="kr">join</span> <span class="nv">d</span> <span class="kr">in</span> <span class="nn">scott</span><span class="p">.</span><span class="n">depts</span> <span class="kr">on</span> <span class="nn">e</span><span class="p">.</span><span class="n">deptno</span> <span class="p">=</span> <span class="nn">d</span><span class="p">.</span><span class="n">deptno</span><span class="p">;</span></div>
</div>

This change aligns with the modern programming convention that
collections have plural names.

### Type-based orderings

The previous `order` syntax no longer works.

You should convert a following `desc` to preceding `DESC`:

<!-- morel skip
(* Old syntax *)
from e in scott.emps
  order e.sal desc;

(* New syntax *)
from e in scott.emps
  order DESC e.sal;
-->

<div class="code-block">
<div class="code-input"><span class="c">(*</span><span class="cm"> Old syntax *)</span>
<span class="kr">from</span> <span class="nv">e</span> <span class="kr">in</span> <span class="nn">scott</span><span class="p">.</span><span class="n">emps</span>
  <span class="kr">order</span> <span class="nn">e</span><span class="p">.</span><span class="n">sal</span> <span class="kr">desc</span><span class="p">;</span>

<span class="c">(*</span><span class="cm"> New syntax *)</span>
<span class="kr">from</span> <span class="nv">e</span> <span class="kr">in</span> <span class="nn">scott</span><span class="p">.</span><span class="n">emps</span>
  <span class="kr">order</span> <span class="n">DESC</span> <span class="nn">e</span><span class="p">.</span><span class="n">sal</span><span class="p">;</span></div>
</div>

and put parentheses around composite orderings:

<!-- morel skip
(* Old syntax *)
from e in scott.emps
  order e.job, e.sal desc;

(* New syntax *)
from e in scott.emps
  order (e.job, DESC e.sal);
-->

<div class="code-block">
<div class="code-input"><span class="c">(*</span><span class="cm"> Old syntax *)</span>
<span class="kr">from</span> <span class="nv">e</span> <span class="kr">in</span> <span class="nn">scott</span><span class="p">.</span><span class="n">emps</span>
  <span class="kr">order</span> <span class="nn">e</span><span class="p">.</span><span class="n">job</span><span class="p">,</span> <span class="nn">e</span><span class="p">.</span><span class="n">sal</span> <span class="kr">desc</span><span class="p">;</span>

<span class="c">(*</span><span class="cm"> New syntax *)</span>
<span class="kr">from</span> <span class="nv">e</span> <span class="kr">in</span> <span class="nn">scott</span><span class="p">.</span><span class="n">emps</span>
  <span class="kr">order</span> <span class="p">(</span><span class="nn">e</span><span class="p">.</span><span class="n">job</span><span class="p">,</span> <span class="n">DESC</span> <span class="nn">e</span><span class="p">.</span><span class="n">sal</span><span class="p">);</span></div>
</div>

## Conclusion

Release 0.7.0 represents a major evolution in Morel's
capabilities. Extensions to the query language, type system, and
standard library make Morel a good solution for a wide range of
data processing tasks, from simple queries to complex data
transformations.

As always, you can get started with Morel by visiting
[GitHub](https://github.com/hydromatic/morel).
For more background, read about its
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

This article
[has been updated](https://github.com/julianhyde/share/commits/main/blog/{{ page.path }}).
