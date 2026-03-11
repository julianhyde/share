---
layout: post
title:  "Morel release 0.5.0"
date:   2025-03-04 12:00:00 -0800
author: Julian Hyde
image:  /assets/img/OldDesignShop_MushroomSpringMorel-240x240.jpg
tweet:  https://x.com/julianhyde/status/1897425406279737401
---

I am pleased to announce Morel
[release 0.5.0](https://github.com/hydromatic/morel/blob/main/HISTORY.md#050--2025-03-04).
Coming fourteen months after
[release 0.4.0](https://github.com/hydromatic/morel/blob/main/HISTORY.md#040--2024-01-04),
this release contains various extensions to syntax to make the
language more powerful and easy to use.

We describe a few of the new features in this article.  For more
information, see the official
[release notes](https://github.com/hydromatic/morel/blob/main/HISTORY.md#050--2025-03-04).

# 1. List-valued functions in pipelines (`into` and `through`)

[[MOREL-171](https://github.com/hydromatic/morel/issues/171)] lets you
use list-based functions in a pipeline, adding two clauses to
the `from` expression:
 * `into` occurs at the end of a pipeline, and converts a list
   to a scalar value;
 * `through` occurs in the middle of a pipeline, and converts a
   list into another list.

First, `into`.
<pre>
<b>from</b> <i>scans</i>
  <i>steps</i>
  <b>into</b> <i>expression</i>
</pre>
is equivalent to
<pre>
(<i>expression</i>) (<b><code>from</code></b> <i>scans</i> <i>steps</i>)
</pre>

*expression* must be a function of type `'a list -> 'b` (for some
 types `'a` and `'b`), and if the output of the last step is `'a` then
 the type of the whole `from` expression is `'b`.

For example, `sum` has type `int list -> int`, and so `into sum`
converts a stream of `int` values into an `int` result.

<!-- morel skip
(* "sum" adds a list of integers. *)
sum [1, 2, 4];
> val it = 7 : int

(* "into" applies a function to its input, collected into a list. *)
from i in [1, 2, 4]
  into sum;
> val it = 7 : int

(* Rewrite "into" to apply the function directly. *)
sum (from e in [1, 2, 4]);
> val it = 7 : int

(* "into" is equivalent to existing keyword "compute". *)
from e in [1, 2, 4]
  compute sum;
> val it = 7 : int

(* Actually, "a into b" is equivalent to "(b) a" for any types, not
   just lists. *)
explode "abc";
> val it = [#"a",#"b",#"c"] : char list

"abc" into explode;
> val it = [#"a",#"b",#"c"] : char list
-->

<div class="code-block">
<div class="code-input"><span class="cmt">(* "sum" adds a list of integers. *)</span>
sum [<span class="num">1</span>, <span class="num">2</span>, <span class="num">4</span>];</div>
<div class="code-output">val it = 7 : int</div>
<div class="code-input">
<span class="cmt">(* "into" applies a function to its input, collected into a list. *)</span>
<span class="kw">from</span> i <span class="kw">in</span> [<span class="num">1</span>, <span class="num">2</span>, <span class="num">4</span>]
  into sum;</div>
<div class="code-output">val it = 7 : int</div>
<div class="code-input">
<span class="cmt">(* Rewrite "into" to apply the function directly. *)</span>
sum (<span class="kw">from</span> e <span class="kw">in</span> [<span class="num">1</span>, <span class="num">2</span>, <span class="num">4</span>]);</div>
<div class="code-output">val it = 7 : int</div>
<div class="code-input">
<span class="cmt">(* "into" is equivalent to existing keyword "compute". *)</span>
<span class="kw">from</span> e <span class="kw">in</span> [<span class="num">1</span>, <span class="num">2</span>, <span class="num">4</span>]
  <span class="kw">compute</span> sum;</div>
<div class="code-output">val it = 7 : int</div>
<div class="code-input">
<span class="cmt">(* Actually, "a into b" is equivalent to "(b) a" for any types, not
   just lists. *)</span>
explode <span class="str">"abc"</span>;</div>
<div class="code-output">val it = [#"a",#"b",#"c"] : char list</div>
<div class="code-input">
<span class="str">"abc"</span> into explode;</div>
<div class="code-output">val it = [#"a",#"b",#"c"] : char list</div>
</div>




Next, `through` is similar to `into`, but has a pattern so that
following steps can refer to the data items:

<pre>
<i>expression1</i>
  <b>through</b> <i>pattern</i> <b>in</b> <i>expression2</i>
  <i>steps</i>
</pre>

is equivalent to

<pre>
<b>from</b> <i>pattern</i> <b>in</b> (<i>expression2</i>) (<i>expression1</i>)
  <i>steps</i>
</pre>

For example, suppose we have a `clean_address` function that takes a
list of orders and returns a list of orders with state and zipcode
filled out. To stream orders through this function, we simply add the
line `through clean_over in clean_address` to the pipeline.

<!-- morel skip
(* Function that converts a list of orders to a list of orders with
   corrected state and zipcode. *)
fun clean_address ...
> val clean_address = fn : order list -> order list;

(* Define a function that takes a collection of orders, removes orders with
   more than 1,000 items, cleans addresses, and summarizes by state. *)
fun pipeline orders =
    from order in orders
       where order.units < 1000
       through clean_order in clean_address
       group clean_order.state compute count;
> val pipeline = fn : order list -> {count: int, state: string} list;
-->

<div class="code-block">
<div class="code-input"><span class="cmt">(* Function that converts a list of orders to a list of orders with
   corrected state and zipcode. *)</span>
<span class="kw">fun</span> clean_address ...</div>
<div class="code-output">val clean_address = fn : order list -&gt; order list;</div>
<div class="code-input">
<span class="cmt">(* Define a function that takes a collection of orders, removes orders with
   more than 1,000 items, cleans addresses, and summarizes by state. *)</span>
<span class="kw">fun</span> pipeline orders =
    <span class="kw">from</span> <span class="kw">order</span> <span class="kw">in</span> orders
       <span class="kw">where</span> <span class="kw">order</span>.units &lt; <span class="num">1000</span>
       through clean_order <span class="kw">in</span> clean_address
       <span class="kw">group</span> <span class="ctor">clean_order</span>.state <span class="kw">compute</span> count;</div>
<div class="code-output">val pipeline = fn : order list -&gt; {count: int, state: string} list;</div>
</div>


Note that the `pipeline` function itself takes a list argument and
returns a list.  We could therefore include it in a higher-level query
using the `through` keyword, and include that query in another query.

The `into` and `through` keywords, combined with Morel's ability to
include queries in function declarations, allow us to do something
very powerful: to compose complex and efficient data flows from
concise functions.

# 2. Comma-separated scans

If you speak SQL, you will know that there are two ways to write a
join:

{% highlight sql %}
-- Comma syntax
SELECT e.ename, d.dname
FROM Emp AS e,
  Dept AS d
WHERE e.deptno = d.deptno;

-- ANSI (SQL-92) syntax using JOIN keyword
SELECT e.ename, d.dname
FROM Emp AS e,
  JOIN Dept AS d ON e.deptno = d.deptno;
{% endhighlight %}

Morel has analogous syntax:

<!-- morel silent
val emps = scott.emps;
val depts = scott.depts;
> val emps = <relation>
>   :
>     {comm:real, deptno:int, empno:int, ename:string, hiredate:string,
>      job:string, mgr:int, sal:real} bag
> val depts = <relation> : {deptno:int, dname:string, loc:string} bag
-->

<!-- morel skip
from e in emps,
    d in depts
  where e.deptno = d.deptno
  yield {e.ename, d.dname};
> val it =
>   [{dname="ACCOUNTING",ename="CLARK"},{dname="ACCOUNTING",ename="KING"},
>    {dname="ACCOUNTING",ename="MILLER"},{dname="RESEARCH",ename="JONES"},
>    {dname="RESEARCH",ename="FORD"},{dname="RESEARCH",ename="ADAMS"},
>    {dname="RESEARCH",ename="SMITH"},{dname="RESEARCH",ename="SCOTT"},
>    {dname="SALES",ename="WARD"},{dname="SALES",ename="TURNER"},
>    {dname="SALES",ename="ALLEN"},{dname="SALES",ename="JAMES"},
>    {dname="SALES",ename="BLAKE"},{dname="SALES",ename="MARTIN"}]
>   : {dname:string, ename:string} list

from e in emps
  join d in depts on e.deptno = d.deptno
  yield {e.ename, d.dname};
> val it =
>   [{dname="ACCOUNTING",ename="CLARK"},{dname="ACCOUNTING",ename="KING"},
>    {dname="ACCOUNTING",ename="MILLER"},{dname="RESEARCH",ename="JONES"},
>    {dname="RESEARCH",ename="FORD"},{dname="RESEARCH",ename="ADAMS"},
>    {dname="RESEARCH",ename="SMITH"},{dname="RESEARCH",ename="SCOTT"},
>    {dname="SALES",ename="WARD"},{dname="SALES",ename="TURNER"},
>    {dname="SALES",ename="ALLEN"},{dname="SALES",ename="JAMES"},
>    {dname="SALES",ename="BLAKE"},{dname="SALES",ename="MARTIN"}]
>   : {dname:string, ename:string} list
-->

<div class="code-block">
<div class="code-input"><span class="kw">from</span> e <span class="kw">in</span> emps,
    d <span class="kw">in</span> depts
  <span class="kw">where</span> <span class="ctor">e</span>.deptno = <span class="ctor">d</span>.deptno
  <span class="kw">yield</span> {<span class="ctor">e</span>.ename, <span class="ctor">d</span>.dname};</div>
<div class="code-output">val it =
  [{dname="ACCOUNTING",ename="CLARK"},{dname="ACCOUNTING",ename="KING"},
   {dname="ACCOUNTING",ename="MILLER"},{dname="RESEARCH",ename="JONES"},
   {dname="RESEARCH",ename="FORD"},{dname="RESEARCH",ename="ADAMS"},
   {dname="RESEARCH",ename="SMITH"},{dname="RESEARCH",ename="SCOTT"},
   {dname="SALES",ename="WARD"},{dname="SALES",ename="TURNER"},
   {dname="SALES",ename="ALLEN"},{dname="SALES",ename="JAMES"},
   {dname="SALES",ename="BLAKE"},{dname="SALES",ename="MARTIN"}]
  : {dname:string, ename:string} list</div>
<div class="code-input">
<span class="kw">from</span> e <span class="kw">in</span> emps
  <span class="kw">join</span> d <span class="kw">in</span> depts <span class="kw">on</span> <span class="ctor">e</span>.deptno = <span class="ctor">d</span>.deptno
  <span class="kw">yield</span> {<span class="ctor">e</span>.ename, <span class="ctor">d</span>.dname};</div>
<div class="code-output">val it =
  [{dname="ACCOUNTING",ename="CLARK"},{dname="ACCOUNTING",ename="KING"},
   {dname="ACCOUNTING",ename="MILLER"},{dname="RESEARCH",ename="JONES"},
   {dname="RESEARCH",ename="FORD"},{dname="RESEARCH",ename="ADAMS"},
   {dname="RESEARCH",ename="SMITH"},{dname="RESEARCH",ename="SCOTT"},
   {dname="SALES",ename="WARD"},{dname="SALES",ename="TURNER"},
   {dname="SALES",ename="ALLEN"},{dname="SALES",ename="JAMES"},
   {dname="SALES",ename="BLAKE"},{dname="SALES",ename="MARTIN"}]
  : {dname:string, ename:string} list</div>
</div>


but used to only allow the comma join syntax immediately after the
`from` keyword, before clauses such as `where` or `join` had occurred.

Following
[[MOREL-216](https://github.com/hydromatic/morel/issues/216)], Morel
allows comma-separated joins later in the pipeline, and also allows
`on` in comma-joins. The following is now legal:

<!-- morel
from a in [1, 2],
    b in [3, 4, 5] on a + b = 6
  where b < 5
  join c in [6, 7] on b + c = 10,
      d in [7, 8];
> val it = [{a=2,b=4,c=6,d=7},{a=2,b=4,c=6,d=8}]
>   : {a:int, b:int, c:int, d:int} list
-->

<div class="code-block">
<div class="code-input"><span class="kw">from</span> a <span class="kw">in</span> [<span class="num">1</span>, <span class="num">2</span>],
    b <span class="kw">in</span> [<span class="num">3</span>, <span class="num">4</span>, <span class="num">5</span>] <span class="kw">on</span> a <span class="op">+</span> b = <span class="num">6</span>
  <span class="kw">where</span> b &lt; <span class="num">5</span>
  <span class="kw">join</span> c <span class="kw">in</span> [<span class="num">6</span>, <span class="num">7</span>] <span class="kw">on</span> b <span class="op">+</span> c = <span class="num">10</span>,
      d <span class="kw">in</span> [<span class="num">7</span>, <span class="num">8</span>];</div>
<div class="code-output">val it = [{a=2,b=4,c=6,d=7},{a=2,b=4,c=6,d=8}]
  : {a:int, b:int, c:int, d:int} list</div>
</div>


This will be particularly convenient (when we have solved some
query-planning issues in
[[MOREL-229](https://github.com/hydromatic/morel/issues/229)]) for
writing queries that use unbounded variables to solve constraints:

<!-- morel skip
from a, b
  where a < b
join c, d, e
  where a > 0
    andalso b > 0
    andalso c > 0
    andalso d > 0
    andalso e > 0
    andalso a + b + c + d + e < 8;
> val it =
>   [{a=1,b=2,c=1,d=1,e=1},{a=1,b=2,c=1,d=1,e=2},{a=1,b=2,c=1,d=2,e=1},
>    {a=1,b=2,c=2,d=1,e=1},{a=1,b=3,c=1,d=1,e=1}]
>   : {a:int, b:int, c:int, d:int, e:int} list
-->

<div class="code-block">
<div class="code-input"><span class="kw">from</span> a, b
  <span class="kw">where</span> a &lt; b
<span class="kw">join</span> c, d, e
  <span class="kw">where</span> a &gt; <span class="num">0</span>
    <span class="kw">andalso</span> b &gt; <span class="num">0</span>
    <span class="kw">andalso</span> c &gt; <span class="num">0</span>
    <span class="kw">andalso</span> d &gt; <span class="num">0</span>
    <span class="kw">andalso</span> e &gt; <span class="num">0</span>
    <span class="kw">andalso</span> a <span class="op">+</span> b <span class="op">+</span> c <span class="op">+</span> d <span class="op">+</span> e &lt; <span class="num">8</span>;</div>
<div class="code-output">val it =
  [{a=1,b=2,c=1,d=1,e=1},{a=1,b=2,c=1,d=1,e=2},{a=1,b=2,c=1,d=2,e=1},
   {a=1,b=2,c=2,d=1,e=1},{a=1,b=3,c=1,d=1,e=1}]
  : {a:int, b:int, c:int, d:int, e:int} list</div>
</div>


# 3. Duplicate elimination (`distinct`)

[[MOREL-231](https://github.com/hydromatic/morel/issues/231)] adds a
`distinct` clause to the `from` expression. It makes the rows unique.

Here is a query that finds the set of distinct job titles:

<!-- morel skip
from e in scott.emp
  yield {e.job}
  distinct;
> val it =
>   [{job="CLERK"},{job="SALESMAN"},{job="ANALYST"},{job="MANAGER"},
>    {job="PRESIDENT"}] : {job:string} list
-->

<div class="code-block">
<div class="code-input"><span class="kw">from</span> e <span class="kw">in</span> <span class="ctor">scott</span>.emp
  <span class="kw">yield</span> {<span class="ctor">e</span>.job}
  <span class="kw">distinct</span>;</div>
<div class="code-output">val it =
  [{job="CLERK"},{job="SALESMAN"},{job="ANALYST"},{job="MANAGER"},
   {job="PRESIDENT"}] : {job:string} list</div>
</div>


`distinct` is short-hand for `group` with all fields and no aggregate
functions (`compute` clause), and is similar to SQL's `SELECT
DISTINCT`.

# 4. Multiple branches in `fn`

[[MOREL-230](https://github.com/hydromatic/morel/issues/230)] allows a
lambda (`fn` expression) to have multiple branches, similar to `case`.
Following this change, the following expressions are equivalent:

<!-- morel
fn [] => 0 | x :: _ => x + 1;
> val it = fn : int list -> int
-->

<div class="code-block">
<div class="code-input"><span class="kw">fn</span> [] <span class="op">=&gt;</span> <span class="num">0</span> | x <span class="op">::</span> _ <span class="op">=&gt;</span> x <span class="op">+</span> <span class="num">1</span>;</div>
<div class="code-output">val it = fn : int list -&gt; int</div>
</div>


<!-- morel
fn list => case list of [] => 0 | x :: _ => x + 1;
> val it = fn : int list -> int
-->

<div class="code-block">
<div class="code-input"><span class="kw">fn</span> list <span class="op">=&gt;</span> <span class="kw">case</span> list <span class="kw">of</span> [] <span class="op">=&gt;</span> <span class="num">0</span> | x <span class="op">::</span> _ <span class="op">=&gt;</span> x <span class="op">+</span> <span class="num">1</span>;</div>
<div class="code-output">val it = fn : int list -&gt; int</div>
</div>


Prior to this change, the first expression would give a syntax error.

# 5. `Int` structure

[[MOREL-228](https://github.com/hydromatic/morel/issues/228)]
implements the `Int` structure, a collection of functions and values
related to the `int` type.
Per [Moscow ML](https://mosml.org/mosmllib/Int.html) it has the
following interface:

<!-- morel skip
val precision : int option
val minInt    : int option
val maxInt    : int option

val ~         : int -> int              (* Overflow      *)
val *         : int * int -> int        (* Overflow      *)
val div       : int * int -> int        (* Div, Overflow *)
val mod       : int * int -> int        (* Div           *)
val quot      : int * int -> int        (* Div, Overflow *)
val rem       : int * int -> int        (* Div           *)
val +         : int * int -> int        (* Overflow      *)
val -         : int * int -> int        (* Overflow      *)
val >         : int * int -> bool
val >=        : int * int -> bool
val <         : int * int -> bool
val <=        : int * int -> bool
val abs       : int -> int              (* Overflow      *)
val min       : int * int -> int
val max       : int * int -> int

val sign      : int -> int
val sameSign  : int * int -> bool
val compare   : int * int -> order

val toInt     : int -> int
val fromInt   : int -> int
val toLarge   : int -> int
val fromLarge : int -> int

val scan      : StringCvt.radix
                -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader
val fmt       : StringCvt.radix -> int -> string

val toString  : int -> string
val fromString : string -> int option   (* Overflow      *)
-->

<div class="code-block">
<div class="code-input"><span class="kw">val</span> precision <span class="op">:</span> int option
<span class="kw">val</span> minInt    <span class="op">:</span> int option
<span class="kw">val</span> maxInt    <span class="op">:</span> int option

<span class="kw">val</span> ~         <span class="op">:</span> int <span class="op">-&gt;</span> int              <span class="cmt">(* Overflow      *)</span>
<span class="kw">val</span> <span class="op">*</span>         <span class="op">:</span> int <span class="op">*</span> int <span class="op">-&gt;</span> int        <span class="cmt">(* Overflow      *)</span>
<span class="kw">val</span> div       <span class="op">:</span> int <span class="op">*</span> int <span class="op">-&gt;</span> int        <span class="cmt">(* Div, Overflow *)</span>
<span class="kw">val</span> mod       <span class="op">:</span> int <span class="op">*</span> int <span class="op">-&gt;</span> int        <span class="cmt">(* Div           *)</span>
<span class="kw">val</span> quot      <span class="op">:</span> int <span class="op">*</span> int <span class="op">-&gt;</span> int        <span class="cmt">(* Div, Overflow *)</span>
<span class="kw">val</span> rem       <span class="op">:</span> int <span class="op">*</span> int <span class="op">-&gt;</span> int        <span class="cmt">(* Div           *)</span>
<span class="kw">val</span> <span class="op">+</span>         <span class="op">:</span> int <span class="op">*</span> int <span class="op">-&gt;</span> int        <span class="cmt">(* Overflow      *)</span>
<span class="kw">val</span> <span class="op">-</span>         <span class="op">:</span> int <span class="op">*</span> int <span class="op">-&gt;</span> int        <span class="cmt">(* Overflow      *)</span>
<span class="kw">val</span> &gt;         <span class="op">:</span> int <span class="op">*</span> int <span class="op">-&gt;</span> bool
<span class="kw">val</span> &gt;=        <span class="op">:</span> int <span class="op">*</span> int <span class="op">-&gt;</span> bool
<span class="kw">val</span> &lt;         <span class="op">:</span> int <span class="op">*</span> int <span class="op">-&gt;</span> bool
<span class="kw">val</span> &lt;=        <span class="op">:</span> int <span class="op">*</span> int <span class="op">-&gt;</span> bool
<span class="kw">val</span> abs       <span class="op">:</span> int <span class="op">-&gt;</span> int              <span class="cmt">(* Overflow      *)</span>
<span class="kw">val</span> min       <span class="op">:</span> int <span class="op">*</span> int <span class="op">-&gt;</span> int
<span class="kw">val</span> max       <span class="op">:</span> int <span class="op">*</span> int <span class="op">-&gt;</span> int

<span class="kw">val</span> sign      <span class="op">:</span> int <span class="op">-&gt;</span> int
<span class="kw">val</span> sameSign  <span class="op">:</span> int <span class="op">*</span> int <span class="op">-&gt;</span> bool
<span class="kw">val</span> compare   <span class="op">:</span> int <span class="op">*</span> int <span class="op">-&gt;</span> <span class="kw">order</span>

<span class="kw">val</span> toInt     <span class="op">:</span> int <span class="op">-&gt;</span> int
<span class="kw">val</span> fromInt   <span class="op">:</span> int <span class="op">-&gt;</span> int
<span class="kw">val</span> toLarge   <span class="op">:</span> int <span class="op">-&gt;</span> int
<span class="kw">val</span> fromLarge <span class="op">:</span> int <span class="op">-&gt;</span> int

<span class="kw">val</span> scan      <span class="op">:</span> <span class="ctor">StringCvt</span>.radix
                <span class="op">-&gt;</span> (char, <span class="ctor">'a</span>) <span class="ctor">StringCvt</span>.reader <span class="op">-&gt;</span> (int, <span class="ctor">'a</span>) <span class="ctor">StringCvt</span>.reader
<span class="kw">val</span> fmt       <span class="op">:</span> <span class="ctor">StringCvt</span>.radix <span class="op">-&gt;</span> int <span class="op">-&gt;</span> string

<span class="kw">val</span> toString  <span class="op">:</span> int <span class="op">-&gt;</span> string
<span class="kw">val</span> fromString <span class="op">:</span> string <span class="op">-&gt;</span> int option   <span class="cmt">(* Overflow      *)</span></div>
</div>


Example use:

<!-- morel
Int.compare;
> val it = fn : int * int -> order
Int.compare (2, 3);
> val it = LESS : order
Int.maxInt;
> val it = SOME 2147483647 : int option
-->

<div class="code-block">
<div class="code-input"><span class="ctor">Int</span>.compare;</div>
<div class="code-output">val it = fn : int * int -&gt; order</div>
<div class="code-input"><span class="ctor">Int</span>.compare (<span class="num">2</span>, <span class="num">3</span>);</div>
<div class="code-output">val it = LESS : order</div>
<div class="code-input"><span class="ctor">Int</span>.maxInt;</div>
<div class="code-output">val it = SOME 2147483647 : int option</div>
</div>


The `Int` structure is an instance of the
[`INTEGER` signature in the Standard ML Basis
Library](https://smlfamily.github.io/Basis/integer.html) but Morel
does not currently have signatures.

# Conclusion

Now that I have
[resigned from Google]({% post_url 2025-03-03-into-the-wilderness %}),
I will have more time to work on Morel bugs and features, in two areas
in particular.

I want to improve how Morel handles
[ordered and unordered multisets](https://github.com/hydromatic/morel/issues/235),
because SQL tables are unordered, functional programming languages use
ordered lists, and real applications need to mix both.
We will add a `bag` type, and to keep the
[Hindley-Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)
happy when converting between the `list` and `bag` types, we will need
better support for
[operator overloading](https://github.com/hydromatic/morel/issues/237).

I believe that Morel can be an attractive solution for graph,
deductive and constraint-programming problems.  To that end, I will be
working on
[constraints](https://github.com/hydromatic/morel/issues/239),
[universal and existential quantifiers](https://github.com/hydromatic/morel/issues/241),
[deduction of ranges](https://github.com/hydromatic/morel/issues/229), and
[tail-call optimization](https://github.com/hydromatic/morel/issues/151).

If you have comments, please reply on
[Bluesky @julianhyde.bsky.social](https://bsky.app/profile/julianhyde.bsky.social)
or Twitter:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>

This article
[has been updated](https://github.com/julianhyde/share/commits/main/blog/{{ page.path }}).
