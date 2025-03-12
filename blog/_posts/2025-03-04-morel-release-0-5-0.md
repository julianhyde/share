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

{% highlight sml %}
- (* sum adds a list of integers *)
= sum [1, 2, 4];
val it = 7 : int

- (* 'into' applies a function to its input, collected into a list. *)
= from i in [1, 2, 4]
=   into sum;
val i = 7 : int

- (* Rewrite 'into' to apply the function directly. *)
= sum (from e in [1, 2, 4]);
val i = 7 : int

- (* 'into' is equivalent to existing keyword 'compute'. *)
= from e in [1, 2, 4]
=   compute sum;
val i = 7 : int

- (* Actually, "a into b" is equivalent to "(b) a" for any types, not
=    just lists. *)
= explode "abc";
> val it = [#"a",#"b",#"c"] : char list

- "abc" into explode;
val it = [#"a",#"b",#"c"] : char list
{% endhighlight %}

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

{% highlight sml %}
- (* Function that converts a list of orders to a list of orders with
=    corrected state and zipcode. *)
= fun clean_address ...
val clean_address = fn : order list -> order list;

- (* Define a function that takes a collection of orders, removes orders with
=    more than 1,000 items, cleans addresses, and summarizes by state. *)
= fun pipeline orders =
=     from order in orders
=        where order.units < 1000
=        through clean_order in clean_address
=        group clean_order.state compute count;
val pipeline = fn : order list -> {count: int, state: string} list;
{% endhighlight %}

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
{% highlight sml %}
- from e in emps,
=     d in depts
=   where e.deptno = d.deptno
=   yield {e.ename, d.dname};
val it =
  [{dname="RESEARCH",ename="SMITH"},{dname="SALES",ename="ALLEN"},
   {dname="SALES",ename="WARD"},...] : {dname:string, ename:string} list

- from e in emps
=   join d in depts on e.deptno = d.deptno
=   yield {e.ename, d.dname};
val it =
  [{dname="RESEARCH",ename="SMITH"},{dname="SALES",ename="ALLEN"},
   {dname="SALES",ename="WARD"},...] : {dname:string, ename:string} list
{% endhighlight %}

but used to only allow the comma join syntax immediately after the
`from` keyword, before clauses such as `where` or `join` had occurred.

Following
[[MOREL-216](https://github.com/hydromatic/morel/issues/216)], Morel
allows comma-separated joins later in the pipeline, and also allows
`on` in comma-joins. The following is now legal:

{% highlight sml %}
- from a in [1, 2],
=     b in [3, 4, 5] on a + b = 6
=   where b < 5
=   join c in [6, 7] on b + c = 10,
=       d in [7, 8];
val it = [{a=2,b=4,c=6,d=7},{a=2,b=4,c=6,d=8}]
  : {a:int, b:int, c:int, d:int} list
{% endhighlight %}

This will be particularly convenient (when we have solved some
query-planning issues in
[[MOREL-229](https://github.com/hydromatic/morel/issues/229)]) for
writing queries that use unbounded variables to solve constraints:

{% highlight sml %}
- from a, b
=   where a < b
= join c, d, e
=   where a > 0
=     andalso b > 0
=     andalso c > 0
=     andalso d > 0
=     andalso e > 0
=     andalso a + b + c + d + e < 8;
val it =
  [{a=1,b=2,c=1,d=1,e=1},{a=1,b=2,c=1,d=1,e=2},{a=1,b=2,c=1,d=2,e=1},
   {a=1,b=2,c=2,d=1,e=1},{a=1,b=3,c=1,d=1,e=1}]
  : {a:int, b:int, c:int, d:int, e:int} list
{% endhighlight %}

# 3. Duplicate elimination (`distinct`)

[[MOREL-231](https://github.com/hydromatic/morel/issues/231)] adds a
`distinct` clause to the `from` expression. It makes the rows unique.

Here is a query that finds the set of distinct job titles:

{% highlight sml %}
- from e in scott.emp
=   yield {e.job}
=   distinct;
val it = ["CLERK","SALESMAN","ANALYST","MANAGER","PRESIDENT"] : string list
{% endhighlight %}

`distinct` is short-hand for `group` with all fields and no aggregate
functions (`compute` clause), and is similar to SQL's `SELECT
DISTINCT`.

# 4. Multiple branches in `fn`

[[MOREL-230](https://github.com/hydromatic/morel/issues/230)] allows a
lambda (`fn` expression) to have multiple branches, similar to `case`.
Following this change, the following expressions are equivalent:

{% highlight sml %}
- fn [] => 0 | x :: _ => x + 1;
val it = fn : int list -> int
{% endhighlight %}

{% highlight sml %}
- fn list => case list of [] => 0 | x :: _ => x + 1;
val it = fn : int list -> int
{% endhighlight %}

Prior to this change, the first expression would give a syntax error.

# 5. `Int` structure

[[MOREL-228](https://github.com/hydromatic/morel/issues/228)]
implements the `Int` structure, a collection of functions and values
related to the `int` type.
Per [Moscow ML](https://mosml.org/mosmllib/Int.html) it has the
following interface:

{% highlight sml %}
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
{% endhighlight %}

Example use:
{% highlight sml %}
- Int.compare;
val it = fn : int * int -> order
- Int.compare (2, 3);
val it = LESS : order
- Int.maxInt;
val it = SOME 1073741823 : int option
{% endhighlight %}

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
[BlueSky @julianhyde.bsky.social](https://bsky.app/profile/julianhyde.bsky.social)
or Twitter:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>

This article
[has been updated](https://github.com/julianhyde/share/commits/main/blog/_posts/2025-03-04-morel-release-0-5-0.md).
