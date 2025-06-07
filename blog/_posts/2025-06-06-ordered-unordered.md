---
layout: post
title:  "Ordered and unordered data"
date:   2025-06-06 12:00:00 -0800
author: Julian Hyde
image:  /assets/img/OldDesignShop_MushroomSpringMorel-240x240.jpg
tweet:  https://x.com/julianhyde/status/1931153173097660591
---

Despite what the relational model says, some data is *ordered*.

I'm not talking about *sorted* data. If you sort a collection,
applying a comparator function to its elements, then you have no
more information than you had before.

No, the integer list

```sml
[3, 1, 4, 1, 5, 9, 2, 6, 5, 3]
```

and the string list

```sml
["Shall I compare thee to a summer's day?",
  "Thou art more lovely and more temperate",
  "Rough winds do shake the darling buds of May",
  "And summer's lease hath all too short a date"]
```

depend on the order of their elements for their meaning.

But of course, some data is *unordered*, for good reason. A relational
database would be foolish to guarantee that if you write rows into a
table in a particular order, they will be read back in the same
order. Such a guarantee would seriously limit the database's
scalability.

This post is about how we allow ordered and unordered data to coexist
in [Morel](https://github.com/hydromatic/morel).

We achieved this with a collection of new features, including
[adding a `bag` type](https://github.com/hydromatic/morel/issues/235),
the
[ordered relational operators](https://github.com/hydromatic/morel/issues/273),
the
[`ordinal` keyword](https://github.com/hydromatic/morel/issues/276),
and a new
[`unorder` step](https://github.com/hydromatic/morel/issues/277).
All of these features will appear shortly in Morel release 0.7.

## List and bag types

As a functional query language, Morel spans the worlds of database and
functional programming.

Databases' fundamental type, the relation, is an unordered collection
of records.  (Though curiously, modern SQL allows columns to contain
"nested tables", which can be either of the ordered `ARRAY` type or
the unordered `MULTISET` type.)

Functional programming languages' fundamental type is the list, an
ordered type. Functional programs are often defined by structural
induction on lists.  For example, the function

```sml
fun allPositive [] = true
  | allPositive (x::xs) = x > 0 andalso allPositive xs;
(*[> val allPositive = fn : int list -> bool]*)
```

inductively defines that a list of numbers is "all-positive" if it is
empty, or if its first element is positive and the rest of the list is
"all-positive". This kind of inductive definition requires a firm
distinction between the first element of a list and the rest of the
list, a distinction that is not present in an unordered collection.

So, Morel needs to support both ordered and unordered collections.

Earlier versions of Morel papered over the difference. All collections
had type `list`, even the unordered collections backed by database
tables. Morel's relational operators produced results in deterministic
order if you applied them to in-memory collections using the
in-process interpreter, but order was not guaranteed when Morel
converted the query to SQL for execution in a DBMS.

To fix the problem, the first step was to add a `bag` type.  (Bag is a
synonym for [multiset](https://en.wikipedia.org/wiki/Multiset),
implying a given element may occur more than once, but iteration order
is not defined.) `bag` is the unordered counterpart to the ordered
`list` type, and has similar operations.

```sml
val b = bag [3, 1, 4, 1, 5];
(*[> val b = [3,1,4,1,5] : int bag]*)
Bag.length b;
(*[> val it = 5 : int]*)
Bag.toList b;
(*[> val it = [3,1,4,1,5] : int list]*)
Bag.fromList [3, 1, 4, 1, 5];
(*[> val it = [3,1,4,1,5] : int bag]*)
```

Order-dependent operations from the `list` type, such as `hd` and
`drop`, are defined for `bag` instances, but they are not guaranteed
to return the same result every time you call them.

```sml
Bag.hd b;
(*[> val it = 3 : int]*)
Bag.drop (b, 2);
(*[> val it = [4,1,5] : int bag]*)
```

Collections backed by database tables now have type `bag`:

```sml
from e in scott.depts;

(*[> deptno dname      loc
> ------ ---------- --------
> 10     ACCOUNTING NEW YORK
> 20     RESEARCH   DALLAS
> 30     SALES      CHICAGO
> 40     OPERATIONS BOSTON
>
> val it : {deptno:int, dname:string, loc:string} bag]*)
```

(You may notice that `scott.depts` collection, backed by the `DEPT`
table of the `SCOTT` JDBC data source, has changed its name as well
as its type. It used to be called `scott.dept`. Morel collection names
should be plural and lower-case, and improvements to the
[name mapping system](https://github.com/hydromatic/morel/issues/255)
make it easier to derive proper collection names.)

Next, we provide relational operators to convert between `list` and
`bag`.

## Converting between ordered and unordered

Now that queries can reference `list` and `bag` collections, we need
operators to convert from one to the other. To do this, we use the
existing `order` step, and add an `unorder` step and an `ordinal`
expression.

In previous versions of Morel, the `order` step converted a list to a
`list` with a different ordering; now its input can be a list *or* a
bag:

```sml
from i in [3, 1, 4, 1, 5]
  order DESC i;
(*[> val it = [5,4,3,1,1] : int list]*)
from i in bag [3, 1, 4, 1, 5]
  order DESC i;
(*[> val it = [5,4,3,1,1] : int list]*)
```

If the sort key does not create a total ordering, the results will be
nondeterministic but still a list. For example, we can sort integers
so that even numbers occur before odd numbers

```sml
from i in bag [3, 1, 4, 1, 5]
  order i mod 2;
(*[> val it = [4, 1, 5, 1, 3]]*)
```

or convert a bag to a list in arbitrary order.

```sml
from i in bag [3, 1, 4, 1, 5]
  order ();
(*[> val it = [5, 4, 1, 1, 3]]*)
```
  
To go the opposite direction, the new `unorder` step converts a list
to a bag:

```sml
from i in [3, 1, 4, 1, 5]
  unorder;
(*[> val it = [3,1,4,1,5] : int bag]*)
```

(You are also free to apply `unorder` to a `bag`; it will have no
effect.)

As we said above, a `bag` contains less information than its
corresponding `list`. If you plan to convert the `bag` to a `list`
at a later stage, you need to store the ordering in an extra field.
The new `ordinal` expression lets us do this:

```sml
from i in [3, 1, 4, 1, 5]
  yield {i, j = ordinal}
  unorder
  order j
  yield i;
(*[> val it = [3,1,4,1,5] : int list]*)
```

The `ordinal` expression can be used in an expression in a
step whose input is ordered (except the steps whose expressions are
evaluated before the query starts: `except`, `intersect`, `skip`,
`take`, and `union`). `ordinal` evaluates to 0 for the first element,
1 for the next element, and so on. But as we shall see, the optimizer
avoids evaluating `ordinal` if it can.

Here is a query that computes the salary rank of each employee,
then returns only the poorly-paid clerks.

```sml
from e in scott.emps
  order e.sal
  yield {e, rank = 1 + ordinal}
  where e.job = "CLERK";

(*[> ename  rank
> ------ ----
> MILLER 9
> ADAMS  12
> JAMES  13
> SMITH  14
>
> val it : {ename:string, rank:int} list]*)
```

The main reason to apply `order` and `unorder` in a query is
to control the target collection type. But there is a more subtle
reason which relates to performance. The ordered and unordered
versions of the relational operators may produce the same results
(modulo ordering) but ordered execution may be less efficient (such
as running with reduced parallelism). If a query contains an `order`
or `unorder`, the order of the input to that step is irrelevant, and
the optimizer can use a more efficient execution plan.

This, by the way, is why the specification of the `order` step does
not guarantee stability. If `order` was stable, the optimizer would
have to use ordered execution of upstream steps if the sort key is
not exhaustive.

If you want `order` to be stable, you can add `ordinal` to the
trailing edge of the sort key:

```sml
from e in scott.emps
  order DESC e.sal
  where e.deptno <> 20
  yield {e.ename, e.job, e.sal}
  order (e.job, ordinal);
(*[> val it =
>   [{ename="MILLER",job="CLERK",sal=1300.0},
>    {ename="JAMES",job="CLERK",sal=950.0},
>    {ename="BLAKE",job="MANAGER",sal=2850.0},
>    {ename="CLARK",job="MANAGER",sal=2450.0},
>    {ename="KING",job="PRESIDENT",sal=5000.0},
>    {ename="ALLEN",job="SALESMAN",sal=1600.0},
>    {ename="TURNER",job="SALESMAN",sal=1500.0},
>    {ename="WARD",job="SALESMAN",sal=1250.0},
>    {ename="MARTIN",job="SALESMAN",sal=1250.0}]
>   : {ename:string, job:string, sal:real} list]*)
```

Materializing `ordinal` as a 1-based, contiguous sequence of integers
is expensive because it forces sequential execution, and the
optimizer will avoid this if it can. In this case, because `ordinal`
is used for sorting but is not returned, the optimizer downgrades
`ordinal` to a virtual expression. The plan might use an ordered
implementation of the `where` and `yield` steps followed by a stable
sort, or it might replace `ordinal` with the previous sort key
(`DESC e.sal`). 

## Ordered relational operators

We need to define the semantics of the relational operators
over all types of collection.

Part of the job has been done already:
* The relational model defines the semantics of operators over
  **sets** (unordered collections without duplicates).
* The SQL standard specifies the relational operators
  over **tables** (unordered collections with duplicates).
* Previous versions of Morel defined semantics for (and implemented)
  relational operators over **multisets** (unordered collections with
  duplicates).  While the collection type was at the time called
  `list`, we were actually defining the current `bag` type.  Unlike
  SQL, elements need not be records.

What remains is to define the semantics of queries over **lists**
(ordered collections with duplicates), and for hybrid queries that
combine lists and multisets. (We define hybrid semantics in the [next
section](#hybrid-relational-operators).)

Because a
[query](https://github.com/hydromatic/morel/blob/main/docs/query.md)
consists of a sequence of steps, each corresponding to a relational
operator, we define the semantics of each step over input that is a
`list`:

* The first step in a query -- <code>from <i>pat</i> in
  <i>exp</i></code>, <code>forall <i>pat</i> in <i>exp</i></code>, or
  <code>exists <i>pat</i> in <i>exp</i></code> -- returns elements in
  the same order that they are emitted from <i>exp</i>.
* <code>join <i>pat</i> in <i>exp</i> [ on <i>condition</i> ]</code>
  for each element from its input evaluates <i>exp</i>, then, in order
  of those elements, emits a record consisting of fields of the two
  elements, skipping records where <i>condition</i> is false.
* If a `from`, `forall`, `exists` or `join` step has more than one
  scan, each subsequent scan behaves as if it were a separate `join`
  step.
* <code>yield <i>exp</i></code> preserves order. 
* <code>where <i>condition</i></code> preserves order, dropping rows
  for which <i>condition</i> is false.
* <code>skip <i>count</i></code> and <code>take <i>count</i></code>
  preserve order (respectively dropping the first <i>count</i> rows,
  or taking the first <i>count</i> rows).
* `distinct` preserves order, emitting only the first occurrence
  of each element.
* <code>group <i>groupKey<sub>1</sub></i>, ...,
  <i>groupKey<sub>g</sub></i> [ compute <i>agg<sub>1</sub></i>, ...,
  <i>agg<sub>a</sub></i> ]</code> preserves order, emitting groups in
  the order that the first element in the group was seen; each
  aggregate function <code><i>agg<sub>i</sub></i></code> is invoked
  with a list of the input elements that belong to that group, in
  arrival order.
* <code>compute <i>agg<sub>1</sub></i>, ...,
  <i>agg<sub>a</sub></i></code> behaves as a `group` step where all
  input elements are in the same group.
* <code>union [ distinct ] <i>exp<sub>1</sub></i>, ...,
  <i>exp<sub>n</sub></i></code> outputs the elements of the input in
  order, followed by the elements of each <i>exp<sub>i</sub></i>
  argument in order (just like the UNIX `cat` command). If `distinct`
  is specified, outputs only the first occurrence of each element.
* <code>intersect [ distinct ] <i>exp<sub>1</sub></i>, ...,
  <i>exp<sub>n</sub></i></code> outputs the elements of the input in
  order, provided that every <i>exp<sub>i</sub></i> argument contains
  at least the number of occurrences of this element so far.  If
  `distinct` is specified, outputs only the first occurrence of each
  element.
* <code>except [ distinct ] <i>exp<sub>1</sub></i>, ...,
  <i>exp<sub>n</sub></i></code> outputs the elements of the input in
  order, provided that the number of occurrences of that element so
  far is less than the number of occurrences of that element in all
  the <i>exp<sub>i</sub></i> arguments.  If `distinct` is specified,
  outputs only the first occurrence of each element.
* <code>require <i>condition</i></code> (which can only occur in a
  `forall` query) has the same behavior as the unordered case.
* `order` and `unorder`, as discussed earlier, have the same
  semantics as in the unordered case.

The rules for `from` and `join` produce the same familiar ordering as
a nested "for" loop in a language such as C, Python or Java:

```sml
from hundreds in [100, 200, 300],
    tens in [10, 20, 30]
  join units in [1, 2, 3]
  yield hundreds + tens + units;
(*[> val it =
>   [111,112,113,121,122,123,131,132,133,211,212,213,221,222,
>    223,231,232,233,311,312,313,321,322,323,331,332,333]]*)
```

The rules for `union`, `intersect` and `except` are rather subtle, and
are best illustrated by example:

```sml
from i in [3, 1, 4, 1, 5, 9, 2, 6]
  union [2, 7, 1, 8, 2, 8, 1, 8];
(*[> val it = [3,1,4,1,5,9,2,6,2,7,1,8,2,8,1,8] : int list]*)
from i in [3, 1, 4, 1, 5, 9, 2, 6]
  intersect [2, 7, 1, 8, 2, 8, 1, 8];
(*[> val it = [1,1,2] : int list]*)
from i in [3, 1, 4, 1, 5, 9, 2, 6]
  except [2, 7, 1, 8, 2, 8, 1, 8];
(*[> val it = [3,4,5,6,9] : int list]*)
```

## Hybrid relational operators

We have specified the behavior of queries where input collections are
all lists or all bags. But what if a query has a mix of list and bag
inputs?

The mixing can occur if the first step of the query (`from`, `exists`,
or `forall`) has more than one scan, or in steps that introduce
another collection (`join`, `union`, `intersect`, or `except`). In all
cases, unordered wins: if any input is a `bag`, the step becomes
unordered, and unordered semantics apply from then on.

For example, if we join a `list` of department numbers (ordered) to a
table of employees (unordered), selecting only the clerks and
managers, the result is a `bag` (unordered):

```sml
from deptno in [10, 20, 30]
  join e in scott.emps on e.deptno = deptno
  where e.job elem ["CLERK", "MANAGER"]
  yield {deptno, e.ename};

(*[> deptno ename
> ------ ------
> 30     JAMES
> 10     CLARK
> 20     ADAMS
> 10     MILLER
> 20     SMITH
> 30     BLAKE
> 20     JONES
>
> val it : {deptno:int, ename:string} bag]*)
```

## Type inference challenges

This feature was challenging to implement because it required
major changes to Morel's type inference algorithm. (We mention this
only in the spirit of sharing war-stories, and for the interest of
those who understand the internal workings of Morel's compiler.
Hopefully, the changes to type-inference algorithm will be invisible
to the casual user.)

The problem is evident in a program such as

```sml
let
  fun f (xs, ys) =
    from i in xs
      intersect ys
in
  f ((from e in scott.emps yield e.empno), [7521, 7782, 8000])
end;
(*[> val it = [7521,7782] : int bag]*)
```

While resolving the type of function `f` and its embedded query, the
types of the arguments `xs` and `ys` have not yet been
determined. Morel's previous type inference algorithm allowed us to
say "<i>`xs` and `ys` must be lists with the same element type</i>" or
"<i>`xs` and `ys` must be bags with the same element type</i>". It was
based on
[Hindley-Milner's Algorithm W](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_W)
and unification, which basically means finding an assignment of
logical variables so that two trees are structurally identical.

But the type inference rules for queries with a mixture of ordered and
unordered collections require conditions that contain the word
'or'. For example, resolving the `intersect` expression above requires
that we say "<i>we can allow `xs` and `ys` to both be bags, or both be
lists, or one to be a bag and the other a list, but they must have
same element type</i>".  Furthermore, we need to derive the result
type, saying "<i>the result of the query is a list if both arguments
are lists, otherwise a bag, with the same element type as the
arguments</i>".

We needed a system where we can place a number of constraints on type
variables, and then solve for those constraints. The new type
inference algorithm extends Hindley-Milner with constraints, using the
approach described in
["A Second Look at Overloading" by Odersky, Wadler & Wehr (1995)](https://dl.acm.org/doi/pdf/10.1145/224164.224195).
As the title of that paper suggests, we have [added a kind of
overloading](https://github.com/hydromatic/morel/issues/237) to Morel;
it is as if the `intersect` operator now has four forms:

 * <code>intersect: &alpha; bag * &alpha; bag &rarr; &alpha; bag</code>
 * <code>intersect: &alpha; bag * &alpha; list &rarr; &alpha; bag</code>
 * <code>intersect: &alpha; list * &alpha; bag &rarr; &alpha; bag</code>
 * <code>intersect: &alpha; list * &alpha; list &rarr; &alpha; list</code>

(and similar overloads for the other relational operators) and the
type inference algorithm solves the constraints to land on one valid
assignment of types.

The algorithm took several months of hard work to implement, but the
results are pleasing.  Morel retains the key benefits of a
Hindley-Milner type system: strong static typing, runtime efficiency,
and type inference without the need for type annotations.

Like any other major change in architecture, constraint-based type
inference will take a while to mature;
[[MOREL-270](https://github.com/hydromatic/morel/issues/270)]
and
[[MOREL-271](https://github.com/hydromatic/morel/issues/271)]
describe some of the remaining issues.

## Conclusion

The ability to combine ordered and unordered data sets, and process
both using relational operators, is a major new feature in Morel. It
allows Morel to handle, with equal ease, data from files and
relational databases, and data that is generated programmatically.

This feature will be available in Morel release 0.7.

To find out more about Morel, read about its
[goals]({% post_url 2020-02-25-morel-a-functional-language-for-data %})
and [basic language]({% post_url 2020-03-03-morel-basics %}), peruse the
[query reference](https://github.com/hydromatic/morel/blob/main/docs/query.md)
or
[language reference](https://github.com/hydromatic/morel/blob/main/docs/reference.md),
or download it from [GitHub](https://github.com/hydromatic/morel/) and
give it a try.

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
