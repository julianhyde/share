---
layout: post
title:  "Morel release 0.8.0"
date:   2025-11-23 12:00:00 -0800
author: Julian Hyde
image:  /assets/img/OldDesignShop_MushroomSpringMorel-240x240.jpg
tweet:  https://x.com/julianhyde/status/XXXXXXXXXXXXXXXXX
published: false
---

I am pleased to announce Morel
[release 0.8.0](https://github.com/hydromatic/morel/blob/main/HISTORY.md#080--2025-11-23),
five months after
[release 0.7.0](https://github.com/hydromatic/morel/blob/main/HISTORY.md#070--2025-06-07).

This release has improvements to aggregate query syntax, the type
system, and the standard library.

The
[syntax of aggregate queries](#1-aggregate-query-syntax)
is now more powerful and consistent. You can compute expressions before
and after aggregation, for example `2.0 * avg over (units * unitPrice)`.
The new `elements` collection lets you access the raw elements of a
group and even write subqueries in the `compute` clause. (*Breaking
change:* The `of` keyword has been replaced by `over`, and composite
keys and compute expressions must now be records with the usual `{`
... `}` syntax.)

The type system includes
[type aliases](#2-type-system)
via the `type` keyword. The `typeof` operator lets you extract an
expression's type.

The built-in library adds the
[`Either`](#3-either-structure),
[`Fn`](#4-fn-structure), and
[`ListPair`](#5-listpair-structure)
structures.

Let's explore the key features. For complete details, see the
[official release notes](https://github.com/hydromatic/morel/blob/main/HISTORY.md#080--2025-11-23).

## 1. Aggregate query syntax

The biggest change in 0.8.0 is the
[improved syntax for `group` and `compute` steps](https://github.com/hydromatic/morel/issues/288).
Following the same spirit as the simplification of the `order` step in
release 0.7.0, we now use record syntax for composite keys and compute
expressions.

### Simplified syntax

Previously, when you wanted to group by multiple fields or compute
multiple aggregates, the syntax mixed special constructs with
expressions. Now everything uses standard expression syntax.

Here's a composite key example:

```sml
(* Old syntax *)
from e in scott.emps
  group odd = e.empno mod 2 = 1, e.deptno
  compute count, avgSal = avg of e.sal;

(* New syntax *)
from e in scott.emps
  group {odd = e.empno mod 2 = 1, e.deptno}
  compute {count over (), avgSal = avg over e.sal};
```

The `of` keyword is replaced with `over` because what comes after `=`
is now syntactically an expression, not a special aggregate construct.

For a singleton key, you can still use the simple syntax:

```sml
from e in scott.emps
  group e.deptno
  compute count over ();
```

But you can also use record syntax if you prefer:

```sml
from e in scott.emps
  group {e.deptno}
  compute {count over ()};
```

### Empty keys

The change for empty keys is breaking but resolves an ambiguity. A
`group` step with a `compute` clause previously looked similar to a
`group` without a `compute` clause followed by a separate `compute`
step. Now you must be explicit:

```sml
(* Old syntax *)
from e in scott.emps
  compute count, avgSal = avg of e.sal;

(* New syntax *)
from e in scott.emps
  group {}
  compute {count over (), avgSal = avg over e.sal};
```

### The elements collection

The new
[`elements` collection](https://github.com/hydromatic/morel/issues/304)
is available in the `compute` clause and gives you access to the raw
elements of a group. This enables advanced aggregation patterns,
including subqueries within aggregations.

With `elements`, you can now write queries that compute statistics over
filtered subsets of a group:

```sml
from e in scott.emps
  group e.deptno
  compute {
    totalCount = count over (),
    highSalCount = count over (from x in elements where x.sal > 2000.0),
    avgSal = avg over (from x in elements yield x.sal)
  };
```

The `elements` collection represents all the rows in the current group,
allowing you to perform complex aggregations that were difficult or
impossible before. You can even write subqueries that reference
`elements` to transform or filter the group's data before aggregating.

## 2. Type system

This release adds two important features to Morel's type system: type
aliases and the `typeof` operator.

### Type aliases

Morel now supports
[type aliases](https://github.com/hydromatic/morel/issues/285)
(also known as type abbreviations) using the `type` keyword, following
the Standard ML specification.

Type aliases let you give meaningful names to complex types:

```sml
type intPair = int * int;
(*[> type intPair = int * int]*)

val x : intPair = (4, 5);
(*[> val x = (4,5) : intPair]*)

val y : int * int = x;
(*[> val y = (4,5) : int * int]*)

val z : intPair = y;
(*[> val z = (4,5) : intPair]*)
```

Types are preserved through simple assignments:

```sml
type myInt = int;
(*[> type myInt = int]*)

val i : myInt = 1;
(*[> val i = 1 : myInt]*)

val j = i;
(*[> val j = 1 : myInt]*)

val l = [i];
(*[> val l = [1] : myInt list]*)
```

This makes your code more readable and self-documenting, especially
when working with complex record types like those representing database
tables.

### The typeof operator

The new
[`typeof` operator](https://github.com/hydromatic/morel/issues/291)
allows you to extract the type of an expression at compile time. This
is particularly useful when working with tables, where record types can
be verbose.

For example, defining a predicate that works on the employees table
used to require writing out the entire type:

```sml
fun isManager (e: {comm:real, deptno:int, empno:int, ename:string,
                   hiredate:string, job:string, mgr:int, sal:real}) =
  e.job = "MANAGER";
```

With `typeof` you can extract the type from a table:

```sml
type Emp = typeof (Bag.hd scott.emps);
fun isManager (e: Emp) = e.job = "MANAGER";
```

Or use an anonymous type:

```sml
fun isManager (e: typeof (Bag.hd scott.emps)) = e.job = "MANAGER";
```

The expression is not evaluated, so `typeof (Bag.hd scott.emps)` will
never throw even if `scott.emps` is empty.

## 3. Either structure

The
[`Either` structure](https://github.com/hydromatic/morel/issues/302)
provides a standard way to represent values that can be one of two
types. This is useful for error handling and for functions that can
return different types of results.

The structure is based on a
[proposed addition to the Standard ML Basis Library](https://github.com/SMLFamily/BasisLibrary/wiki/2015-002-Addition-of-Either-module)
and defines the `either` datatype:

```sml
datatype ('a, 'b) either = INL of 'a | INR of 'b;
```

The `Either` structure is particularly useful when you need to work
with heterogeneous collections or represent computations that can
produce different types of results:

```sml
(* Partition a list of either values into two separate lists *)
Either.partition [INL 1, INR "a", INL 2, INR "b"];
(*[> val it = ([1,2],["a","b"]) : int list * string list]*)

(* Check which variant an either value is *)
Either.isLeft (INL 42);
(*[> val it = true : bool]*)

Either.isRight (INR "hello");
(*[> val it = true : bool]*)

(* Extract values as options *)
Either.asLeft (INL 42);
(*[> val it = SOME 42 : int option]*)

Either.asRight (INL 42);
(*[> val it = NONE : 'a option]*)
```

The structure includes functions like `isLeft`, `isRight`, `asLeft`,
`asRight`, `map`, `mapLeft`, `mapRight`, `app`, `appLeft`, `appRight`,
`fold`, `proj`, and `partition`.

## 4. Fn structure

The
[`Fn` structure](https://github.com/hydromatic/morel/issues/301)
provides utility functions for function composition and manipulation,
based on Andreas Rossberg's
[proposed addition to the Standard ML Basis Library](https://github.com/SMLFamily/BasisLibrary/wiki/2015-005-Addition-of-Fn-module).

This structure is particularly valuable for functional programming
patterns:

```sml
(* Identity function *)
Fn.id 42;
(*[> val it = 42 : int]*)

(* Constant function - always returns the same value *)
val alwaysFive = Fn.const 5;
Fn.apply (alwaysFive, "ignored");
(*[> val it = 5 : int]*)

(* Function composition using o operator *)
val addOne = fn x => x + 1;
val double = fn x => x * 2;
val addOneThenDouble = double o addOne;
addOneThenDouble 5;
(*[> val it = 12 : int]*)

(* Curry and uncurry *)
val add = fn (x, y) => x + y;
val curriedAdd = Fn.curry add;
curriedAdd 3 4;
(*[> val it = 7 : int]*)

(* Flip argument order *)
val divide = fn (x, y) => x div y;
val divideFlipped = Fn.flip divide;
divideFlipped (2, 10);
(*[> val it = 5 : int]*)
```

Other functions include `repeat` (apply a function n times), `equal`,
and `notEqual`.

## 5. ListPair structure

The
[`ListPair` structure](https://github.com/hydromatic/morel/issues/295)
follows the
[Standard ML Basis Library specification](https://smlfamily.github.io/Basis/listpair.html)
and provides functions for working with pairs of lists in parallel.

This is particularly useful when you need to process two related lists
together:

```sml
(* Zip two lists together *)
ListPair.zip ([1, 2, 3], ["a", "b", "c"]);
(*[> val it = [(1,"a"),(2,"b"),(3,"c")] : (int * string) list]*)

(* Unzip back to two lists *)
ListPair.unzip [(1, "a"), (2, "b"), (3, "c")];
(*[> val it = ([1,2,3],["a","b","c"]) : int list * string list]*)

(* Map over two lists in parallel *)
ListPair.map (fn (x, y) => x + y) ([1, 2, 3], [4, 5, 6]);
(*[> val it = [5,7,9] : int list]*)

(* Check if all pairs satisfy a predicate *)
ListPair.all (fn (x, y) => x < y) ([1, 2, 3], [2, 3, 4]);
(*[> val it = true : bool]*)

(* Fold over two lists *)
ListPair.foldl (fn (x, y, acc) => x + y + acc) 0 ([1, 2, 3], [4, 5, 6]);
(*[> val it = 21 : int]*)
```

The structure includes both length-agnostic functions (`zip`, `map`,
`app`, `all`, `exists`, `foldr`, `foldl`) that work when lists have
different lengths, and strict functions (`zipEq`, `mapEq`, `appEq`,
`foldrEq`, `foldlEq`, `allEq`) that raise `UnequalLengths` if the
lists differ in length.

## 6. Operator sections

Morel now supports the
[`op` keyword](https://github.com/hydromatic/morel/issues/311)
for operator sections, allowing you to use operators as first-class
functions.

```sml
(* Use + as a function *)
val add = op +;
(*[> val add = fn : int * int -> int]*)

add (3, 4);
(*[> val it = 7 : int]*)

(* Useful with higher-order functions *)
List.foldl (op +) 0 [1, 2, 3, 4, 5];
(*[> val it = 15 : int]*)
```

## 7. Signatures

Morel can now
[parse `signature` declarations](https://github.com/hydromatic/morel/issues/315),
a step toward full support for Standard ML's module system. Signatures
allow you to specify the interface that a structure must implement.

```sml
signature ORDERED =
sig
  type t
  val compare : t * t -> int
end;
```

While full signature support is still under development, this lays the
groundwork for more sophisticated module systems in future releases.

## 8. Breaking changes

This release includes some breaking changes to be aware of.

### Aggregate query syntax

As mentioned earlier, the `of` keyword has been replaced by `over` in
aggregate functions:

```sml
(* Old syntax *)
from e in scott.emps
  group e.deptno compute sumSal = sum of e.sal;

(* New syntax *)
from e in scott.emps
  group e.deptno compute sumSal = sum over e.sal;
```

Additionally, composite keys and compute expressions must now be
records:

```sml
(* Old syntax *)
from e in scott.emps
  group e.deptno, e.job compute count;

(* New syntax *)
from e in scott.emps
  group {deptno = e.deptno, job = e.job} compute {count over ()};
```

### Set operators

The `intersect` and `except` steps now properly
[count and preserve order](https://github.com/hydromatic/morel/issues/321)
when working with ordered collections.

## Conclusion

Release 0.8.0 continues Morel's evolution as both a powerful query
language and a solid implementation of Standard ML. The improvements to
aggregate syntax make complex queries more natural to express, while
the additions to the type system and standard library strengthen
Morel's capabilities as a general-purpose functional programming
language.

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

<!--
This article
[has been updated](https://github.com/julianhyde/share/commits/main/blog/{{ page.path }}).
-->
