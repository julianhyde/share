---
layout: post
title:  "Aggregate functions"
date:   2020-03-31 12:00:00 -0800
author: Julian Hyde
image:  /assets/img/OldDesignShop_MushroomSpringMorel-240x240.jpg
tweet:  https://twitter.com/julianhyde/status/todo
---

Last week I wrote about solving the
[WordCount](http://research.google.com/archive/mapreduce-osdi04-slides/index-auto-0004.html)
problem in Morel.  The solution used a user-defined function, `split`,
unnested the resulting array of words into a relation, and then used
the `group` operator to count the occurrences of each word.

But getting the right design for the `group` operator wasn't
straightforward.  To see why, let's let's take a quick tour through
the history of aggregate functions in databases.

# Aggregate functions and relational algebra

When Tedd Codd [introduced the relational algebra in 1970](https://github.com/dmvaldman/library/blob/master/computer%20science/Codd%20-%20A%20Relational%20Model%20of%20Data%20for%20Large%20Shared%20Data%20Banks.pdf),
there was no support for aggregation or aggregate functions.  The
operators were project, select, join and semijoin (called
'restriction' in Codd's paper).

It was not until 1982 that Anthony Klug
[added aggregates to relational algebra](https://dl.acm.org/doi/pdf/10.1145/322326.322332).
He remarked

> Previous treatments of aggregate functions in relational languages
> have not been general and have not been well defined. Two examples
> are System R and INGRES.  The formulations of aggregate functions in
> these systems do not apply to more general languages, for example,
> to languages having explicit quantifiers.  Their definitions of
> aggregate functions also rely unnecessarily on "sets" of tuples
> having duplicate members (a contradiction).

Aggregation does not fit easily into relational algebra because
relational algebra works in terms of sets. If you project the `deptno`
column from the `Emps` relation, you do not get 5 records for
department 10 (one for each employee) but just one. If you try to
compute the average salary for employees in each department, the most
natural formulation would compute the set of salary values in
department 10, and therefore if two employees have the same salary
value they would be counted only once.

The commercial systems (System R and INGRES) had fewer problems
because they were based on multisets (bags) rather than sets. Still,
they arrived at an uneasy truce. SQL's `GROUP BY` operation performs a
project (of the key columns) and aggregation simultaneously. The bag
of values being passed to the aggregate function exist only
fleetingly; if you're sqeamish, it's best to look away.

# Nested collections

By the early 1990s, the database research community was turning its
attention to another sacred cow: nested collections.

With nested collections, you can aggregate in two steps: first create
a set (or multiset) of rows that share the same key, then apply a
function to collapse those rows to a single value.

This is how it works in Pig Latin:
```
emps = LOAD '/data/emps' using PigStorage()
  as (empno: int, name: chararray, deptno: int, salary: int);
by_deptno = GROUP emps BY deptno;
dept_stats = FOREACH by_deptno
  GENERATE group as deptno, COUNT(emps), AVG(emps.salary);
```

The `by_deptno` relation looks like this:

| group | emps
| ----- | -----
| 10    | {[100, 'Shaggy', 10, 1500], [120, 'Scooby', 10, 1250]}
| 20    | {[110, 'Velma', 20, 2000]}
| 30    | {[130, 'Fred', 30, 1700]}, {[140, 'Daphne', 30, 1700]}

The two-stage process makes it easy to write powerful aggregate
functions. For example, to implement `MEDIAN`, you need to look at all
of the values, sort them, and take the value that occurs in the
middle.

But many other aggregate functions, including `SUM`, `COUNT`, `MIN`
and `MAX`, can be computed iteratively, adding each row to a small
accumulator; materializing the whole multiset is a waste of memory and
effort.

The accumulator approach is how aggregate functions are usually
implemented in functional programming languages.

# Functional languages: foldl and foldr

In Standard ML, the language from which Morel is derived, the
[`List` structure](http://sml-family.org/Basis/list.html) has a
higher-order function `foldl` (meaning 'fold left'). `foldl` starts
with an initial accumulator value, then uses a "combiner" function
supplied by the caller to combine the initial value with the first
element of the list to form a new accumulator value, then combines
that accumulator value with the second element of the list, and so on.

(There is also a `foldr` function that starts at the end of the list
and works forward.)

Thus `sum` is defined as follows:

```ml
- val sum = foldl (fn (x, y) => x + y) 0;
val sum = fn : int list -> int
```

Here is `sum` applied to a list:
```ml
- sum [1, 2, 3, 5, 8, 13];
val it = 32 : int
```

# Choices, choices...

In Morel, we had to choose whether our `group` operation would
generate lists (like Pig) that would be reduced in a subsequent step,
or whether it would apply aggregate functions. And if it applied
aggregate functions, would these be defined using an accumulator (like
`foldl`) or would we allow a more general mechanism?

Morel's aggregate functions proceed in three steps, all of which occur
within the `group` clause:
* First, Morel applies a *key extractor* expression to each row, and
  gathers rows together by key value;
* Second, for all of the rows in a group, Morel applies an *argument
  extractor* expression to get the argument to which the aggregate
  function will be applied, and collects the argument values into a
  list;
* Last, Morel applies the aggregate function to the list, emitting
  the result as a field in the output record.

In functional programming terms, it's difficult to express this
concisely. If `group` were a higher-order function, its signature
would look something like this:

```
- val group: ('r -> 'k) -> ('r -> 'a) -> ('a list -> 'b) -> ('k * 'b) list
```

where:
* <code>'r</code> is the input row
* <code>'k</code> is the key
* <code>'r -> 'k</code> is the key extractor function
* <code>'a</code> is the type of the argument to the aggregate function
* <code>'r -> 'a</code> is the argument extractor function
* <code>'b</code> is the result of the aggregate function
* <code>'a list -> 'b</code> is the aggregate function that converts a list of arguments to a result
* <code>('k * 'b) list</code> is the output, a list of (key, result) pairs

(And this is just the version for one aggregate function!)

But Morel provides a lot of syntactic sugar so that `group` is simple
to use in practice. For example:

```sml
from e in emps
  group e.deptno compute sumSal = sum of e.sal
```

(This example, and a few others, uses the new `=` syntax for renaming
group keys and aggregate functions that was introduced in
[[MOREL-24](https://github.com/julianhyde/morel/issues/24)], rather
than the old `as` syntax used in morel-0.2.)


The key extractor (`e.deptno`) and argument extractor (`e.sal`) are
not functions but expressions that are evaluated in the environment of
the current row (the variable `e` holds current member of the `emps`
relation). Expressions are as powerful as functions but are more
concise.

The aggregate function is a function - or, in fact, an expression that
yields a function. In this case, we use the constant `sum`, which is a
built-in function value of type `int list -> int`.

The following example illustrates several complexities:

```sml
from e in emps,
    d in depts
  where e.deptno = d.deptno
  group e.deptno, d.dname, e.job
    compute sumSal = sum of e.sal,
      minRemuneration = min of e.sal + e.commission
```

Several things are more complex than the previous example. The key is
composite, there is more than one aggregate function, and the argument
to the aggregate function may be a complex expression. Also, the input
is a join, so there are two variables (`e` and `d`) available for use
in expressions.

The output format is more straightforward than Pig. Pig uses a field
called `group` for keys, which is a record if the key is
composite. Morel just uses the input field names (and allows you to
rename fields using `as`). In this case, Pig's output record is
`{group = {deptno, dname}, sumSal, minRemuneration}`, and Morel's is
`{deptno, dname, job, sumSal, minRemuneration}`.

Pig's intermediate format (after `GROUP` and before `FOREACH`) would
have a list-valued field called `emps`, whereas Morel's intermediate
list is seen only by the argument extractor, and does not need to be
named for the user's benefit.

# User-defined aggregate functions

Morel's goal is to be a simple, concise query language which allows
you to escape into a Turing-complete programming language when you
need to.

So of course you can define your own aggregate functions inside a
query.  In this example, we define our own version of the `sum`
function:

```sml
- let
    fun my_sum [] = 0
      | my_sum (head :: tail) = head + (my_sum tail)
  in
    from e in emps
      group e.deptno
      compute sum_id = my_sum of e.id
  end;
val it =
  [{deptno=20,sum_id=101},{deptno=10,sum_id=100},{deptno=30,sum_id=205}]
  : {deptno:int, sum_id:int} list
```

# Computing aggregate functions efficiently

Not all aggregate functions need to operate on the full list of their
arguments.  It might seem like overkill to form the list of arguments
when not all functions need it.

But in designing Morel, we favor expressive power and concise,
readable syntax over efficiency. We think we can achieve efficiency
(in most cases) by recognizing expressions that can be rewritten to
something simpler.

In the case of aggregate functions, many functions have algebraic
properties that allow them to be computed more efficiently.  For
example, you can compute `sum` by dividing the rows into any subsets
you like, summing those subsets, and summing those sums, in any order
you like. For instance, `sum [1, 2, 3, 5, 8, 13]` is the same as `sum
[1, 3, 5, 13] + sum [2, 8]`, and therefore partitioning the input into
odd and even partitions would be a viable strategy.

In mathematical terms, `sum` is a
[commutative monoid](https://mathworld.wolfram.com/CommutativeMonoid.html).
In computational terms, that means that it is very easy to parallelize.

Morel will, at some point, allow you to declare the algebraic
properties of built-in and user-defined aggregate functions (such as
whether they are monoids). Then it will be able to choose more
efficient plans.

# Not used

In 1987, Özsoyoğlu, Özsoyoğlu and Matos proposed
[set-valued attributes and aggregregate functions](https://dl.acm.org/doi/abs/10.1145/32204.32219).


# Conclusion

If you have comments, please reply on Twitter:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>
