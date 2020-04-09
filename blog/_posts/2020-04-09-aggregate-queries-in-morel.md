---
layout: post
title:  "Aggregate queries in Morel"
date:   2020-04-09 12:00:00 -0800
author: Julian Hyde
image:  /assets/img/OldDesignShop_MushroomSpringMorel-240x240.jpg
tweet:  https://twitter.com/julianhyde/status/1248393932129497088
---

Last week I wrote about
[solving the WordCount problem in Morel](/2020/03/31/word-count-revisited.html).
The solution used a user-defined function, `split`,
unnested the resulting array of words into a relation, and then used
the `group` operator to count the occurrences of each word.

In this post, I want to describe in detail how that `group`
operator works, and the strategy that went into it.

I think that what we came up with is elegant, powerful and concise,
and I hope that you will agree.  But getting the right design wasn't
straightforward.  To see why, we'll take a quick tour through the
history of aggregate functions in databases and functional programming
languages.

# Aggregate functions and relational algebra

When Tedd Codd
[introduced the relational algebra in 1970](https://github.com/dmvaldman/library/blob/master/computer%20science/Codd%20-%20A%20Relational%20Model%20of%20Data%20for%20Large%20Shared%20Data%20Banks.pdf),
there was no support for aggregation or aggregate functions. The
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
column from the `Emp` relation, you don't get 5 records for
department 10 (one for each employee), you get just one. If you try to
compute the average salary for employees in each department, the most
natural formulation would compute the set of salary values in
department 10, and therefore if two employees have the same salary
value, they would be counted only once.

The commercial systems Klug referred to, System R and INGRES, had
fewer problems because they were based on multisets (bags) rather than
sets. Still, they arrived at an uneasy truce. SQL's `GROUP BY` works
by sleight of hand, simultaneously performing a project (of the key
columns) and aggregation. The bag of values being passed to the
aggregate function exists only fleetingly; if you're squeamish, it's
best to look away.

Shoe-horning aggregate behavior into the existing `SELECT` expression
was another compromise, and it left its mark on SQL's
semantics. Consider the following statement:

```sql
SELECT deptno, age, SUM(salary), MIN(age)
FROM Emp
GROUP BY deptno
```

There are two references to the `age` column. The first reference is
illegal (because `age` is not a `GROUP BY` key), but the second
reference (inside the `MIN` function) is legal. The semantic context,
determining what columns are available, is very different if you are
inside or outside an aggregate function, and both of those contexts
exist in just the first line of that query.

SQL had to invent a new clause, `HAVING`, that does the same as
`WHERE` but in the post-aggregation context. (As we shall see, Morel
does not have that problem. The semantic rules after a `group` are the
same as before it, so you can intermix `group` and `where`
[any way you like](https://github.com/julianhyde/morel/issues/21).)

Aggregate functions create other semantic problems. Assuming that the
`Emp` table contains 100 rows, how many rows does the following
statement return?

```sql
SELECT foo(age)
FROM Emp
```

Unless you know whether `foo` is an aggregate function, it's
impossible to say.  If `foo` is an aggregate function, the query
returns 1 row (implicitly totaling all `Emp` records), but if `foo` is
an ordinary scalar function the query returns 100 rows.

The underlying problem is that aggregate functions in SQL have the
same syntax as scalar functions, but very different semantics. This
makes the language fragile, and makes a query difficult to understand
unless you are familiar with all of the library functions that it is
using.

# Nested collections

Having incorporated aggregate functions into relational algebra, by
the early 1990s, the database research community was turning its
attention to another sacred cow: nested collections, formally known as
[Non First Normal Form (NF<sup>2</sup>) relations](https://dl.acm.org/doi/10.1145/588111.588133).

Nested collections are a major extension to the relational model
(though purists would say a corruption) that allow rich
representations of data, but we are interested in them here because
they allow a new approach to aggregation.  With nested collections,
you can aggregate in two steps: first create a set (or multiset) of
rows that share the same key, then apply a function to collapse those
rows to a single value.

As we saw in
[our discussion of WordCount](/2020/03/31/word-count-revisited.html#wordcount-in-pig),
one of the languages that embraced nested collections is
[Apache Pig](https://pig.apache.org).

Consider the following aggregate query in Pig Latin:

```
emps = LOAD '/data/emps' using PigStorage()
  as (empno: int, name: chararray, deptno: int, salary: int);
by_deptno = GROUP emps BY deptno;
dept_stats = FOREACH by_deptno
  GENERATE group as deptno, COUNT(emps), AVG(emps.salary);
```

The `by_deptno` relation is the intermediate step, after `GROUP` has
created a collection for each `deptno` value, and before `GENERATE`
has applied `COUNT` and `AVG` aggregate functions to collapse the
collections into scalar values. `by_deptno` looks like this:

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

# Functional languages: foldl

In Standard ML, the
[language from which Morel is derived](/2020/03/03/morel-basics.html),
the [`List` structure](http://sml-family.org/Basis/list.html) has a
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

To recap, we have seen how grouping and aggregation are implemented in
three languages: SQL, Pig and Standard ML.  SQL calculates aggregates
as it groups, whereas Pig forms collections first. Pig's aggregate
functions work on entire collections, whereas ML's `foldl`
higher-order function reduces collections by incrementally adding to
an accumulator.

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

Yes, this is complicated! And this variant only allows one aggregate function.

But Morel provides syntactic sugar so that `group` is simple to use in
practice, while retaining strongly typing and type inference. For example:

```sml
- from e in emps
    group e.deptno compute sumEmpno = sum of e.empno;
val it =
  [{deptno=20,sumEmpno=38501},{deptno=10,sumEmpno=23555},
   {deptno=30,sumEmpno=46116}] : {deptno:int, sumEmpno:int} list
```

(Examples in this post use the new `=` syntax for renaming group keys
and aggregate functions that was introduced in
[[MOREL-24](https://github.com/julianhyde/morel/issues/24)] but has
not yet been released, rather than the old `as` syntax used in
morel-0.2. Also, some examples ignore the fact that
[`sum` cannot yet be applied to `real` values](https://github.com/julianhyde/morel/issues/28).)

The key extractor (`e.deptno`) and argument extractor (`e.sal`) are
not functions but expressions that are evaluated in the environment of
the current row (the variable `e` holds current member of the `emps`
relation). Expressions are as powerful as functions but are more
concise.

The aggregate function is a function -- or, more precisely, an
expression that yields a function. In this case, we use the constant
`sum`, which is a built-in function value of type `int list -> int`.

Here is another example:

```sml
- from e in emps,
      d in depts
    where e.deptno = d.deptno
    group e.deptno, d.dname, e.job
      compute sumSal = sum of e.sal,
        minRemuneration = min of e.sal + e.commission;
```

Several things are more advanced than the previous example. The key is
composite, there is more than one aggregate function, and the argument
to the aggregate function may be a complex expression. Also, the input
is a join, so there are two variables (`e` and `d`) available for use
in expressions.

The output format is more straightforward than Pig. Pig uses a field
called `group` for keys, which is a record if the key is
composite. Morel just uses the input field names (and allows you to
rename fields using `=`). In this case, Pig's output record is
`{group = {deptno, dname}, sumSal, minRemuneration}`, and Morel's is
`{deptno, dname, job, sumSal, minRemuneration}`.

Pig's intermediate format (after `GROUP` and before `FOREACH`) would
have a list-valued field called `emps`, whereas Morel's intermediate
list is seen only by the argument extractor, and does not need to be
named for the user's benefit.

The output of `group` is simply an iteration context with a number of
variables available (`deptno`, `dname`, `job`, `sumSal`,
`minRemuneration`). That's what you'd expect whenever you are inside a
`from` expression. You can therefore follow `group` with any clause
allowable in `from` -- `where`, `order` or `group` -- or terminate the
`from` expression with a `yield` clause.

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
      compute sumEmpno = my_sum of e.empno
  end;
val it =
  [{deptno=20,sumEmpno=38501},{deptno=10,sumEmpno=23555},
   {deptno=30,sumEmpno=46116}] : {deptno:int, sumEmpno:int} list
```

Aggregate functions are invoked on a collection of values formed by
applying their argument expression to all of the records in the
current group.  SQL's
[COLLECT](https://docs.oracle.com/cd/B28359_01/server.111/b28286/functions024.htm#SQLRF06304)
aggregate function, which creates a collection of its arguments, is
therefore trivial in Morel: we just use the identity operator (`fn x
=> x`) as the aggregate function, and it returns its argument:

```sml
- from e in emps
    group e.deptno
    compute names = (fn x => x) of e.ename;
val it =
  [{deptno=20,names=["SMITH","JONES","SCOTT","ADAMS","FORD"]},
   {deptno=10,names=["CLARK","KING","MILLER"]},
   {deptno=30,names=["ALLEN","WARD","MARTIN","BLAKE","TURNER","JAMES"]}]
  : {deptno:int, names:string list} list
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

# Summary

Morel's `group` operator is elegant and powerful. The syntax is simple
and concise when used with built-in aggregate functions, but you can
easily write user-defined aggregate functions.

Aggregate functions behave as if they are acting on collections, but
in practice they can frequently be computed more efficiently, using
accumulators or by composing sub-totals.

Unlike the complicated semantics of aggregation in SQL, `group`
composes easily with other Morel relational operators such as `where`
and `order`.

If you have comments, please reply on Twitter:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>
