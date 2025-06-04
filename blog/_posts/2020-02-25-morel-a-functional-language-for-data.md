---
layout: post
title:  "Morel: A functional language for data"
date:   2020-02-25 23:24:00 -0800
author: Julian Hyde
image:  /assets/img/OldDesignShop_MushroomSpringMorel-240x240.jpg
tweet:  https://twitter.com/julianhyde/status/1232572869315444736
---

For the past few months, I have been working on an experimental
functional/data language called
[Morel](https://github.com/hydromatic/morel).

[![Morel mushroom](/assets/img/OldDesignShop_MushroomSpringMorel-180x275.jpg "Morel mushroom (credit: OldDesignShop.com)"){:style="float: right;margin: 10px;width: 180px;height: 275px;"}](https://olddesignshop.com/wp-content/uploads/2015/03/OldDesignShop_MushroomSpringMorel-670x1024.jpg)

SQL has several deficiences relating to nested collections,
higher-order functions and type system. After several months trying to
figure out how to add these features to SQL, I noticed that they were
basically the defining characteristics of a functional programming
language.

I figured, rather than fixing SQL, why not dig the tunnel from the
other end?  Start with a functional programming language, then add what
makes SQL a good query language.

Morel's design goals:

* Seamless integration of collections (relations) and relational
  operators into a functional programming language
* As concise as SQL
* Access external data
* Retain the computational power and sophisticated type system of the
  functional programming language
* Allow query planning via relational algebra, even in hybrid programs
  that are mixture of relational algebra
* Suitable for interactive queries from a REPL
  ([read-eval-print loop](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop))
  and also larger scale programs

# Conciseness

SQL is concise. Many useful queries are only a few lines long.

Functional programming languages also tend to be concise, for similar
reasons to SQL: they are strongly typed, and the language has good
type inference. Type inference means that you don't need to explicitly
specify types often, if ever.

When you write a query in Morel, you are writing a short expression in
a functional programming language, but its structure looks very
similar to the equivalent SQL query.

For example, here is a query in Morel:

```sml
from e in hr.emps,
    d in hr.depts
where e.deptno = d.deptno
yield {e.id, e.deptno, ename = e.name, dname = d.name};
```

The equivalent query in SQL looks very similar:

```sql
SELECT e.id, e.deptno, e.name AS ename, d.name AS dname
FROM hr.emps AS e,
    hr.depts AS d
WHERE e.deptno = d.deptno;
```

# External data

In the above example, the `hr` data structure looks like a record in
memory (with fields `emps` and `depts` that are collections of
records) but actually maps to a schema in a DBMS.

Virtualized data is essential to bringing large-scale data sets into
the programming model, and is implemented by calling out to
[Apache Calcite schema adapters](https://calcite.apache.org/docs/adapter.html).
I expect to make more use of Calcite for query planning.

# Interactivity

People tend to write SQL queries in a REPL
([read-eval-print loop](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop)).
Small functional programs can be written in the same way, so there is
a good fit there.

# A modest project

I chose to extend
[Standard ML](https://en.wikipedia.org/wiki/Standard_ML) because it is
a small, simple language, well known in the academic community.

This made it feasible for me to write a parser and interpreter as a
solo project. (By the way, the interpreter is written in Java, and is
quite a nice implementation of Standard ML for the JVM, even if you
don't care for the relational extensions.)

If the Morel experiment is successful, the ideas can be carried into
more complex and powerful languages such as Haskell and Scala.

# Relationally complete vs. Turing complete

Query languages are, by design, powerful but not too powerful.

One reason for this is that, if we add extra power (for example,
function values and arrays) then we have to add extra syntax for these
features. The extra syntax makes the language harder to use for simple
taskes, and also harder to learn.

More important, query languages rely on a query planner.  Many details
can be left out of the program (such as whether to use a hash-join or
sort-merge-join algorithm to perform a join) because the planner can
make these decisions for us.  But if we give the language too much
power, we make the planner's job difficult or impossible.

Why is this? As soon as a language has sufficient power -- if it can loop,
or call functions recursively -- it becomes
[Turing complete](https://en.wikipedia.org/wiki/Turing_completeness),
and not all programs in such a language can be reasoned about. (See,
for example, the
[Halting Problem](https://en.wikipedia.org/wiki/Halting_problem).)

SQL is not Turing complete (if you ignore the `WITH RECURSIVE`
construct), as evidenced by the fact that any query with finite input
relations eventually terminates. It is equivalent in power to
relational algebra and relational calculus, which Edgar F. Codd called
[relationally complete](https://en.wikipedia.org/wiki/Codd%27s_theorem).

Morel, on the other hand, crosses that line. This is necessary,
because all functional languages are Turing complete, but are we
giving the planner an impossible task?

# Limiting the power

I believe that we can solve the problem by separating the "query"
parts of a program, that consist only of relational operators, from
the "looping" parts of a program. This is unproven at this point, but
is bolstered by the observation that many data-oriented programs fall
into one of the following patterns:

* **Pure queries** consist only of relational operators. Such programs
  are often small, are structurally similar to the equivalent SQL, and
  can be planned in a similar fashion.
* **Queries with locally defined scalar functions** can be separated
  into a pure query that makes calls into user-defined functions. The
  user-defined functions do not invoke relational operators and
  therefore the query can be planned as normal.
* **Queries in loops** can be converted into a parameterized query
  that is executed under the control of a functional program.
* **Iterative queries**, for example queries that add to a set until
  it reaches a fixed point, can be planned and executed using
  techniques such as
  [stratified recursion](http://infolab.stanford.edu/~ullman/fcdb/spr99/lec13.pdf).
* **User-defined table functions connected by relational
  operators**. This is essentially the
  [MapReduce](https://en.wikipedia.org/wiki/MapReduce) pattern.  What
  happens in the table functions is beyond our control, but if we know
  something about their inputs and outputs (for example, that an
  aggregate function can be computed by examining only rows with the
  same key) then the framework can assist in running the query
  reliably and in parallel.

In all of these patterns, if we can recognize the 'query' parts we can
optimize them using conventional techniques. If we cannot recognize
any 'query' parts, nothing is lost; we can still execute the whole as
a functional program.

# Conclusion

Morel is an exciting experimental language that combines the best
aspects of database query languages and functional programming.

In this brief introduction, I have not gone into the details of
Morel's syntax, semantics or implementation, but examples can be found
on the
[Morel site](https://github.com/hydromatic/morel/blob/main/README.md)
and in
[Morel's test suite](https://github.com/hydromatic/morel/tree/main/src/test/resources/script),
and I plan to write more blog posts over the following months.

If you have comments, please reply on Twitter:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>

This article
[has been updated](https://github.com/julianhyde/share/commits/main/blog/{{ page.path }}).
