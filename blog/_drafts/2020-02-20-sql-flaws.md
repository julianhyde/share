---
layout: post
title:  "SQL flaws"
date:   2020-02-20 16:32:00 -0800
author: Julian Hyde
image:  todo
tweet:  https://twitter.com/julianhyde/status/todo
---

SQL is an incredibly successful programming language.  There are a few
things that could have been done better, or at least differently.
I've been mulling these things over considering the design of a new
data language, called [Morel](https://github.com/julianhyde/morel);
more about Morel later, but for this post I'll focus on SQL.

# SQL flaws

* Different language for table and column expressions
* Poor handling of nested collections
* No higher-order functions or function values
* Outdated type system, with length-limited strings, no generics,
  algebraic types, awkward handling of null values (via constraints
  rather than type)

SQL does a lot of things right. Data structures are immutable,
functions do not have side-effects and return the same result given
the same arguments. The language is mathematically pure, and so
high-level rewrites and optimizations are possible. These the kind of
values you expect from a functional programming language.

We'll address the question 'Is SQL a functional programming language'
in another post. But for now let's look at the flaws in more detail.

# Table and column expressions

Consider the following query:

{% highlight sql %}
SELECT dept.deptno, hr.emps
FROM hr.depts AS dept
{% endhighlight %}

where `hr` is a schema (also called a database in such as MySQL or SQL
Server), `emps` and `depts` are tables in that schema, and `deptno` is
a column in the `dept` table.

To someone who is familiar with a general-purpose language (such as C,
Java, Python or Scala) but not familiar with SQL, the query looks
OK. They would expect that `hr` is a record containing fields `emps`
and `depts` (whose values happen to be collections), that `dept` is a
record containing a `deptno` field, and that you can use any
expression as long as the values it needs are in scope.

In fact the query is invalid, because of `hr.emps` in the `SELECT`
clause. `hr.emps` would be valid as a table expression, in the `FROM`
clause, where schema `hr` is visible; but in the `SELECT` clause, the
only record available is `dept`. Conversely, column expressions like
`dept.deptno` can only appear in `SELECT` and `WHERE` clauses. (The
rules get even more complex if there are `GROUP BY` or `HAVING`
clauses around.)

SQL wizards know that there are magical incantations to convert table
expressions into column expressions. For example, the `ARRAY` operator
converts a query into a column expression, and the query may contain a
table expression in its `FROM` clause:

{% highlight sql %}
SELECT dept.deptno, ARRAY (SELECT * FROM hr.emps) AS emps
FROM hr.depts
{% endhighlight %}

Or more concisely, the `TABLE` operator converts a table expression
into a query:

{% highlight sql %}
SELECT dept.deptno, ARRAY (TABLE hr.emps) AS emps
FROM hr.depts
{% endhighlight %}

The magic is reversible; you can convert a collection-valued column
into a table expression using `UNNEST`. For example, if the `depts`
table has a `printers` field, you can write

{% highlight sql %}
SELECT dept.deptno, printer.manufacturer
FROM hr.depts AS dept,
    UNNEST (dept.printers) AS printer (manufacturer, model)
{% endhighlight %}

In Morel, there is only one kind of expression, and it can be used in
the `from` or `yield` clauses. The equivalents of the above queries
are simpler, as follows:

{% highlight sml %}
from dept in hr.depts
yield {dept.deptno, hr.emps};

from dept in hr.depts,
  printer in dept.printers
yield {dept.deptno, printer.manufacturer};
{% endhighlight %}

# Handling nested collections

Because nested collections are not relations, you cannot use the usual
relational operators on them.

Suppose that each department has a collection of printers and fax
machines and you wish to combine the two. You cannot use relational
`UNION`; you have to use the `MULTISET UNION` collections operator:

{% highlight sql %}
SELECT dept.deptno,
    dept.printers MULTISET UNION dept.faxes AS devices
FROM hr.depts AS dept
{% endhighlight %}

If you want to project just the `manufacturer` field, you have to
convert to a query and back again:

{% highlight sql %}
SELECT dept.deptno,
    MULTISET (
        SELECT printer.manufacturer
        FROM UNNEST (dept.printers) AS printer
        UNION
        SELECT fax.manufacturer
        FROM UNNEST (dept.faxes) AS fax) AS manufacturers
FROM hr.depts AS dept
{% endhighlight %}

There are also specific aggregate functions for nested collections: `ARRAY_AGG`, `FUSION`, TODO

In Morel, the expressions are straightforward:

{% highlight sml %}
from dept in hr.depts
yield {dept.deptno, devices = dept.printers union dept.faxes};

from dept in hr.depts
yield {dept.deptno,
       devices = (from printer in dept.printers
                   yield printer.manufacturer)
                 union
                 (from fax in dept.faxes
                   yield fax.manufacturer)}
{% endhighlight %}

# Higher-order functions

TODO

# Type system

TODO; some points:

* length-limited strings
* no generics
* algebraic types
* awkward handling of null values (via constraints
  rather than type)
* pattern matching vs. the humble CASE

# A functional language with relational extensions

These flaws can all be fixed, and SQL continues to evolve, they
probably will. But the result will be more complexity in a language
that already has a large footprint. (In my experience, most SQL users
stick to a subset that works for them, and avoid complex features like
windowed aggregate functions and nested collections.)

So, how about digging the tunnel from the other end? Rather than
extending SQL (adding function-values, a type system with polymorphism and algebraic
types, type inference, and looping), how about taking a simple
functional programming langauge and adding support for relational
data? This is the experiment that is [Morel](TODO:ref).

The biggest danger is that we make a language that is too
powerful. When we give a language the ability to loop, or call
functions recursively, it becomes Turing-complete, and Turing-complete
programs cannot always be reasoned about. (See [the Halting
Problem](TODO:ref).)

I suspect that Morel avoids that problem in practice. When used as a
query language, Morel programs are often small, and if they are
computing something that SQL could compute, then they are very similar
in size and structure to that SQL. For larger programs there will be
recognizable patterns such as 'define a local function and use it in a
query' or 'execute a query iteratively, using its output as the input
of the next iteration, until it reaches fixed point'. In these
patterns, we should be able to recognize the 'query' parts and
optimize them. If we cannot recognize any 'query' parts, nothing is
lost; we can still execute the whole as a functional program.

These claims are as yet unproven, but proving them is the point of the
experiment. If we succeed, the reward is great: a query language that
is Turing-complete and general-purpose but still admits high-level
optimizations.

complex and verbose to write queries concisely

# Comments

If you have comments, please reply on Twitter:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>
