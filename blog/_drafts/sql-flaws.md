---
layout: post
title:  "Can we do better than SQL?"
date:   2020-02-22 16:32:00 -0800
author: Julian Hyde
image:  todo
tweet:  https://twitter.com/julianhyde/status/todo
---

How could we make SQL better?

SQL is an incredibly successful data language, but by design, it is
not a general-purpose programming language. Those gaps sometimes make
it frustrating to use; when you hit one, for example when you need a
user-defined function or to iterate, you have to step outside of SQL
and into a general-purpose programming language.

In this post, I investigate those gaps.

(I've been mulling these things over considering the design of a new
data language, called [Morel](https://github.com/julianhyde/morel),
but for most of this post I'll focus on SQL.)

# Viewing SQL through the eyes of a programmer

Suppose that I am a programmer who needs to write a simple program to
read data from a database, do some simple processing, and display the
results.

The conventional approach is to write a simple SQL query to retrieve
the data, and wrap it in a similarly simple program in a
general-purpose language (say Python or Java) or functional language
(say Haskell or OCaml) to process the results.

But suppose that I wanted to solve the whole problem in SQL? For that,
I need to use SQL as a full programming language. Is it a programming
language? And if not, what is missing?

To answer that question, we'll look at SQL through the eyes of a
programmer who is accustomed to functional programming languages, and
see how it measures up.

Why use functional languages as a model? SQL has a lot in common with
this class of languages: data structures are immutable, functions do
not have side-effects and return the same result given the same
arguments. SQL is strongly typed, but the compiler infers types, so
there are very few explicit types in a typical query. Lastly,
functional languages and SQL are mathematically well-founded, and so
high-level rewrites and optimizations are possible.

Here are the kinds of deficiencies we find:

* SQL queries contain two sub-languages -- table and column
  expressions -- that are not easy to mix
* Nested collections are cumbersome
* Cannot create temporary functions or values
* No higher-order functions or function values
* Cannot loop or recurse (except limited cases via `WITH RECURSIVE`)
* Limited type system (no polymorphism, no algebraic types, and
  anachronisms like length-limited strings)

We'll address the question 'Is SQL a functional programming language?'
in a later post. But for now let's look at the flaws in more detail.

# Table and column expressions

Consider the following query:

{% highlight sql %}
SELECT dept.deptno, hr.emps
FROM hr.depts AS dept
{% endhighlight %}

where `hr` is a schema (some systems, such as MySQL, would call it a
database, but the idea is the same), `emps` and `depts` are tables in
that schema, and `deptno` is a column in the `depts` table. `dept` is
a table alias, a variable that holds the current record from the
`depts` table.

To someone who is familiar with a general-purpose language (such as
Java or Python) or functional language but not familiar with SQL, the
query looks just fine. They would expect that `hr` is a record
containing fields `emps` and `depts` (whose values happen to be
collections), that `dept` is a record containing a `deptno` field, and
that you can use any expression as long as the values it needs are in
scope.

But the query is not valid SQL. The expression `hr.emps` would be
valid if it were in the `FROM` clause, but is not valid in the
`SELECT` clause.

This is confusing to the programmer, and results from the fact that
there are two kinds of expressions in SQL. (Actually three, if there
are `GROUP BY` or `HAVING` clauses around, but let's not go there.)

Expressions in the `FROM` clause must be *table expressions*. These
are the names of tables, possibly qualified with the name of a schema
(like `hr.depts`), or sub-queries. Table aliases (like `dept`) and
column names (like `deptno`) are not in scope, and scalar expressions
are not allowed.

Conversely, expressions in `SELECT` and `WHERE` clauses must be
*column expressions*. These consist of column names qualified by table
aliases, such as `dept.deptno`, literals, and all kinds of functions
and operators like `SUBSTRING` and `+`. But you cannot use schema or
table names, or in fact access relations at all.

# Nested collections

The segregation between table and column expressions is most harmful
when dealing with columns whose values are collections. They have
types `ARRAY` or `MULTISET` but their same contents look just like
tables.

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

You might think that the following would work:

{% highlight sql %}
SELECT dept.deptno, (SELECT * FROM hr.emps) AS emps
FROM hr.depts
{% endhighlight %}

It is valid

There is magic to go in the other direction as well; you can convert a
collection-valued column into a table expression using `UNNEST`. For
example, if the `depts` table has a `printers` field, you can write

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

Because it is difficult to convert between table and column
expressions, SQL has similar operators on both sides of the divide.

Suppose that each department has a collection of printers and fax
machines, and you wish to combine the two. You cannot use relational
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

In Morel, the expressions are straightforward:

{% highlight sml %}
from dept in hr.depts
yield {dept.deptno,
       devices = dept.printers union dept.faxes};

from dept in hr.depts
yield {dept.deptno,
       manufacturers = (from printer in dept.printers
                         yield printer.manufacturer)
                       union
                       (from fax in dept.faxes
                         yield fax.manufacturer)}
{% endhighlight %}

# Higher-order functions

TODO

# Type system

If you want to build a library of functions, you quickly realize the
need for polymorphism and lambdas (function values). Finding the
length of lists of employees and departments would require separate
functions, until you introduce polymorphism; then you can write a
generic function parameterized by record type.

Similarly, without function values, sorting a list of employees by age
or by salary would take two separate functions; when you add a
comparator function as an argument, you can use the same sorting
function for any ordering criteria.

Length-limited strings (for example the `VARCHAR(30)` type, which
allows strings up to 30 characters) recall a time when computers had
limited memory and data would be stored on punched cards.

TODO; some points:

* length-limited strings
* no generics
* algebraic types
* awkward handling of null values (via constraints
  rather than type)
* pattern matching vs. the humble CASE

# Typing NULL

From a modern programming language perspective, SQL handles null
values awkwardly.  For example, in Standard ML (and Morel), an `int`
value cannot be null. In fact there is no null value.  There is a type
`int option` (based on the
[Option](http://sml-family.org/Basis/option.html) structure), whose
values include `NONE`, `SOME 0` and `SOME 100`.

In SQL, every type allows nulls, and if you want to declare a column
whose value may not be null, you have to add a constraint `NOT
NULL`. The constraint actually belongs to the column definition, not
the type, so there are several places where you cannot specify `NOT
NULL`. For example, it is valid SQL to write

{% highlight sql %}
CAST(commission AS INTEGER)
{% endhighlight %}

but you cannot include a `NOT NULL` constraint in the target of a
`CAST`, as follows:

{% highlight sql %}
CAST(commission AS INTEGER NOT NULL)
{% endhighlight %}

# A functional language with relational extensions

These flaws can all be fixed, and SQL continues to evolve, they
probably will. But the result will be more complexity in a language
that already has a large footprint. (In my experience, most SQL users
stick to a subset that works for them, and avoid complex features like
windowed aggregate functions and nested collections.)

So, how about digging the tunnel from the other end? Rather than
extending SQL (adding function-values, a type system with polymorphism
and algebraic types, type inference, and looping), how about taking a
simple functional programming language and adding support for
relational data?

Morel is that language, and it is a work in progress.

Will people find it useful as a functional language, a query language,
or both?  Will we succeed in making a language that is [Turing
complete](https://en.wikipedia.org/wiki/Turing_completeness) but still
allows global optimizations? I don't know, but it's going to be fun
finding out.

# Comments

If you have comments, please reply on Twitter:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>
