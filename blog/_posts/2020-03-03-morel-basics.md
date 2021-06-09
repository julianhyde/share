---
layout: post
title:  "Morel: The basic language"
date:   2020-03-03 12:00:00 -0800
author: Julian Hyde
image:  /assets/img/OldDesignShop_MushroomSpringMorel-240x240.jpg
tweet:  https://twitter.com/julianhyde/status/1234746906779119616
---

Last week I wrote about my
[goals for Morel]({% post_url 2020-02-25-morel-a-functional-language-for-data %}),
extending a simple functional language (Standard ML) with relational
operations so that it can be used as a query language.

This week I'd like to go over the basics of the language. Much of this
is the same as ML, and that's a good thing. If you want to learn more
about ML, there are
[plenty of good resources](http://www.cs.cmu.edu/~rwh/isml/book.pdf).

# The shell

The easiest way to start Morel is in its shell. The following example
starts Morel from macOS and evaluates a string literal.

```bash
$ ./morel
morel version 0.1.0 (java version "13", JRE null (build 13+33), JLine terminal, xterm-256color)
= "hello, world!";
val it = "hello, world!" : string
=
```

(To build Morel and start the shell for yourself, follow the
instructions on
[GitHub](https://github.com/julianhyde/morel/blob/main/README.md).
To exit the shell, type Ctrl+D.)

# Primitive types and simple expressions

As a functional language, everything in Morel is an expression.  The
basic types are `bool`, `int`, `real`, `string`, `char`, and `unit`.
Here are literals in each.

```sml
= false;
val it = false : bool
= 10;
val it = 10 : int
= ~4.5;
val it = ~4.5 : real
= "morel";
val it = "morel" : string
= #"a";
val it = #"a" : char
= ();
val it = () : unit
```

As you'd expect, there are built-in operators for each data type. Here
are a few examples:

```sml
= true andalso false;
val it = false : bool
= true orelse false;
val it = true : bool
= not false;
val it = true : bool
= 1 + 2;
val it = 3 : int
= ~(5 - 2);
val it = ~3 : int
= 10 mod 3;
val it = 1 : int
= "mo" ^ "rel";
val it = "morel" : string
```

# Variables

You can assign values to variables.

```sml
= val x = 7;
val x = 7 : int
= val y = x mod 3;
val y = 1 : int;
= x + y;
val it = 8 : int
```

(Morel, following Standard ML, actually calls them "value bindings"
rather than "variables" because you cannot change their value. It's
not much of a hardship, because you can create a new variable with the
same name, and it will obscure the old variable and its value.)

There is a special variable called `it` used by the shell.  Each time
you evaluate an expression, the shell and assigns the value to a
variable called `it`, and prints the value and its type.  You can use
`it` in the next expression.

```sml
= "morel";
val it = "morel" : string
= String.size it;
val it = 5 : int
= it + 4;
val it = 9 : int
```

A `let` expression binds one or more values and evaluates an expression.

```sml
= let
-   val x = 3
-   val y = 2
- in
-   x + y
- end;
val it = 5 : int
```

# Lists, records and tuples

In addition to primitive types, there are list, record, and tuple
types.

```sml
= [1, 2, 3];
val it = [1,2,3] : int list
= {id = 10, name = "Scooby"};
val it = {id=10,name="Scooby"} : {id:int, name:string}
= (1, true, "yes");
val it = (1,true,"yes") : int * bool * string
```

Tuples are actually just records with fields named "1", "2", and so
on. The following example shows that the values are identical, and
have the same type, whether you use tuple or record syntax.

```sml
= (1, true, "yes");
val it = (1,true,"yes") : int * bool * string
= {1 = 1, 2 = true, 3 = "yes"};
val it = (1,true,"yes") : int * bool * string
= (1, true, "yes") = {1 = 1, 2 = true, 3 = "yes"};
val it = true : bool;
```

The empty record and empty tuple are equal, and are the only value of
the type `unit`. Morel outputs it as `()`.

```sml
= {};
val it = () : unit
= ();
val it = () : unit
= {} = ();
val it = true : bool;
```

# Functions

Functions are expressions, too.  `fn` makes a lambda expression.
After we have bound the lambda value to `plusOne`, we can use
`plusOne` as a function.

```sml
= val plusOne = fn x => x + 1;
val plusOne = fn : int -> int
= plusOne 2;
val it = 3 : int
```

Function declarations are common, so the `fun` keyword provides a
shorthand: "<code>fun <i>f</i> <i>arg</i> = <i>exp</i></code>" is
short for "<code>val <i>f</i> = fn <i>arg</i> => <i>exp</i></code>".

```sml
= fun plusOne x = x + 1;
val plusOne = fn : int -> int
= plusOne 2;
val it = 3 : int
```

Functions can have multiple arguments, separated by spaces.

```sml
= fun plus x y = x + y;
val plus = fn : int -> int -> int
= plus 3 4;
val it = 7 : int
```

If we supply too few arguments, we get a closure that captures
the argument value and can be applied later.

```sml
= val plusTen = plus 10;
val plusTen = fn : int -> int
= plusTen 2;
val it = 12 : int
```

Functions can be recursive. Here, the `factorial` function evaluates
by calling itself, using the mathematical identity that `n! = n *
(n-1)!`.

```sml
= fun factorial n =
-   if n = 1 then
-     1
-   else
-     n * factorial (n - 1);
val factorial = fn : int -> int
= factorial 1;
val it = 1 : int
= factorial 5;
val it = 120 : int
```

# Higher-order functions and type inference

A *higher-order function* is a function that operates on other
functions. Here are a couple of examples.

The `map` function applies a given function `f` to each element of a
list, returning a list, as follows:

```sml
= fun map f [] = []
-   | map f (head :: tail) = (f head) :: (map f tail);
val map = fn : ('a -> 'b) -> 'a list -> 'b list
= fun double n = n * 2;
val double = fn : int -> int
= map double [1, 2, 3, 4];
val it = [2,4,6,8] : int list
```

The type of the `map` function, above, is `fn : ('a -> 'b) -> 'a list
-> 'b list`.  Morel's type system (based, like that of ML, on the
[Hindley-Milner type system](https://www.lesswrong.com/posts/vTS8K4NBSi9iyCrPo/a-reckless-introduction-to-hindley-milner-type-inference))
has deduced that `map` has a polymorphic type, and `'a` and `'b` are
type variables.  This means that if `f` has type `'a -> 'b`, for any
types `'a` and `'b`, then `map f` will transform a list of '`a' to a
list of '`b'.

For example, if `f` is the built-in function `String.size` of type
`string -> int`, then `'a` is `string` and `'b` is `int`, and `map
String.size` will convert a `string list` to an `int list`.

Notice that we did not declare any types; the type system deduced
everything for us. Type inference is perhaps ML's greatest feature. In
Morel, it helps us achieve our goal of writing powerful queries
concisely. We don't need to specify types, and furthermore, we can
include temporary values and functions in the query whenever we need
them.

The `filter` function keeps only those elements of a list for which a
predicate `p` evaluates to true, as follows:

```sml
= fun filter p [] = []
-   | filter p (head :: tail) =
-     if (p head) then
-       (head :: (filter p tail))
-     else
-       (filter p tail);
val filter = fn : ('a -> bool) -> 'a list -> 'a list
= fun even n = n mod 2 = 0;
val even = fn : int -> bool
= filter even [1, 2, 3, 4];
val it = [2,4] : int list
```

You may notice that `map` and `filter` are very similar to the
`SELECT` and `WHERE` clauses of a SQL statement. This is no surprise:
relational algebra, which underlies SQL, is basically a collection of
higher-order functions applied to lists of records (relations).

Can we extend ML syntax to make it easier to write relational algebra
expressions? You bet!

# Relational expressions

`from` is a Morel extension that iterates over one or more lists,
applies relational operations, and returns a list.

It has a similar purpose to SQL's `SELECT`. But unlike `SELECT`, its
inputs and outputs can be collections of any type (not just
records). Also, Morel makes no distinction between relations and
expressions; therefore Morel do not need operations like SQL's
`UNNEST` to deal with nested collections, and we can operate on lists
in memory just like tables in a database.

Let's start by defining `emps` and `depts` relations as lists of
records.

```sml
- val emps =
=   [{id = 100, name = "Fred", deptno = 10},
=    {id = 101, name = "Velma", deptno = 20},
=    {id = 102, name = "Shaggy", deptno = 30},
=    {id = 103, name = "Scooby", deptno = 30}];
val emps =
  [{deptno=10,id=100,name="Fred"},{deptno=20,id=101,name="Velma"},
     {deptno=30,id=102,name="Shaggy"},{deptno=30,id=103,name="Scooby"}]
  : {deptno:int, id:int, name:string} list
= val depts =
-   [{deptno = 10, name = "Sales"},
-    {deptno = 20, name = "HR"},
-    {deptno = 30, name = "Engineering"},
-    {deptno = 40, name = "Support"}];
val depts =
  [{deptno=10,name="Sales"},{deptno=20,name="HR"},
     {deptno=30,name="Engineering"},{deptno=40,name="Support"}]
  : {deptno:int, name:string} list
```

Now let's run our first query:

```sml
= from e in emps yield e;
val it =
  [{deptno=10,id=100,name="Fred"},{deptno=20,id=101,name="Velma"},
     {deptno=30,id=102,name="Shaggy"},{deptno=30,id=103,name="Scooby"}]
  : {deptno:int, id:int, name:string} list
```

The equivalent in SQL would be

```sql
SELECT *
FROM emps AS e
```

In Morel there is no difference between a query, a table, and a
list-valued expression, so we could have instead written just `emps`.

```sml
= emps;
val it =
  [{deptno=10,id=100,name="Fred"},{deptno=20,id=101,name="Velma"},
     {deptno=30,id=102,name="Shaggy"},{deptno=30,id=103,name="Scooby"}]
  : {deptno:int, id:int, name:string} list
```

A `where` clause filters out rows, and a `yield` clause controls which
fields are returned.

```sml
= from e in emps
-   where #deptno e = 30
-   yield {id = #id e};
val it = [{id=102},{id=103}] : {id:int} list
```

SQL equivalent is as follows:

```sql
SELECT e.id
FROM emps AS e
WHERE e.deptno = 30
```

If you omit `yield`, you get the raw values of the loop variable `e`.

```sml
= from e in emps
-   where #deptno e = 30;
val it =
  [{deptno=30,id=102,name="Shaggy"},
     {deptno=30,id=103,name="Scooby"}]
  : {deptno:int, id:int, name:string} list
```

# Shorthand

In ML, the usual way to access a field is via an accessor function
that starts with '#'. For example, `#id e` returns the `id` field of
record `e`. But Morel has an alternative syntax, `e.id`, which is more
familiar for SQL users.

Also, when you are constructing a record ML requires each field to be
named, e.g. `id = #id e`, but in Morel you can omit the name field if
it is the same as the current field or variable.

Thus the following 3 queries are equivalent:

```sml
= from e in emps
-   yield {e = #id e};
val it = [{id=100},{id=101},{id=102},{id=103}] : {id:int} list
= from e in emps
-   yield {e = e.id};
val it = [{id=100},{id=101},{id=102},{id=103}] : {id:int} list
= from e in emps
-   yield {e.id};
val it = [{id=100},{id=101},{id=102},{id=103}] : {id:int} list
```

I'll use the abbreviated forms from now on.

# Joins and sub-queries

The following query joins employees and departments relations on
department number.

```sml
= from e in emps,
-     d in depts
-   where e.deptno = d.deptno
-   yield {e.id, e.deptno, ename = e.name, dname = d.name};
val it =
  [{deptno=10,dname="Sales",ename="Fred",id=100},
   {deptno=20,dname="HR",ename="Velma",id=101},
   {deptno=30,dname="Engineering",ename="Shaggy",id=102},
   {deptno=30,dname="Engineering",ename="Scooby",id=103}]
  : {deptno:int, dname:string, ename:string, id:int} list
```

The following query returns the names of employees in the Engineering
department.

```sml
= let
-   fun exists [] = false
-     | exists (head :: tail) = true
- in
-   from e in emps
-     where exists (from d in depts
-                   where d.deptno = e.deptno
-                   andalso d.name = "Engineering")
-     yield e.name
- end;
val it = ["Shaggy","Scooby"] : string list
```

This query shows how much can be accomplished in Morel with just
functions, without extending the language.  In SQL, the equivalent
query would have `EXISTS` and a correlated sub-query, but in Morel
`exists` is an ordinary function that we have defined in the query,
and a correlated sub-query is just an expression that happens to
reference return a list and reference variables in an enclosing scope.

# Summary

To recap, Morel has:
* primitive types `bool`, `int`, `real`, `string`, `char`, `unit`;
* also `list`, tuple, record, and function types;
* lambda expressions and recursive functions;
* polymorphism and type-inference;
* relational `from` expressions (a Morel extension to Standard ML).

This was just a quick introduction to Morel and its ancestor Standard
ML, and I had to skip over many topics. There wasn't time to cover
algebraic types and pattern-matching, variations to `from` expressions
such as the `group` clause, and how Morel accesses external data and
optimizes programs. I hope to cover these topics in upcoming posts.

If you have comments, please reply on Twitter:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>

This article
[has been updated](https://github.com/julianhyde/share/commits/main/blog/_posts/2020-03-03-morel-basics.md).
