---
layout: post
title:  "Morel release 0.6.0"
date:   2025-05-02 12:00:00 -0800
author: Julian Hyde
image:  /assets/img/OldDesignShop_MushroomSpringMorel-240x240.jpg
tweet:  https://x.com/julianhyde/status/1918550796335227174
---

I am pleased to announce Morel
[release 0.6.0](https://github.com/hydromatic/morel/blob/main/HISTORY.md#060--2025-05-02),
just two months after
[release 0.5.0](https://github.com/hydromatic/morel/blob/main/HISTORY.md#050--2025-03-04).

It's been a busy couple of months. I attended
[Data Council 2025](https://www.datacouncil.ai/bay-2025), gave a talk called
"[More than Query](https://www.datacouncil.ai/talks/more-than-query-future-directions-of-query-langages-from-sql-to-morel?hsLang=en),"
and had discussions with a lot of smart people. In a follow-up
chat, Julien Le Dem and I
[went deeper](https://www.youtube.com/watch?v=zpdbEvhhne8) into the
topics I raised at Data Council.  A piece about how Morel could do
[DML and data engineering]({% post_url 2025-04-14-dml-in-morel %})
generated a lot of discussion. And behind the scenes I've been writing
a lot of code, doing fundamental work on Morel's
[type system](https://github.com/hydromatic/morel/issues/237) and
[collection types](https://github.com/hydromatic/morel/issues/235)
that is not yet fully baked but will appear in the next release.

Even though this is a slim release, I'm pleased to be able to add
language features for [logic](#1-logic-extensions) and
[updating records](#2-record-update),
[improvements to the shell](#3-tabular-mode), and
[performance improvements for the type-inference algorithm](#4-unifier-performance-improvements).

Let's explore a few of the new features.  For more information, see
the
[official release notes](https://github.com/hydromatic/morel/blob/main/HISTORY.md#060--2025-05-02).

# 1. Logic extensions

In logic, it is common to ask whether a predicate is true for all
elements of a set, or for any elements of a set. These questions are
essentially queries that return a boolean value. Morel already has
ways to express those queries, but in 0.6.0 we add
[syntax that is closer to logic](https://github.com/hydromatic/morel/issues/241).

The new `exists`, `forall` and `implies` keywords have the following
syntax.

<pre>
exp &rarr;
  ...
| <b>from</b> [ <i>scan<sub>1</sub></i> , ... , <i>scan<sub>s</sub></i> ] step*
                                relational expression (s ≥ 0)
| <b>exists</b> [ <i>scan<sub>1</sub></i> , ... , <i>scan<sub>s</sub></i> ] step*
                                existential quantification (s ≥ 0)
| <b>forall</b> [ <i>scan<sub>1</sub></i> , ... , <i>scan<sub>s</sub></i> ] <b>require</b> <i>exp</i>
                                universal quantification (s ≥ 0)
| <i>exp<sub>1</sub></i> <b>implies</b> <i>exp<sub>2</sub></i>
</pre>

As you can see, `exists` and `forall` have similar syntax to the
existing `from` expression. Collectively, these are called *query
expressions*, and are all documented in the new
[query reference](https://github.com/hydromatic/morel/blob/main/docs/query.md).

The new constructs are specified in terms of existing `from`, `count`,
`not`, and `orelse` operators, as shown in the following table. (Morel
currently uses the rewrites in the table, but that doesn't prevent us
from using an equivalent but more efficient rewrite -- say using a
semi-join rather than `count` -- in future.)

<table>
<tr>
<td>
Original <code>exists</code>
</td>
<td>
<pre>
<b>exists</b> <i>scan<sub>1</sub></i> , ... , <i>scan<sub>s</sub></i> <i>steps</i>
</pre>
</td>
</tr>

<tr>
<td>
Rewritten using <code>count</code>
</td>
<td>
<pre>
<b>count</b> (<b>from</b> <i>scan<sub>1</sub></i> , ... , <i>scan<sub>s</sub></i> <i>steps</i> ) > 0
</pre>
</td>
</tr>

<tr>
<td>
Original <code>forall</code>
</td>
<td>
<pre>
<b>forall</b> <i>scan<sub>1</sub></i> , ... , <i>scan<sub>s</sub></i>
  <b>require</b> <i>exp</i>
</pre>
</td>
</tr>

<tr>
<td>
Rewritten using <code>not exists</code>
</td>
<td>
<pre>
<b>not</b> (<b>exists</b> <i>scan<sub>1</sub></i> , ... , <i>scan<sub>s</sub></i>
  <b>where</b> <b>not</b> <i>exp</i>)
</pre>
</td>
</tr>

<tr>
<td>
Original <code>implies</code>
</td>
<td>
<pre>
<i>exp<sub>1</sub></i> <b>implies</b> <i>exp<sub>2</sub></i>
</pre>
</td>
</tr>

<tr>
<td>
Rewritten using <code>not</code> and <code>orelse</code>
</td>
<td>
<pre>
<b>not</b> <i>exp<sub>1</sub></i> <b>orelse</b> <i>exp<sub>2</sub></i>
</pre>
</td>
</tr>

</table>

To avoid confusion, the previous `Relational.exists` function, which
is a synonym for `List.null` and tests whether a list is non-empty,
has been renamed `Relational.nonEmpty`.

### Example 1. Existential quantification

Are there any employees with a salary greater than 1,000?

```sml
(* Using new "exists" keyword. *)
exists e in emps
  where e.sal > 1000.0;

(* Equivalent using "from" and "List.null". *)
not (List.null (from e in emps where e.sal > 1000.0));

(* Equivalent using "from" and "compute". *)
(from e in emps
  where e.sal > 1000.0
  compute count) > 0
```

The logic expression "&exist; *e* &isin; *set*. *predicate*" maps to the
Morel expression "<code><b>exists</b> <i>e</i> <b>in</b> <i>set</i>
<b>where</b> <i>predicate</i></code>".

`exists` has a syntax very similar to `from`; it starts a query
pipeline, and therefore you may add steps such as `join`, `group`,
and `order`. The expression yields `true` if the query returns at
least one row.

### Example 2. Universal quantification

Do all programmers have a salary greater than 900?
```sml
(* Using new "forall" keyword. *)
forall e in emps
  where e.job = "PROGRAMMER"
  require e.sal > 900.0;

(* Equivalent using "exists". *)
not (exists e in emps
  where e.job = "PROGRAMMER"
  andalso not (e.sal > 900.0));

(* Equivalent using "from" and "List.null". *)
List.null (from e in emps
           where e.job = "PROGRAMMER" andalso not (e.sal > 900.0));
```

The logic expression "&forall; *e* &isin; *set*. *predicate*" maps to
the Morel expression "<code><b>forall</b> <i>e</i> <b>in</b>
<i>set</i> <b>require</b> <i>predicate</i></code>".

Like `from` and `exists`, `forall` starts a pipeline, but that
pipeline must end with a `require` step. The query evaluates to `true`
if the predicate returns true for all rows that make it into
`require`. (If there are no rows, the result is trivially `true`.)

### Example 3. Implication

Why did we add `require` when `where` does almost the same thing?  Our
initial syntax only had `where`, and to solve the previous query many
people would end up writing something like this:

```sml
(* A query using "where" is invalid and not equivalent to the
   original query. *)
forall e in emps
  where e.job = "PROGRAMMER" andalso e.sal > 900;
```

This query is invalid (we decided that a `forall` query must end in
`require`, for reasons that will become clear) but even if it were
valid, it would be incorrect.  It is telling us whether all employees
are programmers who earn more than 900. If just one employee were a
manager earning 1,200, the query would return false. Not what we wanted!

So we added the `require` step. You can use `where` steps (and other
relational operators you like) to narrow down to a population you wish
to check (in this case, programmers), and finish with `require` to
check each member of that population.

But it got us thinking about other ways to write the query. How would
you write it if you were a logician? Probably like this:

```sml
(* Valid, and equivalent to the original query. *)
forall e in emps
  require not (e.job = "PROGRAMMER") orelse e.sal > 900;
```

The query considers the whole population (all employees, including
programmers and managers) and crafts the predicate so that rows not in
the population always pass. The new `implies` operator lets you do
just that: <code><i>a</i> <b>implies</b> <i>b</i></code> evaluates to
true if <code><i>a</i></code> is false, <code><i>b</i></code>
otherwise.

```sml
(* Valid, equivalent to the original query, and we think
   quite nice even if you're not a logician. *)
forall e in emps
  require e.job = "PROGRAMMER" implies e.sal > 900;
```

The `implies` operator (like most operators, including `orelse`,
`andalso` and arithmetic `-`, `mod` and `/`), is left-associative.
"<code><i>a</i> <b>implies</b> <i>b</i> <b>implies</b>
<i>c</i></code>" is equivalent to "(<code><i>a</i> <b>implies</b>
<i>b</i>) <b>implies</b> <i>c</i></code>," and hence to
"(<code><b>not</b> (<b>not</b> <i>a</i> <b>orelse</b> <i>b</i>)
<b>orelse</b> <i>c</i></code>."

# 2. Record update

While thinking about
[syntax for updating tables]({% post_url 2025-04-14-dml-in-morel %})
we needed a way to change just one field of a record.

Suppose that the `emps` table has eight fields and `emp` represents
one row from that table:

```sml
val emp = List.hd scott.emps;
(*[> val emp =
>   {comm=0.0,deptno=20,empno=7369,ename="SMITH",hiredate="1980-12-16",
>    job="CLERK",mgr=7902,sal=800.0}
>   : {comm:real, deptno:int, empno:int, ename:string, hiredate:string,
>      job:string, mgr:int, sal:real}]*)
```

If you want to change just the `sal` field, conventional record syntax
requires you to copy the other seven fields:

```sml
val emp2 = {emp.comm, emp.deptno, emp.empno, emp.ename, emp.hiredate,
    emp.job, emp.mgr, sal = emp.sal * 2.0};
(*[> val emp2 =
>   {comm=0.0,deptno=20,empno=7369,ename="SMITH",hiredate="1980-12-16",
>    job="CLERK",mgr=7902,sal=1600.0}
>   : {comm:real, deptno:int, empno:int, ename:string, hiredate:string,
>      job:string, mgr:int, sal:real}]*)
```

Being a functional programming language, Morel avoids direct field
mutation since mutation often leads to bugs. But creating an entirely
new record doesn't have to be so verbose.

We have borrowed OCaml's
[syntax for functional update of records](https://github.com/hydromatic/morel/issues/249):

```sml
val emp3 = {emp with sal = emp.sal * 2.0};
(*[> val emp3 =
>   {comm=0.0,deptno=20,empno=7369,ename="SMITH",hiredate="1980-12-16",
>    job="CLERK",mgr=7902,sal=1600.0}
>   : {comm:real, deptno:int, empno:int, ename:string, hiredate:string,
>      job:string, mgr:int, sal:real}]*)
```

The `with` keyword in a record expression tells Morel to copy the
original record (`emp` in this case) and only modify the fields
specified.

# 3. Tabular mode

When giving demos of Morel queries and programs, people quickly
understand that the programs are reading data from a database (such as
the `emps` table in the
[`scott`](https://github.com/julianhyde/scott-data-hsqldb) database
that is so ubiquitous in Morel's examples and documentation). But it
takes them a bit longer to realize that Morel is actually executing
queries. Why is that?

Maybe it's because the query results don't look much like queries.

```sml
from d in scott.depts
    join e in scott.emps on e.deptno = d.deptno
  where e.job = "CLERK"
  yield {d.dname, e.empno};
(*[> val it =
>   [{dname="ACCOUNTING",empno=7934},{dname="RESEARCH",empno=7369},
>    {dname="RESEARCH",empno=7876},{dname="SALES",empno=7900}]
>   : {dname:string, empno:int} list]*)
```

By default, result sets are condensed into a compact representation,
which works well for many of Morel's supported data types. However,
this format obscures the natural structure of tabular data. Let's use
the new [tabular mode](https://github.com/hydromatic/morel/issues/259):

```sml
set ("output", "tabular");
(*[> val it = () : unit]*)
```

Now the tabular structure is clear:

```sml
from d in scott.depts
    join e in scott.emps on e.deptno = d.deptno
  where e.job = "CLERK"
  yield {d.dname, e.empno};
(*[> dname      empno
> ---------- -----
> ACCOUNTING  7934
> RESEARCH    7369
> RESEARCH    7876
> SALES       7900
>
> val it : {dname:string, empno:int} list]*)
```

Tabular mode only activates if the data set is a list of records;
otherwise it falls back to the "classic" mode.  There's still much
room for improvement -- such as how values are formatted, and handling
types like `option`, nested records, and nested lists of integers and
strings -- but at least your tabular data now looks tabular.

# 4. Unifier performance improvements

Type inference is at the heart of Morel. We believe that a
well-designed, strong type system is the foundation for maintainable
software, but also that programmers shouldn't need to sprinkle types
all over their program to make it compile. Therefore the language needs
to be able to infer types for itself, and the gold standard is the
[Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system)
type inference algorithm.

That algorithm needs continuous improvement. For the upcoming
[ordered and unordered multisets](https://github.com/hydromatic/morel/issues/235)
feature we need
[overloaded operators](https://github.com/hydromatic/morel/issues/237),
and so we are [extending the type system](https://dl.acm.org/doi/pdf/10.1145/224164.224195).

This release, we have
[tuned the internal data structures](https://github.com/hydromatic/morel/issues/246)
of the unification algorithm that underlies type-inference. Expect
further evolution in future releases.

# Conclusion

Two months since the previous release, and with luck another release
of Morel in a few weeks.  Work continues on the features for that
release; we are especially looking forward to launching
[multisets](https://github.com/hydromatic/morel/issues/235), when they
are ready.

Until then, give Morel a try. Go to
[GitHub](https://github.com/hydromatic/morel) and you can have Morel
built and running in under a minute.

If you have comments, please reply on
[Bluesky @julianhyde.bsky.social](https://bsky.app/profile/julianhyde.bsky.social)
or Twitter:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>

<!--
This article
[has been updated](https://github.com/julianhyde/share/commits/main/blog/_posts/{{ page.slug }}.md).
-->
