---
layout: post
title:  "DML in Morel"
date:   2025-04-14 12:00:00 -0800
author: Julian Hyde
tweet:  https://x.com/julianhyde/status/1911938964627574890
---

Until last week, I hadn't thought much about
[DML](https://en.wikipedia.org/wiki/Data_manipulation_language) in
[Morel]({% post_url 2020-02-25-morel-a-functional-language-for-data %}).
I knew we would support DML, of course, but I had assumed that we
would just add commands analogous to SQL's `INSERT`, `UPDATE`,
`DELETE` commands.

But then, over a beer, my brother asked me whether Morel would have a
`MERGE` command. I almost choked on my
[pale ale](https://www.originalpatternbeer.com/#draft-section). I
realized that `MERGE` (also known as `UPSERT`) and the other SQL DML
commands brought a complexity that could damage the fragile design of
a new language. I started thinking about the whole DML problem. DML is
about mutation, Morel is a functional programming language, and
mutation and FP have always been
[uneasy bedfellows](https://en.wikipedia.org/wiki/Monad_(functional_programming)#State_monads).
So I devised some language extensions that I think fit both Morel and
modern data engineering workflow.

In this post, I describe those DML extensions, and related ideas about
transactions, incremental processing, and view maintenance.

I would love to hear feedback from people who do data engineering, ELT
and ETL, and love or hate how current tools and query languages solve
the problem. My extensions to Morel are at an early stage, so there is
plenty of time to change course. I will be speaking next week at
[Data Council 2025](https://www.datacouncil.ai/talks/more-than-query-future-directions-of-query-langages-from-sql-to-morel?hsLang=en),
so if you are in Oakland, please tell me what you think!

# Translate SQL DML commands to Morel

The obvious thing would be to directly translate SQL's DML commands
into Morel. To see how that would look, let's consider a simple SQL
script that executes the three DML commands on the `emps` table of the
[scott](https://github.com/julianhyde/scott-data-hsqldb) data set, and
then commits the changes.

{% highlight sql %}
-- Delete employees who earn more than 1,000.
DELETE FROM scott.emps
WHERE sal > 1000;

-- Add one employee.
INSERT INTO scott.emps (empno, deptno, ename, job, sal)
VALUES (100, 20, 'HYDE', 'ANALYST', 1150);

-- Double the salary of all managers.
UPDATE scott.emps
SET sal = sal * 2
WHERE job = 'MANAGER';

-- Commit.
COMMIT;
{% endhighlight %}

If we added `insert`, `update`, and `delete` operators to Morel, this
could become the following code.

{% highlight sml %}
(* Delete employees who earn more than 1,000. *)
delete e in scott.emps
  where e.sal > 1000;

(* Add one employee. *)
insert scott.emps
  [{empno = 100, deptno = 20, ename = "HYDE", job = "ANALYST", sal = 1150}];

(* Double the salary of all managers. *)
update e in scott.emps
  where e.job = 'MANAGER'
  assign (e, {e with sal = e.sal * 2});

(* Commit. *)
commit;
{% endhighlight %}

Note that `update` has an `assign` clause that updates
the current record, and we borrow the `with`
[functional update notation](https://github.com/hydromatic/morel/issues/249)
from [OCaml](https://ocaml.org/manual/5.3/coreexamples.html#s:tut-recvariants).

But this doesn't look right to me. The `insert`, `update`, and `delete`
operators all mutate their target data set -- which conflicts with
functional programming's immutability ethos -- and their syntax only
uses parts of Morel's elegant `from` expression.

Also, let's remind ourselves what we are trying to achieve.

# Requirements for a data engineering language

Things a data engineering language needs to do well:

* Refer to any previous version of a data set (committed or uncommitted).
* Work in terms of deltas between data sets, or versions of a data
  set, if that is the most natural way to frame the problem.
* Support data sets other than tables: local files, data frames, cloud
  object storage, non-SQL databases.
* Allow you to commit atomically when you are ready, and not force you
  to commit before you are ready.
* Create intermediate data sets (temporary tables), and materialize
  them if it improves performance.
* Give the compiler freedom to re-order commands, parallelize
  commands, and create (or suggest) intermediate data sets.
* Static typing, so that you can find and refactor all uses of a
  particular table or field across all queries, scripts, programs, and
  jobs.
* Parameterize scripts.
* Refer to previous types of a table, e.g. when the table did not have
  a `bonus` column, and when the `hiredate` column was a string.

SQL DML commands fail most of these requirements. (For example, they
can refer to only one previous version of a data set, namely the
contents of the table at the start of the current command.) DML looks
better when considered through the lens of functional programming.

# DML as functional programming

Functional programming languages really don't like assignment: they
would rather create a new variable than mutate an existing variable.

This makes sense for a functional language's compiler -- if a program
has no mutations, the compiler has more freedom to re-order and
parallelize the program -- but the controller running data engineering
script would also like to re-order and parallelize wherever possible.

SQL's DML commands are weird. (Though many of us have been writing SQL
for so long that we've gotten used to the semantics.) In a command
that modifies the `emps` table, you can also read from a table called
`emps`, but it is a different table! It is the *before image*, the
state of the table before this command started inserting, modifying or
deleting rows.

It is reasonable, and useful, to refer to the before image, but it is
only the before image of the current command. Suppose your script has
three commands -- a `DELETE`, an `INSERT` and an `UPDATE`, as in the
previous example -- and the `UPDATE` command wishes to reference the
state of the `emps` table before the `INSERT` command was run. In SQL,
you're out of luck. In the DML syntax I'm proposing for Morel, it is
straightforward:

{% highlight sml %}
(* Delete employees who earn more than 1,000. *)
val emps2 =
  from e in scott.emps
    where not (e.sal > 1000);

(* Add one employee. *)
val emps3 = emps2 union
  [{empno = 100, deptno = 20, ename = "HYDE", job = "ANALYST", sal = 1150}];

(* Double the salary of all managers. *)
val emps4 =
  from e in emps3
    yield if e.job = "MANAGER" then {e with sal = e.sal * 2} else e;

(* Commit. *)
commit {scott with emps = emps4};
{% endhighlight %}

The `insert`, `update`, and `delete` commands are no more, but we use
the new `with` operator during update and commit.

The `commit` command has changed considerably, and deserves at least
one blog posts all to itself. It is now the only command that mutates
(although what mutatation means in a language without side-effects has
yet to be nailed down). Its argument is the value of a database -- in
this case the `scott` database -- which is a record with a field for
each table -- and therefore allows us to commit changes to all tables
atomically; if we have changed only a few tables, `with` provides a
convenient shorthand. Representing a database as a record will (with
the `forall`
[quantifier just added](https://github.com/hydromatic/morel/issues/241),
and the proposed `check`
[keyword on type specifications](https://github.com/hydromatic/morel/issues/239))
allow us to define
[complex constraints](https://github.com/hydromatic/morel/issues/240)
such as foreign keys.

Because we use a new variable for each version of the `emps` table,
the previous versions are still available.

If you're worried about the performance and storage requirements of
all of these copies of the `emps` table, don't be. These copies are
virtual. For example, if your program asks for `emps2`, this might
expand to the contents of `emps3` minus the "delta" records
inserted. Or if you are using a table format such as
[Apache Iceberg](https://iceberg.apache.org/) or
[Hudi](https://hudi.apache.org/), each version is probably just a
different subset of the underlying files. The proposed Morel syntax is
closer to how databases implement transactions internally (using
[snapshots](https://en.wikipedia.org/wiki/Multiversion_concurrency_control)
and [journals](https://en.wikipedia.org/wiki/Write-ahead_logging)).

# Incremental computation

Incremental computation is essential when you are maintaining large
data sets on disk. If you have a table with a billion rows and you
want to insert, update or delete 1% of those rows, it makes no sense
to write the 99% of rows that are not effected. The SQL `INSERT`,
`UPDATE` and `DELETE` commands are clearly designed with that in mind.

But modifying 1% of a collection is mutation, which is antithetical to
the functional programming ethos. It is more complicated to say "here
are the rows added, modified and removed" than to say "here is the new
relation".

This proposal offers the best of both. A Morel modification command
returns the new set but is implemented by sending incremental commands
to the database. For example, if the "Delete employees who earn more
than 1,000" command above deletes 1% of employees, then `emps2` will
contain the remaining 99% of employees, but Morel will update the
database efficiently by sending a `DELETE` command.

Incremental computation also arises during incremental view
maintenance (IVM), or more generally, when maintaining some
materialized data set that has been created (explicitly by the user,
or implicitly by Morel's planner) earlier this script run or in a
previous run. It is notoriously hard to correctly write the
incremental logic; it is preferable to declare that the data set is
always equivalent to running a particular query, and have the planner
compute the deltas.

For both kinds of incremental computation, we believe that people
should work in the "declarative space," saying "this is the relation
that I want," and Morel should figure out the incremental operations
to compute and store that relation efficiently.

# Conclusion

This article outlines how DML commands could be implemented in Morel,
in a way that we believe is superior to SQL. Rather than creating
direct analogs of the SQL commands, our approach is to use queries to
create named intermediate results: `INSERT` becomes a query with
`union`, `DELETE` becomes a query with `minus` or `where not`, and
`UPDATE` becomes a query with a conditional `yield`. The only new
command, `commit`, has the magical ability to update all tables in a
database atomically.

This is consistent with Morel's ethos as a functional relational
language, and makes some of data engineering's challenges more
tractable.

If you have comments, come see me
[next week in Oakland](https://www.datacouncil.ai/talks/more-than-query-future-directions-of-query-langages-from-sql-to-morel?hsLang=en)
or reply on
[Bluesky @julianhyde.bsky.social](https://bsky.app/profile/julianhyde.bsky.social)
or Twitter:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>

<!--
This article
[has been updated](https://github.com/julianhyde/share/commits/main/blog/_posts/2025-03-31-morel-dml.md).
-->
