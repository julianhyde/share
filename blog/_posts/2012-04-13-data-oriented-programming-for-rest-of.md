---
layout: post
title: Data-oriented programming for the rest of us
date: '2012-04-13T02:24:00.000-07:00'
author: Julian Hyde
tags:
modified_time: '2012-04-13T02:24:48.885-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-1398800168289079083
blogger_orig_url: https://julianhyde.blogspot.com/2012/04/data-oriented-programming-for-rest-of.html
---

I have been a fan of
[LINQ](https://en.wikipedia.org/wiki/Linq) for several years
(my [Saffron](https://saffron.sourceforge.net/overview.html)
project covered many of the same themes) but I've had difficulty
explaining why it isn't just a better
[Hibernate](https://en.wikipedia.org/wiki/Hibernate_(Java)). In
his article “[Why LINQ Matters: Cloud Composability Guaranteed](http://queue.acm.org/detail.cfm?id=2141937)”
(initially in ACM Queue, now in April's CACM), Brian Beckman puts his finger on it.

The idea is **composability**.

He writes:

> Encoding and transmitting such trees of operators across tiers of a
> distributed system have many specific benefits, most notably:
>
> * Bandwidth savings from injecting filters closer to producers of
>   data and streams, avoiding transmission of unwanted data back to
>   consumers.
> * Computational efficiency from performing calculations in the
>   cloud, where available computing power is much greater than in
>   clients.
> * Programmability from offering generic transform and filter
>   services to data consumers, avoiding the need for clairvoyant
>   precanning of queries and data models at data-producer sites.

Databases have been doing this kind of stuff for years. There is a
large performance difference between stored and in-memory data, and
often several ways to access it, so the designers of the first
databases took the decision about which algorithm to use out of the
hands of the programmer. They created a query language out of a few
theoretically well-behaved (and, not coincidentally, composable)
logical operators, a set of composable physical operators to implement
them, and a query planner to convert from one to the other. (Some call
this component a
“[query optimizer](https://en.wikipedia.org/wiki/Query_optimizer)”,
but I prefer the more modest term.) Once the query
planner was in place, they could re-organize not only the algorithms,
but also the physical layout of the data (such as indexes and
clustered tables) and the physical layout of the system
([SMP](https://en.wikipedia.org/wiki/Symmetric_multiprocessing)
and [shared-nothing](https://en.wikipedia.org/wiki/Shared_nothing_architecture)
databases).

These days, there are plenty of other programming tasks that can
benefit from the intervention of a planner that understands the
algorithm. The data does not necessarily reside in a database (indeed,
[may not live on disk at all](https://www.sqlstream.com)),
but needs to be processed on a distributed system, connected by
network links of varying latency, by multi-core machines with lots of
memory.

What problems benefit from this approach? Problems whose runtime
systems are complex, and where the decisions involve large
factors. For example, “Is it worth writing my data to a network
connection, which has 10,000x the latency of memory, if this will
allow me to use 1000x more CPUs to process it?”. Yes, there are a lot of
[problems like that these days]({% post_url 2012-04-11-big-data-is-dead-long-live-big-data %}).

### Composability

Beckman's shout-out to composability is remarkable because it is
something the database and programming language communities can agree
on. But though they may agree about the virtues of composability, they
took it in different directions. The database community discovered
composability years ago, but then set their query language into stone,
so you couldn't add any more operators. Beckman is advocating writing
programs using composable operators, but does not provide a framework
for optimizing those operator trees.

LINQ stands for “Language-INtegrated Query”, but for these purposes,
the important thing about LINQ is not that it is “language
integrated”. It really doesn't matter whether the front end to a LINQ
system uses a “select”, “where” and “from” operator reminiscent of
SQL:

{% highlight java %}
var results = from c in SomeCollection
              where c.SomeProperty < 10
              select new {c.SomeProperty, c.OtherProperty};
{% endhighlight %}

or higher-order operators on collections:

{% highlight java %}
var results =
  SomeCollection
    .Where(c => c.SomeProperty < 10)
    .Select(c => new {c.SomeProperty, c.OtherProperty});
{% endhighlight %}

or actual SQL embedded in JDBC:

{% highlight java %}
ResultSet results = statement.executeQuery(
    "SELECT SomeProperty, OtherProperty\n"
    + "FROM SomeCollection\n"
    + "WHERE SomeProperty < 10");
{% endhighlight %}

All of the above formulations are equivalent, and each can be
converted into the same intermediate form, a tree of
operators.

What matters is what happens next: a planner behind the scenes
converts the operator tree into an optimal algorithm. The planner
understands what the programmer is asking for, the physical layout of
the data sources, the statistics about the size and structure of the
data, the resources available to process the data, and the algorithms
that can implement available to accomplish that. The effect will be
that the program always executes efficiently, even if the data and
system are re-organized after the program has been written.

### Query planner versus compiler

Composability is the secret sauce that powers query planners,
including the one in LINQ. At first sight, a query planner seems to
have a similar purpose to a programming language compiler. But a query
planner is aiming to reap the large rewards, so it needs to consider
radical changes to the operator tree. Those changes are only possible
if the operators are composable, and sufficiently well-behaved to be
described by a small number of transformation rules. A compiler does
not consider global changes, so does not need a simple, composable
language.

The differences between compiler and query planner go further. They
run in different environments, and have different goals. Compared to a
typical programming language compiler, a query planner...

* ... plans later. A compiler optimizes at the time that the program
  is compiled; query planners optimize just before it is executed.
* ... uses more information. A compiler uses the structure of the
  program; query planners use more information on the dynamic state of
  the system.
* ... is involved in task scheduling. Whereas a compiler is quite
  separate from the task scheduler in the language's runtime
  environment, the line between query planners and query schedulers is
  blurred. Resource availability is crucial to query
  planning.
* ... optimizes over a greater scope. A compiler optimizes individual
  functions or modules; query planners optimize the whole query, or
  even the sequence of queries that make up a job.
* ... deals with a simpler language. Programming languages aim to be
  expressive, so have many times more constructs than query
  languages. Query languages are (not by accident) simple enough to be
  optimized by a planner. (This property is what Beckman calls
  “composability”.)
* ... needs to be more extensible. A compiler's optimizer only needs
  to change when the language or the target platform changes, whereas
  a query planner needs to adapt to new front-end languages,
  algorithms, cost models, back-end data systems and data structures.

These distinctions over-generalize a little, but I am trying to
illustrate a point. And I am also giving query planners an unfair
advantage, contrasting a “traditional” compiler with a “still just a
research project” planner. (Modern compilers, in particular
[just-in-time (JIT) compilers](https://en.wikipedia.org/wiki/Just-in-time_compilation),
share some of the dynamic aspects of query planners.) The point is
that a compiler and a planner have different roles, and one should not
imagine that one can do the job of the other.

The compiler allows you to write your program in at a high level of
abstraction in a rich language; its task is to translate that complex
programming language into a simpler machine representation. The
planner allows your program to adapt to its runtime environment, by
looking at the big picture. LINQ allows you to have both; its
architecture provides a clear call-out from the compiler to the query
planner. But it can be improved upon, and points to a system superior
to LINQ, today's database systems, and other data management systems
such as [Hadoop](https://hadoop.apache.org/).

### A manifesto

**1. Beyond .NET**. LINQ only runs on
[Microsoft's .NET framework](https://www.microsoft.com/net),
yet Java is arguably the standard platform for data management. There
should be front-ends for other JVM-based languages such as
[Scala](http://www.scala-lang.org/) and
[Clojure](http://clojure.org/).

**2. Extensible planner**. Today's database query planners work with a
single query language (usually SQL), with a fixed set of storage
structures and algorithms, usually requiring that data is brought into
their database before they will query it. Planners should be allow
application developers to add operators and rules. By these means, a
planner could accept various query languages, target various data
sources and data structures, and use various runtime engines.

**3. Rule-driven**. LINQ has already rescued data-oriented programming
from the database community, and proven that a query planner can exist
outside of a database. But to write a LINQ planner, you need to be a
compiler expert. Out of the frying pan and into the fire. Planners
should be configurable by people who are neither database researchers
nor compiler writers, by writing simple rules and operators. That
would truly be data-oriented programming for the rest of us.
