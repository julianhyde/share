---
layout: post
title: Adding profiling and query plans to mondrian
date: '2010-06-01T11:27:00.000-07:00'
author: Julian Hyde
tags:
- mondrian explain plan mdx profiling
modified_time: '2010-06-01T11:27:54.944-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-2334063592217190619
blogger_orig_url: https://julianhyde.blogspot.com/2010/06/adding-profiling-and-query-plans-to.html
---

I've long wanted to add query plans and profiling to mondrian's
execution engine.

I just logged [jira case MONDRIAN-754](https://jira.pentaho.com/browse/MONDRIAN-754)
with my ideas for how it would work, and I thought it
would be good to share it as a blog post so I can gather
opinions. Most of the rest of this post is taken straight from the
jira case. Note that we are not committing to implement this feature
in any particular release. It's just an idea we are kicking around.

Mondrian currently does not help you find out where the time is spent
in executing a query, except for time spent in SQL. While most
Mondrian queries are SQL-heavy, it would help to see the breakdown.

Specifically: (a) it would increase understanding of the engine by
showing the physical plan (Calc nodes) that mondrian has chosen
(including caching of results, choice of iterator versus list
representation of sets), (b) it would help identify problems where a
particular leaf expression that generates SQL is executed repeatedly
and generates very similar SQL statements (see e.g.
[MONDRIAN-723](https://jira.pentaho.com/browse/MONDRIAN-723));
and (c) it would help developers identify MDX functions taking longer
than expected, or sub-optimal plans, and thereby tune mondrian.

I propose to add a profiling mode with two levels.

At the lower level, mondrian would print the plan of each MDX query as
it executed it:

```
Select(cube="[Cube]" mdx="with set [Foo xxx] AS [Foo].Children select non empty [Foo xxx] on 0, [Bar] * [Baz] on 1 from [Cube] where [Gender].[M]")
= CalculatedSets
=== CalculatedSet(name="Foo xxx", format="iterable")
===== Children(format="list")
======= HierarchyExpr(uniqueName="[Foo]")
= FilterAxis
=== MemberExpr(uniqueName="[Gender].[M]")
= Axes
=== Axis(ordinal="0", nonEmpty="true")
===== SetExpr(name="[Foo xxx]", format="iterable")
=== Axis(ordinal="1", nonEmpty="false")
===== CrossJoin
======= Call(function="{}")
========= Call(function="CURRENTMEMBER")
=========== HierarchyExpr(uniqueName="[Bar]")
======= Call(function="{}")
========= Call(function="CURRENTMEMBER")
=========== HierarchyExpr(uniqueName="[Baz]")
```

Format. I've used leading '=' to preserve indentation in this bug
report. I would use spaces in this bug report. I'd use spaces in the
actual feature. Or we could use XML.

There isn't much difference between the physical plan and the MDX
query because this is a simple example. Note that `[Foo]` has been
expanded as if the user had written
`{[Foo].CurrentMember}`. Differences in more complex plans include:
constant reduction; introduction of Cache operator; choice of physical
format (list, mutable list, iterator); adapters to change physical
format (e.g. copy a list to make it mutable); pushdown of non-empty
and other constraints to native SQL; strategies for evaluating named
sets (first time, each time).

Optionally each node could contain extra static information: the type
(e.g. `Integer`, `String`, `Numeric`, `Member(hierarchy='Store')`,
`Set(Tuple(Member(hierarchy=[Store])`,
`Member(level=[Time].[Year]))))`; format (list, mutable list,
iterator); list of hierarchies an expression is dependent on
(important for cached expressions).

With the higher level of profiling, mondrian would gather information
while the plan is running. The number of times a node is executed, and
amount of time in that node and its children. From that we can also
compute the amount of time in the node alone. At the end of execution,
mondrian would print the plan tree again, with "count", "self" and
"self+children" values attached to each node.

Of course there is always an overhead to collecting profiling info. We
would not recommend that people run production applications with
profiling enabled. The question is always whether the numbers gathered
from the profiled system are representative of the system running in
its normal mode. Call count would be 100% accurate, and elapsed time
should be within a few microseconds per call, so the profiling would
serve its purpose.
