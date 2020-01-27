---
layout: post
title: Hard-won lessons in Mondrian query optimization
date: '2009-01-07T17:00:00.000-08:00'
author: Julian Hyde
tags:
- olap query optimization mondrian native SQL
modified_time: '2009-01-07T18:42:10.641-08:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-2273958695666795847
blogger_orig_url: https://julianhyde.blogspot.com/2009/01/hard-won-lessons-in-mondrian-query.html
---

[Mondrian](https://mondrian.pentaho.org/) is generally very
smart in how it chooses to implement queries.  Over the last month or
so, I have learned some lessons about how hard can be to make Mondrian
smarter.

As a [ROLAP](https://en.wikipedia.org/wiki/ROLAP) engine (I
prefer to call it 'ROLAP with caching'), Mondrian's evaluation
strategy has always been a blend of in-memory processing, caching, and
native SQL execution. Naturally there is always SQL involved, because
Mondrian doesn't store any of its own data, but the question is how
much of the processing Mondrian pushes down to the DBMS and how much
it does itself, based on data in its cache.

The trends are towards native SQL execution. Data volumes are growing
across the board, Mondrian is being deployed to larger enterprises
with large data sets (in some cases displacing more established, and
expensive, engines). Mondrian cannot keep up with the growth by simply
pulling more data into memory and throwing one or two more CPU cores
at the problem.

Luckily a new breed of database engines, including
[Aster Data](http://www.asterdata.com/),
[Greenplum](http://www.greenplum.com/),
[Infobright](http://www.infobright.com/),
[Kickfire](https://www.kickfire.com/),
[LucidDB](http://www.luciddb.org/),
[Netezza](http://www.netezza.com/)
and [Vertica](http://www.vertica.com/),
are helping to solve the
data problem with innovative architectures and algorithms. To exploit
the power of the database engine, Mondrian's ability to generate
native SQL is more important than ever.

I have spent the last few weeks struggling to make Mondrian handle a
particular case more efficiently. It was ultimately unsuccessful, but
it was a case where defeat teaches you more than victory.

Here is the actual MDX query:

{% highlight sql %}
WITH SET [COG_OQP_INT_s9] AS
    'CROSSJOIN({[Store Size in SQFT].[Store Sqft].MEMBERS},[COG_OQP_INT_s8])'
  SET [COG_OQP_INT_s8] AS
    'CROSSJOIN({[Yearly Income].[Yearly Income].MEMBERS},[COG_OQP_INT_s7])'
  SET [COG_OQP_INT_s7] AS
    'CROSSJOIN({[Time].[Time].MEMBERS}, [COG_OQP_INT_s6])'
  SET [COG_OQP_INT_s6] AS
    'CROSSJOIN({[Store].[Store Country].MEMBERS},[COG_OQP_INT_s5])'
  SET [COG_OQP_INT_s5] AS
    'CROSSJOIN({[Promotions].[Promotions].MEMBERS}, [COG_OQP_INT_s4])'
  SET [COG_OQP_INT_s4] AS
    'CROSSJOIN({[Promotion Media].[Promotion Media].MEMBERS},[COG_OQP_INT_s3])'
  SET [COG_OQP_INT_s3] AS
    'CROSSJOIN({[Store Type].[Store Type].MEMBERS}, [COG_OQP_INT_s2])'
  SET [COG_OQP_INT_s2] AS
    'CROSSJOIN({[Marital Status].[Marital Status].MEMBERS}, [COG_OQP_INT_s1])'
  SET [COG_OQP_INT_s1] AS
    'CROSSJOIN({[Gender].[Gender].MEMBERS},
               {[Education Level].[Education Level].MEMBERS})'
SELECT {[Measures].[Unit Sales]} ON AXIS(0),
  NON EMPTY [COG_OQP_INT_s9] ON AXIS(1)
FROM [Sales]
WHERE ([Customers].[All Customers].[USA].[CA].[San Francisco].[Karen Moreland])
{% endhighlight %}

The query looks a bit fearsome, but is quite likely to occur in
practice as a business user slices and dices on several attributes
simultaneously. The rows axis is a `CrossJoin` of ten dimensions, but
because of the filtering effect of the slicer (combined with `NON
EMPTY`) the query evaluates to a single row. The goal is to make
Mondrian generate a SQL statement to evaluate the axis.

Each way that I tried to write the logic, I ended up making decisions
that made other optimizations invalid. It was difficult to make
Mondrian see the big picture: that, although named sets are not
supposed to inherit the context where they evaluated, in this case it
was OK; and to recognize a complex expression (many nested `CrossJoin`
operators, slicer, and implicit non-empty context), and convert the
whole thing into a single SQL statement. For instance, in one attempt
I succeeded in generating a SQL statement which evaluates very
efficiently, but in so doing I had to let the non-empty context of the
evaluator leak into places that it shouldn't... which broke quite a
few existing queries, in particular queries involving calculated sets.

There are several conclusions for Mondrian's architecture. One
conclusion is that we need to deal with filtering non-empty tuples as
part of the expression, not as a flag in the evaluator (the data
structure that contains, among other things, the set of members that
form the context for evaluating an expression).

MDX has an operator,
[EXISTS](https://msdn.microsoft.com/en-us/library/ms144936.aspx),
that specifies that empty tuples should be removed from a set. Then we
can reason about queries by applying logic-preserving transformations
(just the way that an RDBMS query optimizer works), which should be
safer than today's ad hoc reasoning. For example, if I am a developer
implementing an MDX function and the evaluator has `nonEmpty=true`, am
I **required to** eliminate non-empty tuples or am I merely **allowed
to** eliminate them? (In other words, will my caller return the wrong
result if I forget to check the evaluator flag?) I often forget, so I
suspect that filtering of empty tuples is performed inconsistently
throughout the Mondrian code base; which is a shame, because
eliminating empty tuples early can do a lot for performance.

I'd also like to use the same model for native SQL generation as for
other forms of expression compilation. Native SQL generation currently
happens at query execution time: when the function is evaluated, it
figures out whether it can possibly translate the logic (and the
constraints inherited from the evaluation context) into SQL. That is
currently unavoidable, because the nonEmpty flag is only available in
the evaluator, at query execution time. And we need to do some work at
query execution time, if only to plug in the keys of the members in
the current context as predicates in the SQL statement. But I've seen
several cases where we need to be smarter.

One example is `NON EMPTY [Level].Members` that always gets translated
into SQL even though the level only has two members and they are in
cache. Cost-based optimization would help there.

Another example is where there are many layers of MDX functions -- say
`Filter` on top of `CrossJoin` on top of `Filter` -- and these could
be rolled into a single SQL statement. The right approach is to build
a SQL statement by accretion, but it is too expensive to do every time
the expression is evaluated.

Further, as we add more rules for recognizing MDX constructs that can
turn into SQL, we will reach decision points where we choose to have
to choose whether to apply rule A or rule B. Solutions are (a) using
costing to decide which rule to apply, and (b) applying both rules and
seeing which ultimately generates a better outcome. Neither of these
solutions are suitable for query execution time: they need an
optimization stage, as part of query preparation.

It's ironic, considering I've been building SQL optimizers for years
(the first at
[Broadbase](http://infolab.stanford.edu/infoseminar.Archive/FallY97/slides/broadbase/sld001.htm),
and the second the optimizer for the
[Eigenbase project](http://www.eigenbase.org/), which is used
by both LucidDB and [SQLstream](https://www.sqlstream.com/))
that I have avoided giving Mondrian a true query optimizer for so
long. I know it's a lot of work to build an optimizer, and it's
foolish to start before you know what problem you need to solve.

Don't expect to see any changes in the short term; this kind of
architectural change doesn't happen fast. My struggle over the past
few weeks has been a big step in seeing the big picture, and realize
that the considerable pain and effort of unifying Mondrian's query
planning system is justified by the potential benefits in performance.
