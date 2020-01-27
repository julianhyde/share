---
layout: post
title: How Mondrian evaluates expressions
date: '2009-04-30T10:43:00.000-07:00'
author: Julian Hyde
tags:
- mondrian cache calculated members
modified_time: '2009-04-30T11:22:19.772-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-2590586374515504298
blogger_orig_url: https://julianhyde.blogspot.com/2009/04/how-mondrian-evaluates-expressions.html
---

When it comes to expression evaluation, Mondrian keeps things
simple. It doesn't tend to cache the results of expressions, but
calculates them each time they are evaluated. Eventually the
calculation tunnels through all calculated members and ends up at an
atomic cell. Atomic cells are retrieved from the database, and stored
in the cell value cache, so they are only calculated once.

By the way, an atomic cell is not necessarily at the lowest level of
the hierarchy; Mondrian would prefer to load cells at a coarse
granularity, and leave the hard work of aggregating values to the
database, or even better, an aggregate table. And Mondrian does its
best to retrieve atomic cells in batches. It gathers together requests
for lots of cells of the same granularity and generates a single SQL
statement to retrieve them all at once.

Mondrian's 'keep it simple' scheme comes unstuck when a particular
calculation is repeated many times over. Nick Goodman came up with a
classic example of this in
[bug MONDRIAN-552](https://jira.pentaho.com/browse/MONDRIAN-552).
The query is as follows:

{% highlight sql %}
with member [Measures].[Profit Change] as
    ([Measures].[Profit], [Time].CurrentMember)
      - ([Measures].[Profit], [Time].PrevMember)
  member [Measures].[Running Total] as
    ([Measures].[Profit], [Time].CurrentMember)
      + ([Measures].[Running Total], [Time].PrevMember)
  member [Measures].[Average Daily Running Total] as
    Avg(
      Descendants([Time].CurrentMember, [Time.Weekly].[Day]),
      [Measures].[Running Total])
select {[Measures].[Profit Change],
    [Measures].[Running Total],
    [Measures].[Average Daily Running Total]} ON COLUMNS,
  {[Time.Weekly].[Week].Members} ON ROWS
from [Sales]
{% endhighlight %}

Note how `[Measures].[Running Total]` is recursive. The running total
for week 3 is defined as the running total for week 2 plus the profit
for week 3. To calculate the average running total for week 99,
Mondrian computes profit for the first 99 weeks and to calculate the
average running total for week 100, Mondrian computes profit 100 for
the first 100 weeks. There's lots of wasted effort: Mondrian has
computed profit 50,000 times when it could have done it just 100 times
and cached the results.

The solution is simple: wrap the calculation for `[Measures].[Running
Total]` in the
[Cache() function](https://mondrian.pentaho.org/documentation/performance.php#Optimizing_Calculations_with_the_Expression_Cache),
and Mondrian will compute the value only once.

You will see that in the bug I come up with a couple of proposals for
making Mondrian better. I don't think Mondrian should automatically
cache every expression, because caching costs time and memory, and
most expressions are only evaluated once or twice. And by the way, you
should use the `Cache` function sparingly, for the same reason.

But it would be nice if Mondrian could automatically detect some cases
where expression caching is desirable. The proposed `cache` property
of a calculated member would have three values: 0 (never cache), 1
(always cache) and null (Mondrian should use its best judgment). Most
calculated members would leave the caching up to Mondrian, so we would
need to come up with a simple, effective rule that governs caching
before we implemented this feature. What do you think the rule should
be?
