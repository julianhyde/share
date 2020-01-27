---
layout: post
title: Formatting MDX as plain text
date: '2009-04-10T17:31:00.000-07:00'
author: Julian Hyde
tags:
- mondrian mdx formatting olap4j
modified_time: '2009-04-11T00:51:56.798-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-6537313086400326400
blogger_orig_url: https://julianhyde.blogspot.com/2009/04/formatting-mdx-as-plain-text.html
---

When Mondrian tools output MDX results as text, such as in the
`cmdRunner` utility, we've been using the same old crappy format for
years. For example, the query

{% highlight sql %}
select crossjoin(
    {[Time].[1997].[Q1], [Time].[1997].[Q2].[4]},
    {[Measures].[Unit Sales], [Measures].[Store  Sales]}) on 0,
  {[USA].[CA].[Los Angeles], [USA].[WA].[Seattle],
   [USA].[CA].[San Francisco]} on 1
FROM [Sales]
{% endhighlight %}

is formatted as
```
Axis #0:
{}
Axis #1:
{[Time].[1997].[Q1], [Measures].[Unit  Sales]}
{[Time].[1997].[Q1], [Measures].[Store  Sales]}
{[Time].[1997].[Q2].[4], [Measures].[Unit  Sales]}
{[Time].[1997].[Q2].[4], [Measures].[Store Sales]}
Axis #2:
{[Store].[All Stores].[USA].[CA].[Los Angeles]}
{[Store].[All  Stores].[USA].[WA].[Seattle]}
{[Store].[All Stores].[USA].[CA].[San Francisco]}
Row #0: 6,373
Row #0: 13,736.97
Row #0: 1,865
Row #0: 3,917.49
Row #1: 6,098
Row #1: 12,760.64
Row #1: 2,121
Row #1: 4,444.06
Row #2: 439
Row #2: 936.51
Row #2: 149
Row #2: 327.33
```

I've just
[checked in](http://p4web.eigenbase.org/@md=d&amp;c=6PU@12590?ac=10)
an alternative formatter that makes the result look more like a
pivot table. The same query would come out like this:

```
                     1997       1997        1997       1997
                     Q1         Q1          Q2         Q2
                                            4          4
                     Unit Sales Store Sales Unit Sales Store Sales
=== == ============= ========== =========== ========== ===========
USA CA Los Angeles   6,373      13,736.97   1,865      3,917.49
USA WA Seattle       6,098      12,760.64   2,121      4,444.06
USA CA San Francisco 439        936.51      149        327.33
```

Two questions:

1. Should we move this code into the
[olap4j](http://www.olap4j.org/) code base? (It would seem to
make sense because it doesn't require any mondrian internals to do the
job, and the processing requires a 'grid model' similar to query
models already part of olap4j. But I don't want to 'dump' code that is
not generally useful.)

2. What do people feel is the ideal format for formatting MDX results
as text? As a starting point, another couple of possible formats are
below.

**"Oracle" format**

```
                     1997
                     Q1                      Q2
                                            4
                     Unit Sales Store Sales Unit Sales Store Sales
=== == ============= ========== =========== ========== ===========
USA CA Los Angeles        6,373   13,736.97      1,865    3,917.49
    WA Seattle            6,098   12,760.64      2,121    4,444.06
    CA San Francisco        439      936.51        149      327.33
```

**"MySQL"  format**

```
|                          | 1997                                                |
|                          | Q1                       |  Q2                      |
|                          |                          | 4                        |
|                          | Unit Sales | Store Sales | Unit Sales | Store Sales |
+-----+----+---------------+------------+-------------+------------+-------------+
| USA | CA | Los Angeles   |      6,373 |   13,736.97 |      1,865 |   3,917.49  |
|     | WA | Seattle       |      6,098 |   12,760.64 |     2,121  |    4,444.06 |
|     | CA | San Francisco |        439 |     936.51  |        149 |      327.33 |
```
