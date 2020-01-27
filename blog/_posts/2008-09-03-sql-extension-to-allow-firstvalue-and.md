---
layout: post
title: SQL extension to allow FIRST_VALUE and LAST_VALUE in GROUP BY query
date: '2008-09-03T17:27:00.000-07:00'
author: Julian Hyde
tags:
- sql standard extensions streaming
modified_time: '2008-09-04T13:59:26.665-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-1422035625145420945
blogger_orig_url: https://julianhyde.blogspot.com/2008/09/sql-extension-to-allow-firstvalue-and.html
---

At SQLstream, we have come across an interesting query pattern that
seems to be difficult to express in standard SQL (SQL:2003 or
SQL:2008). It turns out to be applicable to regular SQL as well as
streaming SQL, and therefore it would make sense as an extension to
the SQL standard.

First some background, for those of you who don't fall asleep every
night reading the SQL standard. There are two kinds of aggregation in
standard SQL: windowed aggregation, of the form

```
function(arg {, arg... }) OVER window-specification
```

and grouped aggregation, which is of the form

```
function(arg {, arg... })
```

and requires a `GROUP BY` clause. (If the `GROUP BY` clause is not
present, `GROUP BY ()` is assumed.)

According to the standard, these two forms should never meet. It is
illegal to use a windowed aggregation in a `SELECT` that has a `GROUP
BY`, or to mix grouped aggregation and windowed aggregation in the
same `SELECT`. (It's OK to use one in a sub-query and another in an
enclosing query.)

However, here is a very reasonable query that is difficult to express
in standard SQL: Given a record of every trade on a stock exchange,
give me the volume and closing price of each ticker symbol. You might
try

{% highlight sql %}
SELECT day,
  ticker,
  SUM(shares) AS volume,
  LAST_VALUE(price) AS closingPrice
FROM Trades
GROUP BY day, ticker
{% endhighlight %}

but this is illegal SQL. Why is it illegal? Because the `LAST_VALUE`
function (like `FIRST_VALUE` and `RANK`) is a windowed aggregate
function and is only meaningful on an ordered set.

To introduce the notion of ordering, I propose that the following
query should be valid:

{% highlight sql %}
SELECT day,
  ticker,
  SUM(shares) AS volume,
  LAST_VALUE(price) OVER (ORDER BY timeOfDay) AS closingPrice
FROM Trades
GROUP BY day, ticker
{% endhighlight %}

With the `OVER` clause, `LAST_VALUE` is now a
windowed aggregate function within the context of a `GROUP BY` query,
which was previously illegal. Every windowed aggregate is applied to a
window, so what is the window in this case? We want the window to
contain all of the rows with the same day and ticker value, and to be
sorted by `timeOfDay`. In other words, the window inherits the `GROUP BY`
columns as its implicit `PARTITION BY` clause. It is as if they had
written

{% highlight sql %}
LAST_VALUE(price) OVER (PARTITION BY day, ticker ORDER BY timeOfDay)
{% endhighlight %}

Now, if you know that I work for SQLstream, you will guess that I am
motivated to make this work for streaming queries. A streaming
aggregation query over the `Trades` stream would look like this:

{% highlight sql %}
SELECT STREAM day,
  ticker,
  SUM(shares) AS volume,
  LAST_VALUE(price) OVER (ORDER BY timeOfDay) AS closingPrice
FROM Trades
GROUP BY day, ticker
{% endhighlight %}

This is identical to the traditional, non-streaming SQL above, except
for the `STREAM` keyword that tells SQLstream that the result of the
query should be a stream.

In idiomatic SQLstream SQL, we would typically express the query as follows:

{% highlight sql %}
SELECT STREAM FLOOR(t.ROWTIME TO DAY),
  ticker,
  SUM(shares) AS volume,
  LAST_VALUE(price) OVER () AS closingPrice
FROM Trades AS t
GROUP BY FLOOR(t.ROWTIME TO DAY), ticker
{% endhighlight %}

This form uses SQLstream's system `ROWTIME` column and the
`FLOOR(datetime expression TO time unit)` operator, and so can
dispense with the `day` and `timeOfDay` columns. Also, streams are
ordered by `ROWTIME` by default, so we can abbreviate `OVER (ORDER BY
ROWTIME)` to `OVER ()`. This form is more terse, and more typical of
how the query would be written in a SQLstream application, but the
previous form works also.

The end result is powerful and, I think, consistent with the spirit of
standard SQL.
