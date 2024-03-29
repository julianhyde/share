---
layout: post
title: How should Mondrian get table and column statistics?
date: '2012-04-04T13:03:00.000-07:00'
author: Julian Hyde
tags:
modified_time: '2012-04-04T13:03:34.877-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-982458601210064772
blogger_orig_url: https://julianhyde.blogspot.com/2012/04/how-should-mondrian-get-table-and.html
---

When evaluating queries, Mondrian sometimes needs to make decisions
about how to proceed, and in particular, what SQL to generate. One
decision is which aggregate table to use for a query (or whether to
stick with the fact table), and another is whether to "round out" a
cell request for, say, 48 states and 10 months of 2011 to the full
segment of 50 states and 12 months.

These decisions are informed by the volume actual data in the
database. The first decision uses row counts (the numbers of rows in
the fact and aggregate tables) and the second uses column
cardinalities (the number of distinct values in the "month" and
"state" columns).

Gathering statistical information is an imperfect science. The obvious
way to get the information is to execute some SQL queries:

{% highlight sql %}
-- row count of the fact table
select count(*) from sales_fact_1997;

-- count rows in an aggregate table
select count(*) from agg_sales_product_brand_time_month;

-- cardinality of the [Customer].[State] attribute
select count(distinct state) from customer;
{% endhighlight %}

These queries can be quite expensive. (On many databases, a row count
involves reading every block of the table into memory and summing the
number of rows in each. A query for a column's cardinality involves an
entry scan of an index; or, worse, a table scan followed by an
expensive sort if there is no such index.)

Mondrian doesn't need the exact value, but need needs an approximate
value (say correct within a factor of 3) in order to proceed with the
query.

Mondrian has a statistics cache, so the statistics calls only affect
the "first query of the day", when Mondrian has been re-started, or is
using a new schema. (If you are making use of a dynamic schema
processor, it might be that every user effectively has their own
schema. In this case, every user will experience their own slow "first
query of the day".)

We have one mechanism to prevent expensive queries: you can provide
estimates in the Mondrian schema file. When you are defining an
aggregate table, specify the `approxRowCount` attribute of the
`<AggName>` XML element, and Mondrian will skip the row count
query. When defining a level, if you specify the `approxRowCount`
attribute of the `<Level>` XML element (the `<Attribute>` XML element
in mondrian-4), Mondrian will skip the cardinality query. But it is
time-consuming to fill in those counts, and they can go out of date as
the database grows.

I am mulling over a couple of features to ease this problem. (These
features are not committed for any particular release, or even fully
formed. Your feedback to this post will help us prioritize them, shape
them so that they are useful for how you manage Mondrian, and
hopefully trim their scope so that they are reasonably simple for us
to implement.)

### Auto-populate volume attributes

The auto-populate feature would read a schema file, run queries on the
database to count every fact table, aggregate table, and the key of
every level, and populate the `approxRowCount` attributes in the schema
file. It might also do some sanity checks, such as that the primary
key of your dimension table doesn't have any unique values, and warn
you if they are violated.

Auto-populate is clearly a time-consuming task. It might take an hour
or so to execute all of the queries. You could run it say once a
month, at a quiet time of day. But at the end, the Mondrian schema
would have enough information that it would not need to run any
statistics queries at run time.

Auto-populate has a few limitations. Obviously, you need to schedule
it, as a manual task, or a cron job. Then you need to make sure that
the modified schema file is propagated into the solution
repository. Lastly, if you are using a dynamic schema processor to
generate or significantly modify your schema file, auto-populate
clearly cannot populate sections that have not been generated yet.

### Pluggable statistics

The statistics that Mondrian needs probably already exist. Every
database has a query optimizer, and every query optimizer needs
statistics such as row counts and column cardinalities to make its
decisions. So, that `ANALYZE TABLE` (or equivalent) command that you
ran after you populated the database (you did run it, didn't you?)
probably calculated these statistics and stored them somewhere.

The problem is that that "somewhere" is different for each and every
database. In Oracle, they are in
[ALL_TAB_STATISTICS](https://docs.oracle.com/cd/B12037_01/server.101/b10755/statviews_1188.htm#i1591660)
and `ALL_TAB_COL_STATISTICS` tables; in MySQL, they are in
`INFORMATION_SCHEMA.STATISTICS`. And so forth.

JDBC claims to provide the information through the
[`DatabaseMetaData.getIndexInfo`](https://docs.oracle.com/javase/7/docs/api/java/sql/DatabaseMetaData.html#getIndexInfo(java.lang.String, java.lang.String, java.lang.String, boolean, boolean))
method. But it doesn't work for all drivers. (The only one I tried,
MySQL, albeit a fairly old version, didn't give me any row count
statistics.)

Let's suppose we introduced an SPI to get table and column
statistics:

{% highlight java %}
package mondrian.spi;

import javax.sql.DataSource;

interface StatisticsProvider {
  int getColumnCardinality(DataSource dataSource, String catalog,
      String schema, String table, String[] columns);
  int getTableCardinality(DataSource dataSource, String catalog,
      String schema, String table);
}
{% endhighlight %}

and several implementations:

* A fallback implementation `SqlStatisticsProvider` that generates
  `select count(distinct ...) ...` and `select count(*) ...`
  queries.
* An implementation `JdbcStatisticsProvider` that uses
  JDBC methods such as `getIndexInfo`.
* An implementation that uses each database's specific tables,
  `OracleStatisticsProvider`, `MySqlStatisticsProvider`, and so forth.
* Each `Dialect` could nominate one or more implementations of this
  SPI, and try them in order. (Each method can return -1 to say 'I
  don't know'.)

### Conclusion

Statistics are an important issue for Mondrian. In the real world,
missing statistics are more damaging than somewhat inaccurate
statistics. If statistics are inaccurate, Mondrian will execute
queries inefficiently, but the difference with optimal performance is
negligible if the statistics are within an order of magnitude; missing
statistics cause Mondrian to generate potentially expensive SQL
statements, especially during that all-important first query of the
day.

A couple of solutions are proposed.

The auto-population tool would solve the problem in one way, at the
cost of logistical effort to schedule the running of the tool.

The statistics provider leverages databases' own statistics. It solves
the problem of diversity the usual open source way: it provides an SPI
and lets the community provide implementations that SPI for their
favorite database.
