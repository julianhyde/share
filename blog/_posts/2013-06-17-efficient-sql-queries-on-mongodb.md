---
layout: post
title: Efficient SQL queries on MongoDB
date: '2013-06-17T17:15:00.000-07:00'
author: Julian Hyde
tags:
modified_time: '2013-12-30T12:25:02.267-08:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-5125178184412212642
blogger_orig_url: https://julianhyde.blogspot.com/2013/06/efficient-sql-queries-on-mongodb.html
---

How do you integrate [MongoDB](https://www.mongodb.org/)
with other data in your organization?
MongoDB is great for building applications, and it has its own
powerful query API, but it's difficult to mash up data between MongoDB
and other tools, or to make tools that speak SQL, such as
[Pentaho Analysis (Mondrian)](https://mondrian.pentaho.com/), connect to MongoDB.

Building a SQL interface isn't easy, because MongoDB's data model is
such a long way from SQL's model. Here are some of the challenges:
* MongoDB doesn't have a schema. Each database has a number of named
  'collections', which are the nearest thing to a SQL table, but each
  row in a collection can have a completely different set of
  columns.
* In MongoDB, data can be nested. Each row consists of a number of
  fields, and each field can be a scalar value, null, a record, or an
  array of records.
* MongoDB supports a number of relational operations, but doesn't use
  the same terminology as SQL: the `find` method supports the
  equivalent of `SELECT` and `WHERE`, while the `aggregate` method
  supports the equivalent of `SELECT`, `WHERE`, `GROUP BY`, `HAVING`
  and `ORDER BY`.
* For efficiency, it's really important to push as much of the
  processing down to MongoDB's query engine, without the user having
  to re-write their SQL.
* But MongoDB doesn't support anything equivalent to
  `JOIN`.
* MongoDB can't access external data.

I decided to tackle this using
[Optiq](https://github.com/julianhyde/optiq).
Optiq already has a SQL parser and a
powerful query optimizer that is powered by rewrite rules. Building on
Optiq's core rules, I can add rules that map tables onto MongoDB
collections, and relational operations onto MongoDB's `find` and
`aggregate` operators.

What I produced is a effectively a JDBC driver for MongoDB. Behind it
is a hybrid query-processing engine that pushes as much of the query
processing down to MongoDB, and does whatever is left (such as joins)
in the client.

Let's give it a try. First, [install MongoDB](https://www.mongodb.org/downloads),
and import MongoDB's zipcode data set:

{% highlight bash %}
$ curl -o /tmp/zips.json https://media.mongodb.org/zips.json
$ mongoimport --db test --collection zips --file /tmp/zips.json
Tue Jun  4 16:24:14.190 check 9 29470
Tue Jun  4 16:24:14.469 imported 29470 objects
{% endhighlight %}

Log into MongoDB to check it's there:

{% highlight bash %}
$ mongo
MongoDB shell version: 2.4.3
connecting to: test
> db.zips.find().limit(3)
{ "city" : "ACMAR", "loc" : [ -86.51557, 33.584132 ], "pop" : 6055, "state" : "AL", "_id" : "35004" }
{ "city" : "ADAMSVILLE", "loc" : [ -86.959727, 33.588437 ], "pop" : 10616, "state" : "AL", "_id" : "35005" }
{ "city" : "ADGER", "loc" : [ -87.167455, 33.434277 ], "pop" : 3205, "state" : "AL", "_id" : "35006" }
> exit
bye
{% endhighlight %}

Now let's see the same data via SQL. Download and install Optiq:

{% highlight bash %}
$ git clone https://github.com/julianhyde/optiq.git
$ mvn install
{% endhighlight %}


Optiq comes with a sample model in JSON format, and the
[sqlline](https://github.com/julianhyde/sqlline) SQL shell.
Connect using the [mongo-zips-model.json](https://github.com/julianhyde/optiq/blob/master/mongodb/src/test/resources/mongo-zips-model.json)
Optiq model, and use sqlline's `!tables` command to list the available tables.

{% highlight bash %}
$ ./sqlline
sqlline> !connect jdbc:optiq:model=mongodb/target/test-classes/mongo-zips-model.json admin admin
Connecting to jdbc:optiq:model=mongodb/target/test-classes/mongo-zips-model.json
Connected to: Optiq (version 0.4.13)
Driver: Optiq JDBC Driver (version 0.4.13)
Autocommit status: true
Transaction isolation: TRANSACTION_REPEATABLE_READ
sqlline> !tables
+------------+--------------+-----------------+---------------+
| TABLE_CAT  | TABLE_SCHEM  |   TABLE_NAME    |  TABLE_TYPE   |
+------------+--------------+-----------------+---------------+
| null       | mongo_raw    | zips            | TABLE         |
| null       | mongo_raw    | system.indexes  | TABLE         |
| null       | mongo        | ZIPS            | VIEW          |
| null       | metadata     | COLUMNS         | SYSTEM_TABLE  |
| null       | metadata     | TABLES          | SYSTEM_TABLE  |
+------------+--------------+-----------------+---------------+
{% endhighlight %}


Each collection in MongoDB appears here as a table. There are also the
`COLUMNS` and `TABLES` system tables provided by Optiq, and a view
called `ZIPS` defined in `mongo-zips-model.json`.

Let's try a simple query. How many zip codes in America?

{% highlight sql %}
sqlline> SELECT count(*) FROM zips;
+---------+
| EXPR$0  |
+---------+
| 29467   |
+---------+
1 row selected (0.746 seconds)
{% endhighlight %}

Now a more complex one. How many states have a
city called Springfield?

{% highlight sql %}
sqlline> SELECT count(DISTINCT state) AS c FROM zips WHERE city = 'SPRINGFIELD';
+-----+
|   C |
+-----+
| 20  |
+-----+
1 row selected (0.549 seconds)
{% endhighlight %}


Let's use the SQL `EXPLAIN` command to see how the query is
implemented.

{% highlight sql %}
sqlline> !set outputformat csv
sqlline> EXPLAIN PLAN FOR
. . . .> SELECT count(DISTINCT state) AS c FROM zips WHERE city = 'SPRINGFIELD';

'PLAN'
'EnumerableAggregateRel(group=[{}], C=[COUNT($0)])
  EnumerableAggregateRel(group=[{0}])
    EnumerableCalcRel(expr#0..4=[{inputs}], expr#5=['SPRINGFIELD'], expr#6=[=($t0, $t5)], STATE=[$t3], $condition=[$t6])
      MongoToEnumerableConverter
        MongoTableScan(table=[[mongo_raw, zips]], ops=[[<{city: 1, state: 1, _id: 1}, {$project ...}>]])
'
1 row selected (0.115 seconds)
{% endhighlight %}



The last line of the plan shows that Optiq calls MongoDB's find
operator asking for the `city`, `state` and `_id` fields. The first
three lines of the plan show that the filter and aggregation are
implemented using in Optiq's built-in operators, but we're working on
pushing them down to MongoDB.

Finally, quit sqlline.

{% highlight sql %}
sqlline> !quit
Closing: net.hydromatic.optiq.jdbc.FactoryJdbc41$OptiqConnectionJdbc41
{% endhighlight %}

Optiq and its MongoDB adapter shown here are available on github. If
you are interested in writing your own adapter, check out
[optiq-csv](https://github.com/julianhyde/optiq-csv),
a sample adapter for Optiq that makes CSV files appear as tables. It has own
[tutorial](https://github.com/julianhyde/optiq-csv/blob/master/TUTORIAL.md)
on writing adapters.

Check back at this blog over the next few months, and I'll show how to
write views and advanced queries using Optiq, and how to use Optiq's
other adapters.
