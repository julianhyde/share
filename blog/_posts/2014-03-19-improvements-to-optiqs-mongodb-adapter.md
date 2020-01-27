---
layout: post
title: Improvements to Optiq's MongoDB adapter
date: '2014-03-19T13:39:00.002-07:00'
author: Julian Hyde
tags:
modified_time: '2014-03-19T13:39:37.039-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-34624433740046933
blogger_orig_url: https://julianhyde.blogspot.com/2014/03/improvements-to-optiqs-mongodb-adapter.html
---

It’s been a while since I posted to this blog, but I haven’t been
idle. Quite the opposite; I’ve been so busy writing code that I
haven’t had time to write blog posts. A few months ago I
[joined Hortonworks](http://hortonworks.com/blog/welcoming-julian-hyde/),
and I’ve been improving Optiq on several fronts, including
[several releases](https://github.com/julianhyde/optiq/blob/master/RELEASE.md#05--2014-03-14),
[adding a cost-based optimizer to Hive](https://issues.apache.org/jira/browse/HIVE-6439)
and some other initiatives to make Hadoop faster and smarter.

More about those other initiatives shortly. But Optiq’s mission is to
improve access to all data, so here I want to talk about improvements
to how Optiq accesses data in MongoDB. Optiq can now translate SQL
queries to extremely efficient operations inside MongoDB.

MongoDB 2.2 introduced the
[aggregation framework](https://docs.mongodb.org/manual/core/aggregation-introduction/),
which allows you to compose queries as pipelines of
operations. They have basically implemented relational algebra, and we
wanted to take advantage of this.

As the following table shows, most of those operations map onto
Optiq’s relational operators. We can exploit that fact to push SQL
query logic down into MongoDB.

| MongoDB operator | Optiq operator
| ---------------- | --------------
| $project         | ProjectRel
| $match           | FilterRel
| $limit           | SortRel.limit
| $skip            | SortRel.offset
| $unwind          | -
| $group           | AggregateRel
| $sort            | SortRel
| $geoNear         | -

A [bug](https://issues.apache.org/jira/browse/CALCITE-164)
pointed out that it would be more efficient if we evaluated `$match`
before `$project`. As I fixed that bug yesterday, I decided
to push down limit and offset operations. (In Optiq, these are just
attributes of a `SortRel`; a `SortRel` sorting on 0 columns can be
created if you wish to apply limit or offset without sorting.)

That went well, so I decided to go for the prize: pushing down
aggregations. This is a big performance win because the output of a
`GROUP BY` query is often a lot smaller than its input. It is much
more efficient for MongoDB aggregate the data in memory, returning a
small result, than to return a large amount of raw data to be
aggregated by Optiq.

Now queries involving `SELECT`, `FROM`, `WHERE`, `GROUP BY`, `HAVING`,
`ORDER BY`, `OFFSET`, `FETCH` (or `LIMIT` if you prefer the
PostgreSQL-style syntax), not to mention sub-queries, can be evaluated
in MongoDB. (`JOIN`, `UNION`, `INTERSECT`, `MINUS` cannot be pushed
down because MongoDB does not support those relational operators;
Optiq will still evaluate those queries, pushing down as much as it
can.)

Let's see some examples of push-down in action.

Given the query:

{% highlight sql %}
SELECT state, COUNT(*) AS c
FROM zips
GROUP BY state
{% endhighlight %}

Optiq evaluates:

{% highlight mongodb %}
db.zips.aggregate(
   {$project: {STATE: ‘$state’}},
   {$group: {_id: ‘$STATE’, C: {$sum: 1}}},
   {$project: {STATE: ‘$_id’, C: ‘$C’}})
{% endhighlight %}

and returns

```
STATE=WV; C=659
STATE=WA; C=484
```

Now let’s add a `HAVING` clause to find out which states have more
than 1,500 zip codes:

{% highlight sql %}
SELECT state, COUNT(*) AS c
FROM zips
GROUP BY state
HAVING COUNT(*) > 1500
{% endhighlight %}

Optiq adds a `$match` operator to the previous query's pipeline:

{% highlight mongodb %}
db.zips.aggregate(
   {$project: {STATE: ‘$state’}},
   {$group: {_id: ‘$STATE’, C: {$sum: 1}}},
   {$project: {STATE: ‘$_id’, C: ‘$C’}},
   {$match: {C: {$gt: 1500}}})
{% endhighlight %}

and returns

```
STATE=NY; C=1596
STATE=TX; C=1676
STATE=CA; C=1523
```

Now the *pièce de résistance*. The following query finds the top 5
states in terms of number of cities (and remember that each city can
have many zip-codes).

{% highlight sql %}
SELECT state, COUNT(DISTINCT city) AS cdc
FROM zips
GROUP BY state
ORDER BY cdc DESC
LIMIT 5
{% endhighlight %}

`COUNT(DISTINCT {column})` is difficult to implement because it
requires the data to be aggregated twice -- once to compute the set of
distinct values, and once to count them within each group. For this
reason, MongoDB doesn’t implement distinct aggregations. But Optiq
translates the query into a pipeline with two `$group` operators. For
good measure, we throw in `ORDER BY` and `LIMIT` clauses.

The result is an awe-inspiring pipeline that includes two `$group`
operators (implementing the two phases of aggregation for
distinct-count), and finishes with `$sort` and `$limit`.

{% highlight mongodb %}
db.zips.aggregate(
  {$project: {STATE: '$state', CITY: '$city'}},
  {$group: {_id: {STATE: '$STATE', CITY: '$CITY'}}},
  {$project: {_id: 0, STATE: '$_id.STATE', CITY: '$_id.CITY'}},
  {$group: {_id: '$STATE', CDC: {$sum: {$cond: [ {$eq: ['CITY', null]}, 0, 1]}}}},
  {$project: {STATE: '$_id', CDC: '$CDC'}},
  {$sort: {CDC: -1}},
  {$limit: 5})
{% endhighlight %}

I had to jump through some hoops to get this far, because MongoDB’s
expression language can be baroque. In one case I had to generate

{% highlight mongodb %}
{$ifNull: [null, 0]}
{% endhighlight %}

in order to include the constant 0 in a `$project` operator. And I was
foiled by MongoDB bug
[SERVER-4589](https://jira.mongodb.org/browse/SERVER-4589)
when trying to access the values inside the `zips` table's `loc`
column, which contains (latitude, longitude) pairs represented as an
array.

In conclusion, Optiq on MongoDB now does a lot of really smart
stuff. It can evaluate any SQL query, and push down a lot of that
evaluation to be executed efficiently inside MongoDB.

I encourage you to
[download Optiq](https://github.com/julianhyde/optiq) and try
running some sophisticated SQL queries (including those generated by
the OLAP engine I authored,
[Mondrian](https://community.pentaho.com/projects/mondrian/)).
