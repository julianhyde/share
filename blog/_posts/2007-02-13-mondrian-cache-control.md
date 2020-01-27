---
layout: post
title: Mondrian cache control
date: '2007-02-13T18:37:00.000-08:00'
author: Julian Hyde
tags:
modified_time: '2009-04-18T13:52:34.524-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-5055608065354412098
blogger_orig_url: https://julianhyde.blogspot.com/2007/02/mondrian-cache-control.html
---

One of the strengths of mondrian's design is that you don't need to do
any processing to populate special data structures before you start
running OLAP queries. More than a few people have observed that this
makes mondrian an excellent choice for 'real-time OLAP' -- running
multi-dimensional queries on a database which is constantly changing.

The problem is that mondrian's cache gets in the way. Usually the
cache is a great help, because it ensures that mondrian only goes to
the DBMS once for a given piece of data, but the cache becomes out of
date if the underlying database is changing.

This is solved with a new set of APIs for cache control in
mondrian-2.3. Before I explain the API, let's understand how mondrian
caches data.

## How mondrian's cache works

Mondrian's cache ensures that once a multidimensional cell -- say the
Unit Sales of Beer in Texas in Q1, 1997 -- has been retrieved from the
DBMS using an SQL query, it is retained in memory for subsequent MDX
calculations. That cell may be used later during the execution of the
same MDX query, and by future queries in the same session and in other
sessions. The cache is a major factor ensuring that Mondrian is
responsive for speed-of-thought analysis.

The cache operates at a lower level of abstraction than access
control. If the current role is only permitted to see only sales of
Dairy products, and the query asks for all sales in 1997, then the
request sent to Mondrian's cache will be for Dairy sales in 1997. This
ensures that the cache can safely be shared among users which have
different permissions.

If the contents of the DBMS change while Mondrian is running,
Mondrian's implementation must overcome some challenges. The end-user
expects a speed-of-thought query response time yielding a more or less
up-to-date view of the database. Response time necessitates a cache,
but this cache will tend to become out of date as the database is
modified.

Mondrian cannot deduce when the database is being modified, so we
introduce an API so that the container can tell Mondrian which parts
of the cache are out of date. Mondrian's implementation must ensure
that the changing database state does not yield inconsistent query
results.

Until now, control of the cache has been very crude: applications
would typically call

{% highlight java %}
mondrian.rolap.RolapSchema.clearCache();
{% endhighlight %}

to flush the cache which maps connect string URLs to in-memory
datasets. The effect of this call is that a future connection will
have to re-load metadata by parsing the schema XML file, and then load
the data afresh.

There are a few problems with this approach. Flushing all data and
metadata is all appropriate if the contents of a schema XML file has
changed, but we have thrown out the proverbial baby with the
bath-water. If only the data has changed, we would like to use a
cheaper operation.

The final problem with the `clearCache()` method is that it affects
only new connections. Existing connections will continue to use the
same metadata and stale data, and will compete for scarce memory with
new connections.

## New CacheControl API

The new `CacheControl` API solves all of the problems described
above. It provides fine-grained control over data in the cache, and
the changes take place as soon as possible while retaining a
consistent view of the data.

When a connection uses the API to notify Mondrian that the database
has changed, subsequent queries will see the new state of the
database. Queries in other connections which are in progress when the
notification is received will see the database state either before or
after the notification, but in any case, will see a consistent view of
the world.

The cache control API uses the new concept of a *cache region*, an
area of multidimensional space defined by one or more members. To
flush the cache, you first define a cache region, then tell Mondrian
to flush all cell values which relate to that region. To ensure
consistency, Mondrian automatically flushes all rollups of those
cells.

### A simple example

Suppose that a connection has executed a query:

{% highlight java %}
import mondrian.olap.*;

Connection connection;
Query query = connection.parseQuery(
  "SELECT" +
  "  {[Time].[1997]," +
  "    [Time].[1997].Children} ON COLUMNS," +
  "  {[Customer].[USA]," +
  "    [Customer].[USA].[OR]," +
  "    [Customer].[USA].[WA]} ON ROWS" +
  "FROM [Sales]");
Result result = connection.execute(query);
{% endhighlight %}

and that this has populated the cache with the following segments:

Segment YN#1
```
Year Nation Unit Sales
1997 USA    xxx

Predicates: Year=1997, Nation=USA
```

Segment YNS#1
```
Year Nation State Unit Sales
1997 USA    OR    xxx
1997 USA    WA    xxx

Predicates: Year=1997, Nation=USA, State={OR, WA}
```

Segment YQN#1
```
Year Quarter Nation Unit Sales
1997 Q1      USA    xxx
1997 Q2      USA    xxx

Predicates: Year=1997, Quarter=any, Nation=USA
```

Segment YQNS#1
```
Year Quarter Nation State Unit Sales
1997 Q1      USA    OR    xxx
1997 Q1      USA    WA    xxx
1997 Q2      USA    OR    xxx
1997 Q2      USA    WA    xxx

Predicates: Year=1997, Quarter=any, Nation=USA, State={OR, WA}
```

Now suppose that the application knows that batch of rows from Oregon,
Q2 have been updated in the fact table. The application notifies
Mondrian of the fact by defining a cache region:

{% highlight java %}
// Lookup members
Cube salesCube =
  connection.getSchema().lookupCube("Sales", true);
SchemaReader schemaReader =
  salesCube.getSchemaReader(null);
Member memberTimeQ2 =
  schemaReader.getMemberByUniqueName(
    new String[] {"Time", "1997", "Q2"},
    true);
Member memberCustomerOR =
  schemaReader.getMemberByUniqueName(
    new String[] {"Customer", "USA", "OR"},
    true);

// Create a cache region defined by
// [Time].[1997].[Q2] cross join
// [Customer].[USA].[OR].
CacheControl.CellRegion measuresRegion =
  cacheControl.createMeasuresRegion(salesCube);
CacheControl.CellRegion regionTimeQ2 =
  cacheControl.createMemberRegion(memberTimeQ2, true);
CacheControl.CellRegion regionCustomerOR =
  cacheControl.createMemberRegion(memberCustomerOR, true);
CacheControl.CellRegion regionOregonQ2 =
  cacheControl.createCrossjoinRegion(measuresRegion,
    regionCustomerOR, regionTimeQ2);
{% endhighlight %}

and flushing that region:

{% highlight java %}
cacheControl.flush(regionOregonQ2);
{% endhighlight %}

Now let's look at what segments are left in memory after the flush.

Segment YNS#1
```
Year Nation State Unit Sales
1997 USA    OR    xxx
1997 USA    WA    xxx

Predicates: Year=1997, Nation=USA, State={WA}
```

Segment YQN#1
```
Year Quarter Nation Unit Sales
1997 Q1      USA    xxx
1997 Q2      USA    xxx

Predicates: Year=1997, Quarter={any except Q2}, Nation=USA
```

Segment YQNS#1
```
Year Quarter Nation State Unit Sales
1997 Q1      USA    OR    xxx
1997 Q1      USA    WA    xxx
1997 Q2      USA    OR    xxx
1997 Q2      USA    WA    xxx

Predicates: Year=1997, Quarter=any, Nation=USA, State={WA}
```

The effects are:

* Segment YN#1 has been deleted. All cells in the segment could
  contain values in Oregon/1997/Q2.
* The constraints in YNS#1 have been strengthened. The constraint on
  the State column is modified from State={OR, WA} to State={WA} so
  that future requests for (1997, Q2, USA, OR) will not consider this
  segment.
* The constraints in YQN#1 have been strengthened. The constraint on
  the Quarter column is modified from Quarter=any to Quarter={any
  except Q2}.
* The constraints in YQNS#1 have been strengthened, similar to YNS#1.

# More about cell regions

The previous example showed how to make a cell region consisting of a
single member, and how to combine these regions into a two-dimensional
region using a crossjoin. The CacheControl API supports several
methods of creating regions:

* `createMemberRegion(Member, boolean)` creates a region containing a
  single member, optionally including its descendants.
* `createMemberRegion(boolean lowerInclusive, Member lowerMember,
  boolean upperInclusive, Member upperMember, boolean descendants)`
  creates a region containing a range of members, optionally including
  their descendants, and optionally including each endpoint. A range
  may be either closed, or open at one end.
* `createCrossjoinRegion(CellRegion...)` combines several regions into
  a higher dimensionality region. The constituent regions must not
  have any dimensions in common.
* `createUnionRegion(CellRegion...)` unions several regions of the
  same dimensionality.
* `createMeasuresRegion(Cube)` creates a region containing all of the
  measures of a given cube.

The second overloading of `createMemberRegion()` is interesting
because it allows a range of members to be flushed. Probably the most
common use case for cache flush -- flushing all cells since a given
point in time -- is expressed as a member range. For example, to flush
all cells since February 15th, 2006, you would use the following code:

{% highlight java %}
// Lookup members
Cube salesCube =
  connection.getSchema().lookupCube("Sales", true);
SchemaReader schemaReader = salesCube.getSchemaReader(null);
Member memberTimeOct15 =
  schemaReader.getMemberByUniqueName(
    new String[] {"Time", "2006", "Q1"", "2" ,"15}, true);

// Create a cache region defined by
// [Time].[1997].[Q1].[2].[15] to +infinity.
CacheControl.CellRegion measuresRegion =
  cacheControl.createMeasuresRegion(salesCube);
CacheControl.CellRegion regionTimeFeb15 =
  cacheControl.createMemberRegion(true, memberTimeFeb15,
    false, null, true);
{% endhighlight %}

Recall that the cell cache is organized in terms of columns, not
members. This makes member ranges difficult for mondrian to
implement. A range such as "February 15th 2007 onwards" becomes

{% highlight sql %}
year > 2007
or (year = 2007
    and (quarter > 'Q1'
         or (quarter = 'Q1'
             and (month > 2
                  or (month = 2
                      and day >= 15)))))
{% endhighlight %}

# Merging and truncating segments

The current implementation does not actually remove the cells from
memory. For instance, in segment YNS#1 in the example above, the cell
(1997, USA, OR) is still in the segment, even though it will never be
accessed. It doesn't seem worth the effort to rebuild the segment to
save a little memory, but we may revisit this decision.

In future, one possible strategy would be to remove a segment if more
than a given percentage of its cells are unreachable.

It might also be useful to be able to merge segments which have the
same dimensionality, to reduce fragmentation if the cache is flushed
repeatedly over slightly different bounds. There are some limitations
on when this can be done, since predicates can only constrain one
column: it would not be possible to merge the segments {(State=TX,
Quarter=Q2)} and {(State=WA, Quarter=Q3)} into a single segment, for
example. An alternative solution to fragmentation would be to simply
remove all segments of a particular dimensionality if fragmentation is
detected.

# Flushing the dimension cache

An application might also want to make modifications to a dimension
table. Mondrian does not currently allow an application to control the
cache of members, but we intend to do so in the future. Here are some
notes which will allow this to be implemented.

The main way that Mondrian caches dimensions in memory is via a cache
of member children. That is to say, for a given member, the cache
holds the list of all children of that member.

If a dimension table row was inserted or deleted, or if its key
attributes are updated, its parent's child list would need to be
modified, and perhaps other ancestors too. For example, if a customer
Zachary William is added in city Oakland, the children list of Oakland
will need to be flushed. If Zachary is the first customer in Oakland,
California's children list will need to be flushed to accommodate the
new member Oakland.

There are a few other ways that members can be cached:

* Each hierarchy has a list of root members, an 'all' member (which
  may or not be visible), and a default member (which may or may not
  be the 'all' member).
* Formulas defined against a cube may reference members.
* All other references to members are ephemeral: they are built up
  during the execution of a query, and are discarded when the query
  has finished executing and its result set is forgotten.

Possible APIs might be `flushMember(Member, boolean children)` or
`flushMembers(CellRegion)`.

# Consistency

Mondrian's cache implementation must solve several challenges in order
to prevent inconsistent query results. Suppose, for example, a
connection executes the query

{% highlight sql %}
SELECT {[Measures].[Unit Sales]} ON COLUMNS,
  {[Gender].Members} ON ROWS
FROM [Sales]
{% endhighlight %}

It would be unacceptable if, due to updates to the underlying
database, the query yielded a result where the total for `[All gender]`
did not equal the sum of `[Female]` and `[Male]`, such as

```
            Unit sales
=========== ==========
All gender     100,000
Female          60,000
Male            55,000
```

We cannot guarantee that the query result is absolutely up to date,
but the query must represent the state of the database at some point
in time. To do this, the implementation must ensure that both cache
flush and cache population are atomic operations.

First, Mondrian's implementation must provide **atomic cache flush**
so that from the perspective of any clients of the cache. Suppose that
while the above query is being executed, another connection issues a
cache flush request. Since the flush request and query are
simultaneous, it is acceptable for the query to return the state of
the database before the flush request or after, but not a mixture of
the two.

The query needs to use two aggregates: one containing total
sales, and another containing sales sliced by gender. To see a
consistent view of the two aggregates, the implementation must ensure
that from the perspective of the query, both aggregates are flushed
simultaneously. The query evaluator will therefore either see both
aggregates, or see none.

Second, Mondrian must provide **atomic cache
population**, so that the database is read consistently. Consider
an example.

1. The end user runs a query asking for the total
sales:
```
            Unit sales
=========== ==========
All gender     100,000
```
After that query has completed, the cache contains the total sales but
not the sales for each gender.
2. New sales are added to the fact table.
3. The end user runs a query which shows total sales and sales for
   male and female customers. The query uses the cached value for
   total sales, but issues a query to the fact table to find the
   totals for male and female, and sees different data than when the
   cache was last populated. As result, the query is inconsistent:
```
            Unit sales
========== ===========
All gender     100,000
Female          60,000
Male            55,000
```

Atomic cache population is difficult to ensure if the
database is being modified without Mondrian's knowledge. One solution,
not currently implemented, would be for Mondrian to leverage the DBMS'
support for read-consistent views of the data. Read-consistent views
are expensive for the DBMS to implement (for example, in Oracle they
yield the infamous 'Snapshot too old' error), so we would not want
Mondrian to use these by default, on a database which is known not to
be changing.

Another solution might be to extend the Cache Control API so that the
application can say 'this part of the database is currently undergoing
modification'.

This scenario has not even considered aggregate
tables. We have assumed that aggregate tables do not exist, or if they
do, they are updated in sync with the fact table. How to deal with
aggregate tables which are maintained asynchronously is still an open
question.

## Metadata cache control

The `CacheControl` API tidies up a raft of (mostly equivalent) methods
which had grown up for controlling metadata (schema XML files loaded
into memory). The methods
* `mondrian.rolap.RolapSchema.clearCache()`
* `mondrian.olap.MondrianServer.flushSchemaCache()`
* `mondrian.rolap.cache.CachePool.flush()`
* `mondrian.rolap.RolapSchema.flushRolapStarCaches(boolean)`
* `mondrian.rolap.RolapSchema.flushAllRolapStarCachedAggregations()`
* `mondrian.rolap.RolapSchema.flushSchema(String,String,String,String)`
* `mondrian.rolap.RolapSchema.flushSchema(DataSource,String)`

are all deprecated and are superseded by the CacheControl methods

{% highlight java %}
void flushSchemaCache();

void flushSchema(
  String catalogUrl,
  String connectionKey,
  String jdbcUser,
  String dataSourceStr);

void flushSchema(
  String catalogUrl,
  DataSource dataSource);
{% endhighlight %}

# Conclusion

The new CacheControl API will be available in mondrian-2.3, which is
in the final stages before its release. (The first release candidate
will be released in about a week from now, 20th February 2007. The
source code is in the perforce repository, for those brave enough to
download and build from source.)

Give it a try, and let us know how it works with your application.
