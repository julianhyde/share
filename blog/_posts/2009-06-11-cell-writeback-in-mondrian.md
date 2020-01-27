---
layout: post
title: Cell writeback in Mondrian
date: '2009-06-11T18:18:00.000-07:00'
author: Julian Hyde
tags:
- mondrian mdx writeback
modified_time: '2009-06-22T10:18:41.110-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-2859338247682311979
blogger_orig_url: https://julianhyde.blogspot.com/2009/06/cell-writeback-in-mondrian.html
---

Writeback is a feature that allows you to modify OLAP cell values and
see the effects ripple through the data set, automatically modifying
child and parent cells, and also cells derived using
calculations. This allows you to perform 'what if' analysis and
applications such as budgeting.

I have added experimental support for writeback to Mondrian.

In Mondrian's case, the term 'writeback' is a bit misleading. In a
ROLAP system such as Mondrian, writing back to the database would be
difficult, since values are stored in a fact table but we allow cells
of any granularity to be modified. One modified cell might contain
thousands of fact table rows. So, we don't write cells back to the
database, but just retain the modified cells in memory, and propagate
the modifications to related cells.

Here's how to use the experimental writeback support. Some of the
details may change later as we make the feature more
usable.

First, enable writeback for your
cube. Create a dimension called '`Scenario`', and a measure called
'`Atomic Cell Count`':

{% highlight xml %}
<Cube name='Sales'>
  <Dimension name='Scenario' foreignKey='time_id'>
    <Hierarchy primaryKey='time_id' hasall='true'>
      <InlineTable alias='_dummy'>
        <ColumnDefs>
          <ColumnDef name='foo' type='Numeric'/>
        </ColumnDefs>
        <Rows/>
      </InlineTable>
      <Level name='Scenario' column='foo'/>
    </Hierarchy>
  </Dimension>
  <!-- Other dimensions... -->
  <Measure name='Atomic' aggregator='count'/>
  <!-- Other measures... -->
</Cube>
{% endhighlight %}

(Yes, this is a lot of crud to add to your cube definition, and it's
temporary. In future, we will let you flag a cube as 'writeback
enabled', and a `[Scenario]` dimension and `[Atomic Cell Count]`
measure will be created automatically. Also, we will make it easier
for you to create dimensions that have only calculated members,
without resorting to inline tables.)

Next, create a Scenario:

{% highlight java %}
Connection connection;
Scenario scenario = connection.createScenario();
int scenarioId = scenario.getId();
{% endhighlight %}

(The Scenario API will soon move to [olap4j](http://www.olap4j.org/):
before mondrian-4.0, I
hope. This includes the class `mondrian.olap.Scenario`, the method
`mondrian.olap.Cell.setValue()`, and the method
`mondrian.olap.Connection.createScenario()`. It will be optional for
an olap4j driver to support writeback, but Mondrian's olap4j driver
will, of course.)


Write a query that uses the scenario. Assuming that `scenarioId` above
was 1, the query

{% highlight sql %}
SELECT [Measures].[Unit Sales] ON COLUMNS,
  {[Product],
   [Product].Children,
   [Product].[Drink].Children} ON ROWS
FROM [Sales]
WHERE [Scenario].[1]
{% endhighlight %}

returns

```
[Product]                 [Unit Sales]
========================= ============
(All)                          266,773
 + Drink                        24,597
 +--+ Alcoholic Beverages        6,838
 +--+ Beverages                 13,573
 +--+ Dairy                      4,186
 + Food                        191,940
 + Non-Consumable               50,236
```

Choose one of the cells returned from the query and modify its
value. For example, let's reduce the sales of `Drink` by 1,000 from
24,597 to 23,597:

{% highlight java %}
Result result = connection.executeQuery(...);
Cell cell = result.getCell(new int[] {0, 1});
cell.setValue(23597, AllocationPolicy.EQUAL_ALLOCATION);
{% endhighlight %}

Execute the query again, and it returns

```
[Product]                 [Unit Sales]
========================= ============
(All)                          265,773
 + Drink                        23,597
 +--+ Alcoholic Beverages        6,563
 +--+ Beverages                 12,990
 +--+ Dairy                      4,043
 + Food                        191,940
 + Non-Consumable               50,236
```

The value for `Drink` is 23,597, as expected, and the values of its
children have been correspondingly reduced.

How the value is allocated to the children (and in fact all
descendants) is decided by the allocation policy. In this case, we
specified `EQUAL_ALLOCATION`, which means that all atomic cells have
the same value.

An atomic cell is the finest grained value that can be viewed
multidimensionally; for this cube, it is an instance of a particular
customer buying a particular product, on a particular promotion, on a
particular day, in a particular store. That makes for an awful lot of
of atomic cells, but there may be fewer atomic cells than fact table
rows. If the fact table does not have a primary key on (customer,
product, time, promotion, store) some cells may have more than one
fact table row.

If instead we had
written

{% highlight java %}
cell.setValue(23597, AllocationPolicy.EQUAL_INCREMENT);
{% endhighlight %}

the query would have returned
```
[Product]                 [Unit Sales]
========================= ============
(All)                          265,773
 + Drink                        23,597
 +--+ Alcoholic Beverages        6,560
 +--+ Beverages                 13,022
 +--+ Dairy                      4,015
 + Food                        191,940
 + Non-Consumable               50,236
```

We notice that `Beverages` has not been reduced as much under
`EQUAL_INCREMENT` policy than `EQUAL_ALLOCATION` policy; the average
value for atomic cells of Beverages must be greater than for Drink as
a whole.

Allocation policies are defined consistent with Analysis Services'
[UPDATE CUBE statement](https://technet.microsoft.com/en-us/library/ms145488.aspx).
Mondrian does not currently implement
`WEIGHTED_ALLOCATION` or `WEIGHTED_INCREMENT` policies.

Treating scenarios as a dimension is an elegant and powerful
idea. Using the `Scenario` dimension, you can easily switch from one
scenario to another, or you can compare scenarios side-by-side.

Note that you can also set a connection's current scenario. This
effectively becomes the default value for the `Scenario` dimension in
that connection, so you do not need to specify Scenario in the
slicer. However, there still needs to be an explicit scenario in the
context when you call `Cell.setValue()`. I'm not sure whether the
benefit of having a scenario for a connection outweighs the
benefit/confusion, and we may discontinue this feature.

Remember, this is still an experimental feature. There is some cleanup
to be done, some performance tuning, and the API needs to be moved
into olap4j. But most importantly, it's not useful until a user
interface, such as PAT or JPivot, supports scenarios and modifying
cell values.
