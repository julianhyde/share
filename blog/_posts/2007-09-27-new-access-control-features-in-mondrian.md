---
layout: post
title: New access-control features in mondrian
date: '2007-09-27T00:53:00.000-07:00'
author: Julian Hyde
tags:
modified_time: '2007-09-27T02:01:57.579-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-5373313658788544104
blogger_orig_url: https://julianhyde.blogspot.com/2007/09/new-access-control-features-in-mondrian.html
---

Data warehouse administrators often want to give different users
access to different subsets of the data. The OLAP model is a powerful
paradigm for exploring data, and this paradigm also allows complex
access-control rules to be expressed easily.

Mondrian already implements a rich set of access-control primitives
(see [mondrian schema guide](https://mondrian.pentaho.org/documentation/schema.php#Access_control)),
but we are considering some features to make
mondrian more expressive in future releases.

# Introduction: Access control in mondrian-2.4

In the current version of mondrian, roles are defined by `<role>`
elements in the mondrian schema file. A role can have access granted,
or denied, for cubes, hierarchies, and members of
hierarchies.

The type of access granted to an object is inherited by its
children. For example, if you deny access to a schema, then access to
cubes in that schema will be denied by default, but you can override
by granting access to specific cubes.

Most objects have just 'all' or 'none' access, but there is
finer-grained access for hierarchies. If a role has 'custom' access to
a hierarchy, it can spell out specific the members it has access
to.

Mondrian does not perform authentication, or have a concept
of a user. It is the responsibility of the container (usually a
webserver or application server) to check that the user is allowed to
access the mondrian instance, and to provide mondrian with a role
under which the user should execute queries.

# Deficiencies in access-control

There are several deficiencies in mondrian's current access control
model. Here are some of the major ones:

* **Partial cells**. Need a policy where if a role can only see some
  of the children of a member, the total for that member includes only
  accessible children.
* **Hidden cells**. Need a policy where if a role can only see some of
  the children of a member, the total for that member is not
  accessible.
* **Union roles**. If a user belongs to more than one role, there is
  no easy way to run queries in the combined privileges of the roles.
* **Intrinsic roles**. Need a way to define which members a role has
  access to using a rule rather than an explicit list of members in
  the schema file.
* **External roles**. Need a way to define roles outside of the schema
  file, say in a database table.

# Rollup policy

This proposed feature covers 1 'Partial cells' and 2 'Hidden cells'
above.

Suppose that Fred belongs to a role that can see `[USA].[CA]` and
`[USA].[OR]` but not `[USA].[WA]`. Fred runs the query

{% highlight sql %}
SELECT {[Measures].[Unit Sales]} ON COLUMNS,
  {[[Store].[USA], Store].[USA].Children} ON ROWS
FROM [Sales]
{% endhighlight %}

Under the current access-control policy,
called 'full', the query returns

```
           | Unit Sales
-----------+-----------
[USA]      |    266,773
[USA].[CA] |     74,748
[USA].[OR] | 67,659
```

Note that `[USA].[WA]` is not returned, per the access-control policy,
but the total includes the total from Washington (124,366) that Fred
cannot see. For some applications, this is not appropriate. In
particular, if the dimension has a small number of members, the
end-user may be able to deduce the values of the members which they do
not have access to.

To remedy this, a role can apply a 'rollup policy' to a hierarchy. The
policy describes how a total is calculated for a particular member if
the current role can only see some of that member's
children:

* **Full**. The total for that member includes all children. This is
  the only policy today, and will remain the default policy.
* **Partial**. The total for that member includes only accessible
  children.
* **Hidden*. If any of the children are inaccessible, the total is
  hidden.

Results under 'partial' policy:

```
           | Unit Sales
-----------+-----------
[USA]      |    142,407
[USA].[CA] |     74,748
[USA].[OR] |     67,659
```

Results under 'hidden' policy:

```
           | Unit Sales
-----------+-----------
[USA]      |          -
[USA].[CA] |     74,748
[USA].[OR] |     67,659
```

The policy is specified per role and hierarchy. In the following
example, the role sees partial totals for the `[Store]` hierarchy but
full totals for `[Product]`.

{% highlight xml %}
<Role name="South Pacific manager">
  <SchemaGrant access="none">
    <CubeGrant cube="Sales" access="all">
      <HierarchyGrant hierarchy="[Store]" access="custom"
          rollupPolicy="partial" topLevel="[Store].[Store Country]">
        <MemberGrant member="[Store].[USA].[CA]" access="all"/>
        <MemberGrant member="[Store].[USA].[CA].[Los Angeles]" access="none"/>
      </HierarchyGrant>
      <HierarchyGrant hierarchy="[Customers]" access="custom"
          rollupPolicy="full" topLevel="[Customers].[State Province]"
          bottomLevel="[Customers].[City]">
        <MemberGrant member="[Customers].[USA].[CA]" access="all"/>
        <MemberGrant member="[Customers].[USA].[CA].[Los Angeles]"
          access="none"/>
      </HierarchyGrant>
      <HierarchyGrant hierarchy="[Gender]" access="none"/>
    </CubeGrant>
  </SchemaGrant>
</Role>
{% endhighlight %}

This example also shows existing features, such as how hierarchy
grants can be restricted using `topLevel` and/or `bottomLevel`
attributes, and how a role can be prevented from seeing a hierarchy
using `access="none"`. See the schema guide for a description of these
features.

It is important that Mondrian applies access-control
transparently. Within a particular role and access-control scheme, the
value of a particular cell is always the same; it is not possible to
circumvent access-control by defining calculations, for instance.

# Intrinsic roles

Currently mondrian roles are **extrinsic**: they describe explicitly
the members which they can see. If the number of members is large, or
the set of members changes often, it is more convenient to define the
members by their properties.

An **intrinsic role** defines which members it can see according to
the properties of the member. For example, this role can see only
products whose color is red.

{% highlight xml %}
<Role name="Red Part manager">
  <SchemaGrant access="none">
    <CubeGrant cube="Sales" access="all">
      <HierarchyGrant hierarchy="[Product]" access="custom">
        <MemberGrant level="[Product].[Product Name]" access="all"/>
        <Expression>
          <![!CDATA[
            Filter([Product].[Product Name].Members,
                   [Product].Properties("Color") = 'red')
          ]]>
        </Expression>
    </HierarchyGrant>
{% endhighlight %}

As before, parent members are visible if one of their children is
visible, and the values of cells for those members are determined by
the rollup policy.

# Combining roles

Many systems have an access-control model where a user belongs to many
roles simultaneously; when a user attempts an action, such as
accessing a file or executing a query, the system either selects the
appropriate role or uses the sum of the privileges of all of the
roles. Mondrian does not work well with these systems, because we
require that queries are executed in precisely one role.

The obvious solution is for mondrian to automatically combine roles
roles when executing queries into one 'super role'. The semantics are
subtle. First each role figures out which members it can see (which,
for extrinsic roles may involve rules which say they **cannot** see
certain members), then the members visible to the roles are combined.

For example, suppose that role `R1` can see all cities in California
except San Francisco and Los Angeles, and role `R2` can see all cities
whose population is less than 1M. If Fred has roles `R1` and `R2`, can he
see San Francisco? Yes: `R1` cannot see San Francisco, `R2` can see it
because it has fewer than 1M inhabitants, so it is in the union of the
members in `R1` and `R2`.

# Discussion

The **partial** and **hidden** rollup policies are more expensive to
evaluate than **full**. Full requires just one point in
multidimensional space to be fetched from disk, which can be satisfied
using a SQL statement or a cache lookup, whereas partial and hidden
require a more complex MDX expression, sometimes involving many cells.

Various implementations for partial and hidden are possible. One
suggested scheme would add a special '`role_id`' column to aggregate
tables, which would contain a set of data for each role. This scheme
would likely perform well, but because it is custom made for access
control, it would tend to work against other strategies such as
caching and semi-joins.

The preferred implementation is one which leverages the strengths of
the existing MDX language. Some of these queries will require special
'tricks', not currently implemented in mondrian, to evaluate
efficiently, but at least these tricks will benefit a wide range of
queries beside access control, and therefore be thoroughly tested.

We plan to
implement the partial and hidden policies by having the schema-reader
introduce a calculated member for each partial total. For instance,
under a 'partial' policy, above query would be executed as if Fred had
written

{% highlight sql %}
WITH MEMBER [Store].[USA Partial]
  AS 'Aggregate([Store].[USA].Children)'
SELECT {[Measures].[Unit Sales]} ON COLUMNS,
  {[Store].[USA Partial], [Store].[USA].Children} ON ROWS
FROM [Sales]
{% endhighlight %}

In the naive evaluation strategy, the calculated member is expanded
away as mondrian recursively evaluates expressions. In a more
sophisticated strategy, the predicates which define the calculated
member -- including the access-control policies which filter the
results of the expression `[Store].[USA].Children` -- will be
generated into a SQL query.

Evaluating expressions in memory is inefficient when there are many
thousands of members. This is particularly easy to do when you can
define a role according to attributes of members, for example 'Red
products'. We need to find ways to optimize those kinds of queries, by
pushing the predicates down to SQL, and ideally by expressing the
expressions in a dimensional manner so that the results can be cached.

# Optimization #1: dimensional shift

This optimization involves recognizing that a set of members is being
filtered by a property which is also dimension. The filtered set can
be replaced by a member of the dimension.

For example,

{% highlight sql %}
Filter([Customer].[Name].Members,
       [Customer].CurrentMember.[Gender] = 'F')
{% endhighlight %}

becomes

{% highlight sql %}
[Gender].[F]
{% endhighlight %}

This optimization saves a scan over a large set of members, replacing
it with an access to a single cache cell.

# Optimization #2, push predicates down to SQL

In some filter expressions the predicate is too complex to be
converted a dimension. If the number of members in the set is large,
it is worthwhile to let the DBMS evaluate the entire expression. For
example,

{% highlight java %}
Filter(
  [Customer].[Name].Members,
  [Customer].[Gender] = 'F' or [Customer].[Age] > 50)
{% endhighlight %}

The 'or' prevents the dimensional shift from taking place. The
best optimization would be to (a) translate the condition into the SQL
`WHERE` clause

{% highlight sql %}
SELECT ...
FROM customer
WHERE gender = 'F' and age > 50
{% endhighlight %}

The DBMS can use evaluation techniques such as indexes, and it incurs
less I/O if we crunch the data as close to the disk as possible.

These kinds of predicates are very likely to occur when using
intrinsic access-control on hierarchies.

# Optimization #3, cache cells whose coordinates are complex members

After the above query has run, store the results in the cache. The
should fabricate special members like 'Red product' so that these
cells can be kept in the dimensional cache.

This optimization will benefit totals generated by access-controlled
hierarchies, but also totals over calculated sets such as top 10
customers, and other commonly occurring patterns.

# Conclusion

I have discussed some of the deficiencies in mondrian's current
access-control model, some features which could be introduced to
address them, and sketches for how those features could be
implemented. The 'rollup policy' feature is planned for mondrian-3.0
(see [mondrian roadmap](https://mondrian.pentaho.org/documentation/roadmap.php)),
but the other features are currently unplanned.
