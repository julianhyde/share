---
layout: post
title: Pentaho 2.0 brings good things
date: '2008-10-22T03:35:00.000-07:00'
author: Julian Hyde
tags:
- pentaho puc aggregate designer mondrian summary table
modified_time: '2008-10-23T05:03:01.181-07:00'
thumbnail: https://4.bp.blogspot.com/_BVv0WTpeWTs/SP8FLyIXrYI/AAAAAAAAABQ/KYlKC-PuSWI/s72-c/puc.png
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-7801524386643335067
blogger_orig_url: https://julianhyde.blogspot.com/2008/10/pentaho-20-brings-good-things.html
---

This week [Pentaho released version 2.0 of its BI Suite](https://www.pentaho.com/the_alternative/),
and it contains two major features that the mondrian community will
love.

First, the Pentaho User Console, a web-based environment where end
users can create, view, save, and share BI content. Content is
arranged into folders, and includes operational reports created with
Pentaho Reports, and dimensional analytics created with Pentaho
Analysis (mondrian). Users can also create subscriptions to receive
reports regularly by email. PUC is simple and elegant. I predict that
it will quickly become the face of Pentaho for end users.

<a onblur="try {parent.deselectBloggerImageGracefully();} catch(e) {}" href="/assets/img/puc.png">
  <img style="margin: 0px auto 10px; display: block; text-align: center; cursor: pointer;"
    src="/assets/img/puc-thumb.png" alt="Pentaho User Console" id="BLOGGER_PHOTO_ID_5259928589785607554" border="0" />
</a>

Second, the Pentaho Aggregate Designer, which automatically creates a
set of aggregate tables to accelerate a mondrian schema. I believe
that the Aggregate Designer takes mondrian to a new level of
scalability. Let me explain why.

There are several architectures possible for multidimensional analysis
engines, and they each have their strengths and weaknesses. Mondrian's
architecture is best described as 'ROLAP with caching'. A ROLAP
engine, short for 'relational online analytical processing', stores
its data in a relational database (RDBMS) and accesses it via SQL. It
follows that the RDBMS does most of the heavy-duty processing, such as
JOIN and GROUP BY operations, while the ROLAP engine deals with
presentation, caching, and calculations that can be expressed in a
multidimensional model but cannot easily be converted into SQL.

The chief advantage of ROLAP is its simplicity. A ROLAP engine does
not have its own storage engine: everything is in the RDBMS. In
particular, the load process is simply a matter of loading the RDBMS,
and if the contents of the RDBMS change you just need to
[flush mondrian's cache](https://julianhyde.blogspot.com/2007/02/mondrian-cache-control.html)
to see the up to date contents. Provided that the
RDBMS scales, you can scale mondrian to greater numbers of concurrent
users by having multiple instances of mondrian in a farm of web
servers.

This great strength is also a great weakness. It means that mondrian
is beholden to the RDBMS for performance. In particular, that first
query of the day, the one that scans all 100 million rows in the fact
table to generate a three segment pie chart on the CEO's dashboard:

{% highlight sql %}
SELECT customer.region,
  sum(fact.store_sales)
FROM sales AS fact
  JOIN customer ON fact.cust_id = sales.cust_id
GROUP BY customer.region
{% endhighlight %}

Without aggregate tables, that query takes however long the RDBMS
takes to scan 100 million rows -- perhaps 1 minute, or ten minutes --
but the CEO is not prepared to wait that long. Aggregate tables are
the answer. They contain the pre-computed result of such queries, and
are declared in mondrian's schema so that mondrian knows how to
generate SQL to make use of them.

The problem is that aggregate tables are hard to use. Mondrian has
[supported aggregate tables](https://mondrian.pentaho.org/documentation/aggregate_tables.php)
for several releases, but very few people have
made effective use of them. The steps are as follows.

First of all, choose an effective set of aggregate tables. The
possibilities are literally exponential: in a schema with N attributes
(hierarchy levels) there are 2<sup>N</sup> possible aggregate
tables. If you choose too many, you will use too much disk space and
spend too long loading them every night. If you choose too few, many
queries will fall through the net and end up using a full scan of the
fact table. Many aggregates can be derived from other aggregates, so
it is possible to economize, but there are pitfalls if you do it by
hand. (I will write further about the algorithm the Aggregate Designer
uses to choose a near-optimal set of aggregate tables in a future
post.)

Next, create the aggregate tables in the RDBMS and add mapping
elements such as `<aggname>` to mondrian's schema. Last, write SQL
statements to populate the aggregate tables as part of your ETL
process.

These steps are possible by hand, but very difficult for mere mortals
to get right. The Aggregate Designer automates all of these
steps. Once you have chosen a mondrian schema, and a particular cube
in that schema to optimize, the algorithm analyzes the data in the
star schema underlying that cube, and generates a set of aggregate
tables. If you have a particular set of aggregate tables in mind, you
can create these before running the algorithm, and the algorithm will
create additional aggregate tables, taking yours into account.

<a onblur="try {parent.deselectBloggerImageGracefully();} catch(e) {}"
  href="/assets/img/pad.png">
<img style="margin: 0px auto 10px; display: block; text-align: center; cursor: pointer;"
  src="/assets/img/pad-thumb.png"
  alt="Pentaho Aggregate Designer"
  id="BLOGGER_PHOTO_ID_5259933009947793954" border="0" />
</a>

Each aggregate table is categorized according to its cost (a
combination of the number of bytes on disk and the time it will take
to populate) and its benefit (the effort that will be saved at run
time, over a typical query load, by having the aggregate table). The
Aggregate Designer displays the set of tables it has chosen as a
graph: usually convex, reflecting the fact that the first tables
suggested are the ones with the most favorable cost/benefit ratios.

When the algorithm has run, Aggregate Designer can add the definitions
of the aggregate tables into the mondrian schema. You can either
create and populate the tables immediately, or save a scripts of
`CREATE TABLE` and `INSERT INTO {aggregate table} SELECT ...`
statements. You can even generate
[Pentaho Data Integration (Kettle)](https://kettle.pentaho.org/)
steps to perform the ETL process.

Pentaho User Console and Pentaho Aggregate Designer are both available
in the Pentaho open source BI suite version 2.0. Download the suite,
or check out the [live demo](https://demo.pentaho.com/).
They are compatible with mondrian 3.0.4.11371, which is
available as part of Pentaho 2.0 or for separate download.
