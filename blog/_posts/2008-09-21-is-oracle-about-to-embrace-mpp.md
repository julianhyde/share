---
layout: post
title: Is Oracle about to embrace MPP?
date: '2008-09-21T19:16:00.000-07:00'
author: Julian Hyde
tags:
- oracle openworld sqlstream aeturnum mpp smp streaming sql etl
modified_time: '2008-09-23T11:03:23.183-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-4026734431624018875
blogger_orig_url: https://julianhyde.blogspot.com/2008/09/is-oracle-about-to-embrace-mpp.html
---

Oracle's Larry Ellison has some major announcements to make during
Oracle's [OpenWorld conference](https://www.oracle.com/openworld/2008/index.html)
this coming week in San Francisco. A few months ago he
was promising to announce a
"[major database innovation](https://seekingalpha.com/article/82717-oracle-f4q08-qtr-end-5-31-08-earnings-call-transcript?page=3)",
but declined to give further details, so the Oracle community has been
[speculating furiously](http://www.computerworld.com/action/article.do?command=viewArticleBasic&amp;taxonomyName=Databases&amp;articleId=9115059&amp;taxonomyId=53&amp;pageNumber=1).

With a keynote entitled "Extreme Performance," and product
announcements coming in areas of grid computing and database
acceleration, all the indications are that Oracle is getting serious
about problems that require massive scalability, massive throughput,
and low latency.

This is an area where Oracle has been falling behind. In Oracle's
approach, which independent database analyst Curt Monash calls a
"[shared everything](https://www.dbms2.com/2007/03/06/why-oracle-and-microsoft-will-lose-in-vldb-data-warehousing/)"
architecture, multiple servers belong to the same Oracle Real
Application Cluster (RAC) and share a common pool of memory and disk
storage. But this approach does not allow Oracle to be run on hundreds
or thousands of servers, which is how companies such as Google are
solving problems which require large amounts of storage and
processing. That sort of massively parallel processing (MPP) requires
a "shared nothing" architecture, and internet companies have been
rolling their own architectures out of simpler components.

The result is that Oracle "is way behind in the 'scale-out' world,"
said Paul Vallee, CEO of [The Pythian Group](http://www.pythian.com/),
an Ottawa, Ontario-based database services provider. "MySQL
is eating its lunch in terms of Internet-scaled deployments."

Oracle's own experts seem to agree. In the abstract for a talk
"[Oracle's New Database Accelerator: A Technical Overview](http://www28.cplan.com/cc208/session_details.jsp?isid=298681&amp;ilocation_id=208-1&amp;ilanguage=english)",
Ron Weiss writes:

> New and revolutionary solutions and methodologies are coming
> together to handle the exploding data volumes real-world systems are
> being required to store and serve up. Supporting ever-larger
> databases, with ever-increasing demands for getting "answers"
> faster, requires a new way to approach the problem.

Weiss's solution uses improvements to storage management, but I doubt
that it would satisfy Google's requirements, or even the
price/performance requirements of a medium-sized internet media
company.

Meanwhile, those who have adopted shared-nothing architectures are
feeling the pain too. Having stitched together hundreds or thousands
of databases, the problem is how to populate and coordinate them. For
example, internet companies' transaction rates are so high that it is
not possible to load the day's data during an eight hour nightly load
window, and besides, business owners want to see data in near real
time. Organizations are adapting a 'trickle ETL' process to populate
the data warehouse continuously and with low latency.

So data architects seem to be caught between a rock and a hard
place. Either stick with Oracle's shared-everything (or indeed IBM DB2
or Microsoft SQL Server -- they have the same approach) and live with
the scalability limitations, or move to the wild frontier of
shared-nothing, and be prepared to spend a lot of effort managing,
populating and coordinating your farm of databases.

Ironically, the answer, as Larry Ellison and his cohorts taught us
thirty years ago, is in the relational model. By extending the
relational model beyond stored data to include streaming data, SQL can
be used to efficiently manage data flowing into and between multiple
databases, as well as storage and retrieval within those
databases. This creates a scalable shared-nothing system, with
databases decoupled from each other, but because the data flow is
managed by declarative SQL, it is as manageable as a shared-everything
system such as Oracle.

[SQLstream](https://www.sqlstream.com/) is an implementation
of this new SQL, and can be applied to continuous ETL, real-time BI
and monitoring problems. For example, if there are many data sources
for your ETL process, and many servers to be populated, SQLstream can
act as a cross-hatch, load-balancing the data, aggregating, and
routing each row to the correct database engine with low latency. And
because SQLstream's SQL encompasses both data at rest and
[data in flight]({% post_url 2008-02-27-streaming-sql-meets-olap %}),
it can correlate data in the warehouse with arriving data.

SQLstream is partnering with companies that are building
next-generation data warehousing architectures on Oracle and on other
databases. [Aeturnum](http://www.aeturnum.com/) is an
exciting new delivery partner for SQLstream with extensive expertise
in data warehousing ([Netezza](http://www.netezza.com/))
and business intelligence ([Pentaho](https://www.pentaho.com/)).

Come and see SQLstream at Oracle OpenWorld. We will be at the Aeturnum
stand (2716 Moscone South) all this week.
