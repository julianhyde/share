---
layout: post
title: SQL past and future
date: '2010-06-29T18:08:00.000-07:00'
author: Julian Hyde
tags:
- sql oracle db2 nosql "one size fits all"
modified_time: '2010-06-29T18:08:52.794-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-4814397995264524607
blogger_orig_url: https://julianhyde.blogspot.com/2010/06/sql-past-and-future.html
---

Ken North, writing in Dr. Dobb's Journal, gives a
[nice overview of the long and storied history of SQL](http://www.drdobbs.com/blog/archives/2010/06/database_indust.html).
The piece helps
one understand the wave of mergers among the big database vendors, and
make sense of current trends in database and database-like
software. And I'd like to offer my opinion about where SQL and
database management systems are headed.

North looks into the claims that 'the database is dead' and finds that
-- yet again -- reports of its death were greatly exaggerated:

> Forrester Research recently estimated the total database market
> (licenses, support, consulting) would grow from $27 billion in 2009
> to $32 billion by 2012. SQL technology is entrenched in many
> organizations and across millions of web sites. Perhaps that
> explains why, during the past decade, IBM, Oracle, Sun and SAP made
> billion-dollar investments in a ‘dead’ technology.

However, I do believe that the relational database is currently in
crisis. Relational databases have been the mainstay of data management
for over twenty years, but Oracle and its cohorts have no answer for
"[Big Data](http://queue.acm.org/detail.cfm?id=1563874)",
the massive onslaught of information from the web and sensors.

The [NoSQL movement](https://en.wikipedia.org/wiki/NoSQL) is
solving these problems by challenging some of the assumptions held by
RDBMS vendors. At SQLstream, we regard ourselves as part of the NoSQL
movement even though we are huge fans of SQL, because we are
challenging the biggest assumption of them all: that you have to put
data on disk before you can analyze it.

It's a shame that North doesn't mention streaming SQL, because it fits
perfectly into the grand arc of the SQL language: adopt new problems,
express them declaratively, and solve them first with special-purpose
database engines and finally by adapting the architecture of the big,
general-purpose database engines. This last step sometimes takes many
years to happen, but it happened for transaction processing, object
database, and data warehousing, and I have no doubt that it will
happen for streaming relational data.

One of the reasons that SQL has remained relevant is SQL standards
process; products built on one database can be run on another database
and, perhaps more important, skill sets acquired on one engine can be
applied to another. When the dust settles, and the big databases have
learned hard architectural lessons, I think a lot of these new
problems will be solved in SQL.

Unlike [Mike Stonebraker](http://www.cs.brown.edu/~ugur/fits_all.pdf),
I do think that organizations will want to put all
these different forms of data into one database management
system. That database will of course be a facade spread over many
servers, disks, data organizations and query processing engines, but
will offer centralized management and allow the different forms of
data to be combined. They will get their wish because the SQL language
is so powerful at hiding differences in underlying data
organization.

When the dust has settled, the SQL language will have changed and
adapted yet again, and maybe there will be some new names at the top
of the roster of database vendors, but we will once again be solving
most of our data management problems using declarative queries
beginning with the word "`SELECT`". SQL is dead; long live SQL!
