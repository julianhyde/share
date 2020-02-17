---
layout: post
title: Database virtualization, distributed caching and streaming SQL
date: '2008-08-27T12:14:00.000-07:00'
author: Julian Hyde
tags:
- virtualization etl cdc olap streaming sql esp
modified_time: '2008-08-27T12:52:08.727-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-9038453523802235237
blogger_orig_url: https://julianhyde.blogspot.com/2008/08/database-virtualization-distributed.html
---

James Kobelius [writes in Network World](http://www.networkworld.com/columnists/2008/082008kobelius.html)
how the need for scalable real-time business intelligence will create
a convergence of technologies centered on database virtualization:

> Real-time is the most exciting new frontier in business
> intelligence, and virtualization will facilitate low-latency
> analytics more powerfully than traditional approaches. Database
> virtualization will enable real-time business intelligence through a
> policy-driven, latency-agile, distributed-caching memory grid that
> permeates an infrastructure at all levels.
>
> As this new approach takes hold, it will provide a convergence
> architecture for diverse approaches to real-time business
> intelligence, such as trickle-feed extract transform load (ETL),
> changed-data capture (CDC), event-stream processing and data
> federation. Traditionally deployed as stovepipe infrastructures,
> these approaches will become alternative integration patterns in a
> virtualized information fabric for real-time business
> intelligence.

Kobelius makes it clear that this "virtualized information fabric" is
an ambitious program that will be accomplished only over a number of
years, but the underlying trends are visible now: for example, the
convergence of distributed caches with databases, as evidenced by
[Oracle's acquisition of Tangosol](https://www.oracle.com/tangosol/index.html),
and [Microsoft's recently announced Project Velocity](https://code.msdn.microsoft.com/velocity).

This envisioned system contains so many moving parts that a new
paradigm will be needed to link them together. I don't think that
databases are the answer. They elegantly handle stored data, but
founder when dealing with change, caching, and the kind of replication
problems you encounter when implementing virtualized and distributed
systems. For example, database triggers are the standard way of
managing change in a database, and are still clunky fifteen years
after they were introduced; and
[Enterprise Information Integration (EII)](https://en.wikipedia.org/wiki/Enterprise_Information_Integration)
systems were an attempt to extend the database model to handle
federated data, but only work well for a proscribed set of
distribution patterns.

I [wrote recently]({% post_url 2008-02-27-streaming-sql-meets-olap %})
about how [SQLstream](https://www.sqlstream.com/) can implement
trickle-feed [ETL](https://en.wikipedia.org/wiki/Extract,_transform,_load)
and use the knowledge it gleans from the passing data to proactively manage the
[mondrian OLAP engine](https://mondrian.pentaho.org/)'s cache.
SQLstream also has adapters to implement
[change-data capture (CDC)](https://en.wikipedia.org/wiki/Change_data_capture)
and to manage data federation.

In SQLstream, the *lingua franca* for all of these integration
patterns is SQL, whereas ironically, if you tried to achieve these
things in Oracle or Microsoft SQL Server, you would end up writing
procedural code: PL/SQL or Transact SQL. Therefore streaming SQL -- a
variant of what Kobelius calls event-stream processing where,
crucially, the language for event-processing language is SQL -- seems
the best candidate for that unifying paradigm.
