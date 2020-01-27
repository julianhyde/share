---
layout: post
title: Concepts in Streaming SQL
date: '2010-10-29T20:21:00.000-07:00'
author: Julian Hyde
tags:
- streaming sql
modified_time: '2010-10-29T20:21:16.246-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-3688808049615720870
blogger_orig_url: https://julianhyde.blogspot.com/2010/10/concepts-in-streaming-sql.html
---

SQLstream's marketing head Rick Saletta just wrote a
[layman's guide to streaming SQL](https://www.sqlstream.com/blog/2010/10/concepts-in-streaming-sql/).
It's short, sweet, entirely buzzword-free,
and a good introduction to streaming queries. So I thought I'd share
the whole post:

> A streaming SQL query is a continuous, standing query that executes
> over streaming data. Data streams are processed using familiar SQL
> relational operators augmented to handle time sensitive
> data. Streaming queries are similar to database queries in how they
> analyze data; they differ by operating continuously on data as they
> arrive and by updating results in real-time.
>
> Streaming SQL queries process dynamic, flowing data, in contrast to
> traditional RDBMSs, which process static, stored data with repeated
> single-shot queries. Streaming SQL is simple to configure using
> existing IT skills, dramatically reducing integration cost and
> complexity. Combining the intuitive power of SQL with this
> simplicity of configuration enables much faster implementation of
> business ideas, while retaining the scalability and investment
> protection important for business-critical systems.
>
> By processing transactions continuously, streaming SQL directly
> addresses the real-time business needs for low latency, high volume,
> and rapid integration. Complex, time-sensitive transformations and
> analytics, operating continuously across multiple input data
> sources, are simple to configure and generate streaming-analytics
> answers as input data arrive. Sources can include any application
> inputs or outputs, or any of the data feeds processed or generated
> within an enterprise. Examples include financial trading data,
> internet clickstream data, sensor data, and exception events. SQL
> can process multiple input and output streams of data, for multiple
> publishers and subscribers.

If you want to learn more, download the
[Concepts in Streaming SQL](https://www.sqlstream.com/Resources/ConceptsInStreamingSQL.pdf) white paper.
