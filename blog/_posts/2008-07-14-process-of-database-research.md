---
layout: post
title: The process of database research
date: '2008-07-14T11:28:00.000-07:00'
author: Julian Hyde
tags:
modified_time: '2008-07-14T11:47:58.236-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-1578489472903099001
blogger_orig_url: https://julianhyde.blogspot.com/2008/07/process-of-database-research.html
---

Jennifer Widom just received the
[ACM SIGMOD Edgar F. Codd Innovations Award](https://www.sigmod.org/sigmodinfo/awards/#innovations),
and spoke about the
[process of database research](http://infoblog.stanford.edu/2008/07/database-research-principles-revealed.html):

> [I]t's imperative to think about all three of the critical
> components -- **data model**, **query language**, and **system** --
> and in that order! We in research have a rare luxury, compared to
> those in industry, that we can mull over a data model for a long
> time before we move on to think about how we'll query it, and we can
> nail down a solid syntax and semantics for a query language before
> we implement it.

I've designed languages before, and I know how hard it is to do it
right, so when I was designing
[SQLstream's extensions to SQL](https://www.sqlstream.com/Products/SQLstream_RAMMS_White_Paper.pdf)
I looked at the research, and
[Jennifer's team's work](http://dbpubs.stanford.edu:8090/pub/2003-67)
was easily the best in the field.

Some of my colleagues balked at the paper's formal approach, but it
was just what we needed to build a language for combining streaming
and stored relational data, and the optimizer rules and execution
objects to implement it.

She is correct that it is a rare luxury for industry to have a sound
foundation to build next-generation technology on. Congratulations on
the award, Jennifer, and thanks for helping to build that foundation.
