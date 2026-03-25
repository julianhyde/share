---
layout: post
title:  "Into the Wilderness"
date:   2025-03-03 12:00:00 -0800
author: Julian Hyde
tweet:  https://x.com/julianhyde/status/1896834583947313535
---

Today was my last day at Google.

I joined Looker in 2018, and two years later Looker was
[acquired by Google](https://techcrunch.com/2020/02/13/google-closes-2-6b-looker-acquisition/)).
Before the acquisition closed, the company locked down for
COVID, so it was almost a year before I met my Google
colleagues. During my time at Looker/Google, I have led the team that
validates Looker users' queries against a LookML model, expands them
to relational algebra, and translates that algebra to the many SQL
dialects that Looker supports. I have learned much about Google's
incredible internal systems, Google's myriad internal databases (all
unified by a common language and parser, GoogleSQL), and lived by the
mantra "Love Looker Love."

It's time to create something new. I don't know yet what I'll build,
but last time I headed "into the wilderness" it worked out all right.

The year was 2011, and my second son had just been born. Typing with
one hand, so as not to wake the sleeping baby, I started the project
that would later become known as
[Apache Calcite](https://calcite.apache.org).
While "Big Data" and "NoSQL" were the industry's favorite buzzwords at
the time, I was convinced that people would ultimately want systems
with a SQL interface and a cost-based query optimizer.  My instincts
were correct, and over time Calcite has become a successful project
and the foundation for dozens of data systems.

Since then, SQL has come to dominate the landscape.  ("Never bet
against SQL," I like to say.)  Most users have moved to cloud SQL
services (BigQuery, Snowflake) because convenience is more important
than the flexibility and raw power of
[Apache Hadoop](https://hadoop.apache.org) or
[Spark](https://spark.apache.org). SQL now handles
workloads—streaming, data transformation, documents, geospatial,
machine learning—previously handled by non-SQL systems. But there are
two areas where SQL has not yet prevailed—Business Intelligence and
data-intensive computing—and I want to investigate whether the
solution is SQL or something else.

In the next few months, I intend to work on four things:

* **Evolve and promote the Morel language**.
   [Morel](https://github.com/hydromatic/morel/blob/main/README.md)
   is a functional programming language with first-class support for
   relations, or, conversely, a query language that supports functions
   and polymorphic types. Morel can solve data-engineering and
   data-intensive problems that are
   [beyond SQL's capabilities]({% post_url 2020-03-31-word-count-revisited %})
   (think [Dbt](https://www.getdbt.com/) and Spark) but with the
   benefit of a single compiler/optimizer. Even problems that can be
   fully solved in SQL will, I think, benefit from the programming
   language approach (static typing, unit tests, refactoring, modules,
   version control).

* **Realize the SQL semantic layer**. If you
   [add measures to SQL](https://arxiv.org/pdf/2406.00251), a SQL
   view can publish a calculation such as "profit," and a query can
   find the 5 products with the greatest year-over-year profit
   growth. Similarly, you can add display attributes, such as
   formatted value or sort order, to SQL queries and tables. If these
   capabilities are in a DBMS rather than a BI tool, are BI
   applications quicker and easier to build? Does a SQL semantic layer
   improve AI applications?

* **Improve Calcite**. I am
  [improving Calcite's SQL dialect tests](http://issues.apache.org/jira/browse/CALCITE-5529)
  (and indirectly its dialects) and would welcome paid work to improve
  other aspects of Calcite.

* Lastly, I intend to **play more piano**, specifically Beethoven's
   late-period piano sonatas. With a lot of effort over the last few
   months, I have learned to play the
   [Hammerklavier (sonata no. 29)](https://en.wikipedia.org/wiki/Piano_Sonata_No._29_(Beethoven))
   but Beethoven wrote sonatas 30, 31 and 32 for a reason: the
   challenge must not go unanswered!

If you have comments, please reply on
[Bluesky @julianhyde.bsky.social](https://bsky.app/profile/julianhyde.bsky.social)
or Twitter:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>

This article
[has been updated](https://github.com/julianhyde/share/commits/main/blog/{{ page.path }}).
