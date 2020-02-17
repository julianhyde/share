---
layout: post
title: Optiq latest
date: '2013-03-01T13:29:00.000-08:00'
author: Julian Hyde
tags:
modified_time: '2013-03-01T13:29:02.574-08:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-3297441682839682298
blogger_orig_url: https://julianhyde.blogspot.com/2013/03/optiq-latest.html
---

[Optiq](https://github.com/julianhyde/optiq) has been developing steadily
over the past few months. Those of you who
[watch github](https://github.com/julianhyde/optiq/commits/master)
will know most of this already, but I
thought I'd summarize what's been going on.

(This post originally appeared as an email to the
[optiq-dev](https://groups.google.com/forum/?fromgroups#!forum/optiq-dev)
mailing list. Since I compose email
messages a lot faster than blog posts, and the email message contained
a lot of stuff that you'd all find interesting, it made sense to
recycle it. Hope you don't mind.)

There are two exciting new projects using Optiq:
* I have been working in private with [Chris Wensel](https://twitter.com/cwensel)
  on a project to use Optiq to provide a SQL interface to
  [Cascading](https://www.cascading.org/), and last week we
  [announced Lingual]({% post_url 2013-02-26-announcing-lingual %}).
* I am working a SQL interface for the
  [Apache Drill](https://incubator.apache.org/drill/) project.

This week I attended the
[Strata conference in Santa Clara](http://strataconf.com/strata2013/),
and met lots of people who are interested in Optiq for various
reasons. There are at least 4 back-end platforms or front-end
languages that people would like to see. I can't describe them all
here, but this space. Some exciting stuff will play out in this forum
over the next few months.

One of my personal favorite projects is to get Optiq running on
compressed, in-memory tables managed by a
[JSR-107](http://jcp.org/en/jsr/detail?id=107)-compliant cache/data-grid such as
[ehCache](http://ehcache.org/) or
[Infinispan](https://www.jboss.org/infinispan).
[ArrayTable](https://github.com/julianhyde/optiq/blob/master/src/main/java/net/hydromatic/optiq/impl/clone/ArrayTable.java) and
[CloneSchema](https://github.com/julianhyde/optiq/blob/master/src/main/java/net/hydromatic/optiq/impl/clone/CloneSchema.java)
are the beginnings of that project. The end result will be a
high-performance, distributed, in-memory SQL database... how cool is
that? (Certainly, my own Mondrian project will be able to make massive
use of it.)

And, some people were asking for the
[Splunk adapter](https://github.com/julianhyde/optiq-splunk)
(the so-called "JDBC driver for Splunk") to be improved. Good to hear
that it's proving useful.

Now regarding the code.

One person noted that `mvn clean install` should just work for any
maven-based project, and it doesn't. He's right. It should.
[I fixed it](https://github.com/julianhyde/optiq/commit/e49f96c220b12bf1dc4d968fbbb6dcfc4321f40b).
Now it does.

I made some
[breaking API changes last week](https://github.com/julianhyde/optiq/commit/4737cbbafc4051d0d70a3887f40461df7f4e3d03),
so I upped the version to 0.2.

Expect the version numbers to continue to move erratically, because in
our current development mode, it doesn't seem to make sense to have
specific milestones. We're basically working on general stability
rather than a few big features. We are trying to maintain backwards
compatibility, but if we need to change API, we'll do it. I'll help
dependent projects such as Lingual and Drill migrate to the new API,
and make it as easy as possible for the rest of you.

Over the last week I'd been working on the code generation that powers
SQL scalar expressions and built-in functions. This code generation
is, obviously, used by the
[Java provider](https://github.com/julianhyde/optiq/tree/master/src/main/java/net/hydromatic/optiq/rules/java),
but it can also be used by other
providers. For instance, Lingual generates Java strings for filters
that it passes to
[Janino](http://docs.codehaus.org/display/JANINO/Home).
I've been working on `OptiqSqlOperatorTest`
to get more and more of the built-in SQL functions to pass, and I've
added `OptiqAssert.planContains` so that we can add tests to make sure
that the minitiae of java code generation are as efficient as
possible.

I still need to tell you about the extensions I've been making to
Optiq SQL to support Drill (but useful to any project that wants to
use nested data or late-binding schemas), but that will have to wait
for its own blog post. Watch this space.
