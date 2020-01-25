---
layout: post
title: Announcing Lingual
date: '2013-02-26T15:31:00.001-08:00'
author: Julian Hyde
tags: 
modified_time: '2013-02-26T15:31:09.711-08:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-4452374024585215763
blogger_orig_url: https://julianhyde.blogspot.com/2013/02/announcing-lingual.html
---

The last few months, I've been collaborating on a project with <a href="https://twitter.com/cwensel" target="_blank">Chris Wensel</a>, the author of <a href="http://www.cascading.org/" target="_blank">Cascading</a>. Last week we announced <a href="http://www.cascading.org/lingual/" target="_blank">Lingual</a>, an open source project that puts a SQL interface on top of Cascading.<br /><br />Architecturally, Lingual combines the Cascading engine with my own&nbsp;<a href="https://github.com/julianhyde/optiq" target="_blank">Optiq</a> framework. Optiq provides the SQL interface (including <a href="http://en.wikipedia.org/wiki/Java_Database_Connectivity" target="_blank">JDBC</a>), reads table and column definitions from Cascading's metadata store, and few custom <a href="http://www.hydromatic.net/optiq/apidocs/org/eigenbase/relopt/RelOptRule.html" target="_blank">Optiq rules</a> target relational operations (project, filter, join and so forth) onto a Cascading operator graph. The queries are executed, on top of <a href="http://hadoop.apache.org/" target="_blank">Hadoop</a>, using Cascading's existing runtime engine. <br /><br />Not everyone has heard of Cascading, so let me explain what it is, and why I think it fits well with Optiq. Cascading is a <a href="http://docs.cascading.org/cascading/1.2/userguide/html/ch02.html" target="_blank">Java API for defining data flows</a>. You write a Java program to build data flows using constructs such as pipes, filters, and grouping operators, Cascading converts that data flow to a MapReduce job, and runs it on Hadoop. Cascading was established early, picked the right level of abstraction to be simple and useful, and has grown to industry strength as it matured.<br /><br />As a result, <a href="http://www.concurrentinc.com/case-studies/" target="_blank">companies who are doing really serious things with Hadoop often use Cascading</a>. Some of the very smartest Hadoop users are <a href="http://cs.stackexchange.com/questions/9648/what-use-are-groups-monoids-and-rings-in-database-computations" target="_blank">easily smart enough</a> to have built their own Hadoop query language, but they did something even smarter&nbsp;—&nbsp;they layered <a href="http://en.wikipedia.org/wiki/Domain-specific_language" target="_blank">DSLs</a> such as <a href="https://dev.twitter.com/blog/scalding" target="_blank">Scalding</a> and <a href="https://github.com/nathanmarz/cascalog" target="_blank">Cascalog</a> on top of Cascading. In a sense, Optiq-powered SQL is just another DSL for Cascading. I'm proud to be in such illustrious company.<br /><br />Newbies always ask, "What is Hadoop?" and then a few moments later, "Is Hadoop a database?". (The answer to the second question is easy. Many people would love Hadoop to be an "open source Teradata", but wanting it doesn't make it so. <a href="http://en.wikipedia.org/wiki/Yes,_Virginia,_there_is_a_Santa_Claus" target="_blank">No Virginia</a>, Hadoop is not a database.)<br /><br />A cottage industry has sprung up of bad analogies for Hadoop, so forgive me if I make another one: Hadoop is, in some sense, an operating system for the compute cluster. After mainframes, minicomputers, and PCs, the next generation of hardware is the compute cluster. Hadoop is the OS, and <a href="http://en.wikipedia.org/wiki/MapReduce" target="_blank">MapReduce</a> is the assembly language for that hardware&nbsp;—&nbsp;all powerful, but difficult to write and debug. UNIX came about to serve the then-new minicomputers, and crucial to its success was the <a href="http://en.wikipedia.org/wiki/C_(programming_language)" target="_blank">C programming language</a>. C allowed developers to be productive while writing code almost as efficient as assembler, and it allowed UNIX to move beyond its original PDP-7 hardware.<br /><br />Cascading is the C of the Hadoop ecosystem. Sparse, elegantly <a href="http://queue.acm.org/detail.cfm?id=2141937" target="_blank">composable</a>, low-level enough to get the job done, but it abstracts away the nasty stuff unless you really want to roll up your sleeves.<br /><br />It makes a lot of sense to put SQL on top of Cascading. There has been a lot of buzz recently about SQL on Hadoop, but we're not getting caught up in the hype. We are not claiming that Lingual will give speed-of-thought response times (Hadoop isn't a database, remember?), nor will it make advanced predictive analytics will be easy to write (Lingual is not magic). But Hadoop is really good at storing, processing, cleaning and exporting data at immense scale. Lingual brings that good stuff to a new audience.<br /><br />A large part of that SQL-speaking audience is machines. I'd guess that 80% of the world's SQL statements are generated by tools. Machine-generated SQL is pretty dumb, so it essential that you have an optimizer. (As author of a tool that speaks SQL — <a href="http://mondrian.pentaho.com/" target="_blank">Mondrian</a>&nbsp;—&nbsp;and several SQL engines&nbsp;—&nbsp;Broadbase, <a href="http://www.luciddb.org/html/main.html" target="_blank">LucidDB</a>, <a href="http://www.sqlstream.com/" target="_blank">SQLstream</a>&nbsp;—&nbsp;I have been on both sides of this problem.) Once you have an optimizer, you can start doing clever stuff like re-organizing your data to make the queries run faster. Maybe the optimizer will even help.<br /><br />Lingual is not a "SQL-like language". Because it is based on Optiq, Lingual is a mature implementation of ANSI/ISO standard SQL. This is especially important for those SQL-generating tools, which cannot rephrase a query to work around a bug or missing feature. As part of our test suite, we ran Mondrian on <a href="http://www.postgresql.org/" target="_blank">PostgreSQL</a>, and captured the SQL queries it issued and the results the database gave. Then we replayed those queries&nbsp;—&nbsp;over 6,200 of them&nbsp;—&nbsp;to Lingual and checked that Lingual gave the same results. (By the way, putting Optiq and Cascading together was surprisingly easy. The biggest challenge we had was removing the Postgres-isms from thousands of generated queries.)<br /><br />Lingual is not the only thing I've been working on. (You can tell when I'm busy by the deafening silence on this blog.) I've also been working on <a href="http://incubator.apache.org/drill/" target="_blank">Apache Drill</a>, using Optiq to extend SQL for JSON-shaped data, and I'll blog about this shortly. Also, as Optiq is integrated with more data formats and engines, the number of possibilities increases. If you happen to be at <a href="http://strataconf.com/strata2013/" target="_blank">Strata conference</a> tomorrow (Wednesday), drop me a line <a href="https://twitter.com/julianhyde" target="_blank">on twitter</a> and we can meet up and discuss. Probably in the bar.