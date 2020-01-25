---
layout: post
title: Streaming SQL meets OLAP
date: '2008-02-27T06:21:00.000-08:00'
author: Julian Hyde
tags:
- sqlstream mondrian stream sql olap etl
modified_time: '2008-02-27T06:48:13.754-08:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-6162397310379136982
blogger_orig_url: https://julianhyde.blogspot.com/2008/02/streaming-sql-meets-olap.html
---

Streaming SQL and <a href="http://en.wikipedia.org/wiki/Olap">OLAP</a> are two of the most interesting and powerful paradigms in data processing. OLAP is a well-established technique for analyzing large databases of historic data. Streaming SQL is a more recent innovation, that applies the declarative power of the SQL language to the problem of managing data in motion.<br /><br />So, what happens when you combine OLAP with Streaming SQL? The combination is capable of solving some business problems that can't be solved any other way. OLAP is usually hampered by conventional <a href="http://en.wikipedia.org/wiki/Etl">ETL</a> techniques: it is difficult to keep the data warehouse up to date, because batch-based ETL processes are only efficient when dealing with a few hours or days of data. OLAP engines excel at comparisons between time periods (say, this quarter compared to the same quarter last year) or comparable data sets (say, this brand versus that brand); when powered by a streaming SQL engine, an OLAP engine can also include the most current data in its analysis (say, this hour compared to the average for this hour of the day over the last 6 months).<br /><br />The highest value data in the enterprise is that which represents what is happening to the business right now. This data includes various kinds of remote procedure calls, state changes of critical systems, and all kinds of business events. This data isn't stored on disk - we call it <span style="font-style: italic;">data in flight</span> as opposed to conventional <span style="font-style: italic;">data at rest</span> - and conventional ETL has difficulty accessing it. Streaming SQL allows you to bring this data into the same format as other enterprise data, but retain the ability to analyze and act on it in real time.<br /><br />I'm going to look at how you could combine the <a href="http://mondrian.pentaho.org">mondrian</a> OLAP engine with the <a href="http://www.sqlstream.com/">SQLstream </a>streaming SQL engine.<br /><br />Mondrian requires its data to be stored in a relational database. To ensure high performance on a large data set, mondrian caches query results in memory, and also uses aggregate tables which have been populated with summaries of the data. Mondrian's <a href="http://mondrian.pentaho.org/documentation/architecture.php">cache</a> and <a href="http://mondrian.pentaho.org/documentation/aggregate_tables.php">aggregate tables</a> both require careful management if mondrian is to give the correct answers on a rapidly changing data set.<br /><br />SQLstream helps mondrian do this by providing a continuous, real-time ETL process. As we shall see, the steps are: <span style="font-style: italic;">acquire</span> the real-time data and expose it as a common relational format; <span style="font-style: italic;">transform</span> into an organization suitable for OLAP and data warehousing; <span style="font-style: italic;">load</span> into the data warehouse, including aggregate tables; and <span style="font-style: italic;">notify</span> mondrian of changes to its cache.<br /><br />First of all, SQLstream can help to <span style="font-weight: bold; font-style: italic;">acquire</span> the data. As we said earlier, traditional ETL processes are limited to reading data at rest: from databases, mainframes, and files extracted from other operational systems. Data in flight exists in other formats: messages on <a href="http://en.wikipedia.org/wiki/Message_Oriented_Middleware">message-oriented middleware</a>, <a href="http://en.wikipedia.org/wiki/Web_service">web service</a> calls, <a href="http://en.wikipedia.org/wiki/Transmission_Control_Protocol">TCP</a> network packets, and so forth. SQLstream can <a href="http://www.sqlstream.com/Products/productsTechAdapters.htm">subscribe to these sources of data</a>, and tap into the traditional data warehouse sources too: it can monitor a database table and generate events as new transactions occur, and tail a log file to read rows as they are appended to the log file.<br /><br />One of SQLstream's core concepts is a <span style="font-style: italic;">stream</span>. A stream is analogous to a table in a relational database; but whereas a table contains a finite set of rows which have been inserted at some time in the past and stored on disk, a stream contains an infinite sequence of rows that arrive whenever the producer decides to send them. (SQLstream in fact supports tables too, so that you can combine historical or reference data with event data.)<br /><br />What streams and tables have in common is the fact that you can manipulate them using SQL queries.  Not just the simple operations like filtering and routing, but operations which combine multiple rows such as join and aggregation. You can combine rows with other rows in the same stream (often demarcated by a time window of interest), with rows from other streams, and with historical and reference data.<br /><br />Next, you need to <span style="font-weight: bold; font-style: italic;">prepare the data</span> and convert it into a form suitable for large-scale analysis. In SQLstream, you can use SQL to perform a real-time, continuous ETL process. For example:<br /><ul><li>You can apply standard SQL operators to cleanse and convert the data fields</li><li>You can calculate trends such as moving averages using SQLstream's windowed aggregation operations.</li><li>If your data warehouse schema contains <a href="http://en.wikipedia.org/wiki/Slowly_changing_dimension#Type_2">slowly-changing dimensions</a>, SQLstream can help the loading process by identifying transactions which represent a new member of a dimension. For example, when an order is received from an existing customer, SQLstream can find that customer's id, whereas if the customer is new, it can generate a new surrogate key value.</li><li>If your data warehouse schema contains aggregate tables, they need to be populated with records which represent multiple fact table records. It is often cheaper to compute these aggregate records in memory.<br /></li></ul>On the subject of aggregate tables, note that if you have many aggregate tables and data rates are extremely high, eventually the I/O capacity of the DBMS makes it impossible to keep the aggregate tables 100% up to date. You should reduce the number or granularity of the aggregate tables, and partition each aggregate table by time to ensure that only one block per is being actively written to and therefore the active block of aggregate tables can fit into the DBMS's buffer cache.<br /><br /><span style="font-weight: bold; font-style: italic;">Loading the data warehouse</span> is straightforward. SQLstream has a database adapter that makes DBMS tables appear as foreign streams; writing to these streams makes an insert, update or delete occur in the data warehouse.<br /><br />As data is loaded into the data warehouse, it becomes inconsistent with the state of mondrian's cache. Mondrian's cache is necessary for performance if mondrian has many concurrent users or if the data warehouse is so large that SQL queries take a long time, but flushing the entire cache every time there is an update negates the value of the cache.<br /><br />Fortunately mondrian has an <a href="http://julianhyde.blogspot.com/2007/02/mondrian-cache-control.html">API to let you notify mondrian of changes that affect its cache contents</a>. You can tell mondrian specifically which data changed; for example, you can say 'there was just a sale of beer in Texas', and mondrian will mark precisely these entries in the cache as invalid, so they will be re-read from the database next time an OLAP query requests them.<br /><br />Once again, the problem can easily be solved using a foreign stream. The foreign stream should call mondrian's cache control API for each row it receives; a SQLstream pump object ensures that every record written into the fact table is mirrored into the foreign stream and therefore mondrian's cache is kept in sync with the DBMS.<br /><br />In conclusion, there is a synergy between OLAP and streaming SQL techniques that allows new business problems to be solved and existing problems to be solved much more efficiently. SQLstream provides a platform for all manner of continuous ETL operations, and mondrian with its open-source license and extensible Java architecture is a natural fit.