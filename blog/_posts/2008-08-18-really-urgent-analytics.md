---
layout: post
title: Really urgent analytics
date: '2008-08-18T09:48:00.000-07:00'
author: Julian Hyde
tags:
- real-time analytics event stream processing esp
modified_time: '2008-08-18T10:32:48.618-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-7212230696877623691
blogger_orig_url: https://julianhyde.blogspot.com/2008/08/really-urgent-analytics.html
---

A Forrester report entitled "<a href="http://www.forrester.com/Research/Document/Excerpt/0,7211,45965,00.html"><span class="greyBLURB">Really Urgent Analytics: The Sweet Spot for Real-Time Data Warehousing</span></a>" makes the connection between event-stream processing (ESP) and data warehousing, and Intelligent Enterprise published a <a href="http://www.intelligententerprise.com/info_centers/ent_dev/showArticle.jhtml?articleID=210101150&amp;pgno=1">nice summary</a>.<br /><br />A traditional data warehouse contained huge amounts of data but was loaded infrequently: say monthly, or nightly at best. Modern businesses demand actions at lower latencies, and data warehousing professionals have been able tune the traditional data warehouse load process to reduce latency.<br /><br />But even when cranked up to the maximum, the load process cannot achieve latencies of less than a few seconds, whereas many business processes need their answers faster than that. And this performance comes at the cost of higher complexity, so it takes more time and effort to modify the load process to incorporate new data or ask new questions.<br /><br />To solve the squeeze between lower latency and increasing complexity -- and, I would mention, ever-increasing data volumes and a trend towards distributed systems -- data warehousing needs a new architectural component, and Forrester rightly point to Event Stream Processing to fill that gap. I would add that, given the skill set of data warehousing professionals, it makes a lot of sense for that Event Stream Processing to be in SQL.<br /><br />At <a href="http://www.sqlstream.com">SQLstream</a>, we saw this need four years ago, and are dedicated to solving the latency-complexity problem using SQL.