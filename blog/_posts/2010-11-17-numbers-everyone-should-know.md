---
layout: post
title: Numbers everyone should know
date: '2010-11-17T15:04:00.000-08:00'
author: Julian Hyde
tags:
- system design
modified_time: '2010-11-17T15:04:17.508-08:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-3969700668946149667
blogger_orig_url: https://julianhyde.blogspot.com/2010/11/numbers-everyone-should-know.html
---

Jeffrey Dean recently gave a talk "Building Software Systems at Google
and Lessons Learned" at Stanford ([video](http://goo.gl/0MznW)).
One of his slides was the following list of numbers:

|L1 cache reference|0.5 ns
|Branch mispredict|5 ns
|L2 cache reference|7 ns
|Mutex lock/unlock|25 ns
|Main memory reference|100 ns
|Compress 1K bytes w/ cheap algorithm|3,000 ns
|Send 2K bytes over 1 Gbps network|20,000 ns
|Read 1 MB sequentially from memory|250,000 ns
|Round trip within same datacenter|500,000 ns
|Disk seek|10,000,000 ns
|Read 1 MB sequentially from disk|20,000,000 ns
|Send packet CA &rarr; Netherlands &rarr; CA|150,000,000 ns

Everyone who wants to design high-performance, scalable systems should
memorize these numbers. There are many, many lessons to be learned.
