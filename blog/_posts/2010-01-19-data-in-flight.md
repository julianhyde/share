---
layout: post
title: Data in Flight
date: '2010-01-19T13:20:00.000-08:00'
author: Julian Hyde
tags:
- streaming sql database cep
modified_time: '2010-01-19T13:28:06.439-08:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-2398723386094007458
blogger_orig_url: https://julianhyde.blogspot.com/2010/01/data-in-flight.html
---

An article of mine,
"[Data in Flight](http://cacm.acm.org/magazines/2010/1/55738-data-in-flight/fulltext),"
is published in this month's [Communications of the ACM](http://cacm.acm.org/).
In it, I took the time to explain, in layman's terms, why I think
streaming database technology is a game-changer.

Many pundits have latched on to the term
[CEP (Complex Event Processing)](https://en.wikipedia.org/wiki/Complex_event_processing)
to describe this technology. CEP is a legitimate and important
application, and I believe that streaming SQL is a good way to solve
it, but the article tries to put a bit of space between the two
concepts. There are so many problems that benefit from the
declarative, relational approach but where the data arrives
incrementally and the problem can be solved much more efficiently by a
streaming engine working (mainly) in memory than a database, and CEP
is just one application area. My article describes a few of those
problems.

I'm all fired up about streaming databases, just as I was when I
co-founded [SQLstream](https://www.sqlstream.com). I've
worked in the database field for over 20 years, and I think it's the
most exciting thing to happen in databases in a generation. (Yes, it's
more important than data warehousing and, cough, object databases.)

Streaming SQL technology is rapidly becoming part of the standard
toolkit for solving data management problems. If you're not familiar
with the technology, reading the article is a good way to come up to
speed. Enjoy!
