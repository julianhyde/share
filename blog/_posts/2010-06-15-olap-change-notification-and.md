---
layout: post
title: OLAP change notification, and the CellSetListener API
date: '2010-06-15T23:21:00.000-07:00'
author: Julian Hyde
tags:
- olap4j streaming notification
modified_time: '2010-06-15T23:21:11.998-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-2216338124335761403
blogger_orig_url: https://julianhyde.blogspot.com/2010/06/olap-change-notification-and.html
---

There has been an interesting
[design discussion on the olap4j forums](https://sourceforge.net/projects/olap4j/forums/forum/577988/topic/3737252)
about how an OLAP server could
notify its client that the data set has changed. It is exciting
because it would allow us to efficiently update OLAP displays in
real-time.

We came up with an API, at the center of which is
[the new interface CellSetListener](http://www.olap4j.org/head/api/org/olap4j/CellSetListener.html),
which I have just checked into
[olap4j's subversion repository](https://olap4j.svn.sourceforge.net/viewvc/olap4j?revision=319&view=revision).
(The API is experimental. That means you
shouldn't expect to find a working implementation just yet, or assume
that the API won't change radically before it is finalized, but it
does mean we are still very much open to suggestions for
improvements.)

Of course, OLAP notifications are a subject close to my heart, because
they bring together my interests in [SQLstream](https://www.sqlstream.com)
and [mondrian](https://mondrian.pentaho.org/). 'Push-based'
computing is challenging, because every link in the chain needs to
propagate the events to the next link. In a previous post
[I described]({% post_url 2008-02-27-streaming-sql-meets-olap %})
how SQLstream could do continuous ETL, populate fact and
aggregate tables incrementally, and notify mondrian that data items in
its cache were out of date.

A mondrian implementation of the CellSetListener API would cause
mondrian to internally re-evaluate all queries that have listeners and
cover an affected area of the cache. If the results of those queries
changed, mondrian would transmit those notifications to OLAP client
applications such as [Pentaho Analyzer](https://www.pentaho.com/products/analysis/)
or [PAT](https://code.google.com/p/pentahoanalysistool/). The
client application would then change the value of the cell on the
screen, and maybe change the cell's background color momentarily to
attract the user's attention.

Getting data to change on the screen, in front of the end-user's eyes,
within seconds of the data changing in the operational system, would
be truly spectacular.

There are several links in the chain to make that happen. Two of the
links, SQLstream and [mondrian's cache control API]({% post_url 2007-02-13-mondrian-cache-control %}),
are already complete. We've just begun forging the next link.
