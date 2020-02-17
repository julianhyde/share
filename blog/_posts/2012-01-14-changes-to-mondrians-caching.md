---
layout: post
title: Changes to Mondrian's caching architecture
date: '2012-01-14T16:05:00.000-08:00'
author: Julian Hyde
tags:
modified_time: '2012-01-16T09:50:28.309-08:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-70633547855380786
blogger_orig_url: https://julianhyde.blogspot.com/2012/01/changes-to-mondrians-caching.html
---

I checked in some architectural changes to Mondrian's cache this week.

First the executive summary:
1. Mondrian should do the same thing as it did before, but scale up
   better to more concurrent queries and more cores.
2. Since this is a fairly significant change in the architecture, I'd
   appreciate if you kicked the tires, to make sure I didn't break
   anything.

Now the longer version.

Since we
[introduced external caches in Mondrian 3.3]({% post_url 2011-02-04-scalable-caching-in-mondrian %}),
we were aware that we were putting a strain on the caching
architecture. The caching architecture has needed modernization for a
while, but external caches made it worse. First, a call to an external
cache can take a significant amount of time: depending on the cache,
it might do a network I/O, and so take several orders of magnitude
longer than a memory access. Second, we introduced external caching
and introduced in-cache rollup, and for both of these we had to beef
up the in-memory indexes needed to organize the cache
segments.

Previously we'd used a critical section approach: any thread that
wanted to access an object in the cache locked out the entire
cache. As the cache data structures became more complex, those
operations were taking longer. To improve scalability, we adopted a
radically different architectural pattern, called the
[Actor Model](https://en.wikipedia.org/wiki/Actor_model).
Basically, one thread, called the Cache Manager is
dedicated to looking after the cache index. Any query thread that
wants to find a segment in the cache, or to add a segment to the
cache, or create a segment by rolling up existing segments, or flush
the cache sends a message to the Cache Manager.

Ironically, the cache manager does not get segments from external
caches. As I said earlier, external cache accesses can take a while,
and the cache manager is super-busy. The cache manager tells the
client the segment key to ask the external cache for, and the client
does the asking. When a client gets a segment, it stores it in its
private storage (good for the duration of a query) so it doesn't need
to ask the cache manager again. Since a segment can contain thousands
of cells, even large queries typically only make a few requests to the
cache manager.

The external cache isn't just slow; it is also porous. It can have a
segment one minute, and forget it the next. The Mondrian query thread
that gets the cache miss will tell the cache manager to remove the
segment from its index (so Mondrian doesn't ask for it again), and
formulate an alternative strategy to find it. Maybe the required cell
exists in another cached segment; maybe it can be obtained by rolling
up other segments in cache (but they, too, could have gone missing
without notice). If all else fails, we can generate SQL to populate
the required segment from the database (a fact table, or if possible,
an aggregate table).

Since the cache manager is too busy to talk to the external cache, it
is certainly too busy to execute SQL statements. From the cache
manager's perspective, SQL queries take an eternity (several million
CPU cycles each), so it farms out SQL queries to a pool of worker
threads. The cache manager marks that segment as 'loading'. If another
query thread asks the cache manager for a cell that would be in that
segment, it receives a
[Future](https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/Future.html)&lt;[SegmentBody](https://mondrian.pentaho.com/api/mondrian/rolap/agg/SegmentBody.html)&gt;
that will be populated as
soon as the segment arrives. When that segment returns, the query
thread pushes the segment into the cache, and tells the cache manager
to update the state of that segment from 'loading' to
'ready'.

The Actor Model is a radically different architecture. First, let's
look at the benefits. Since one thread is managing an entire
subsystem, you can just remove all locking. This is liberating. Within
the subsystem, you can code things very simply, rather than perverting
your data structures for thread-safety. You don't even need to use
concurrency-safe data structures like
[CopyOnWriteArrayList](https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/CopyOnWriteArrayList.html),
you can just use the fastest
data structure that does the job. Once you remove concurrency controls
such as 'synchronized' blocks, and access from only one thread, the
data structure becomes miraculously faster. How can that be? The data
structure now resides in the thread's cache, and when you removed the
concurrency controls, you were also removing memory barriers that
forced changes to be written through L1 and L2 cache to RAM, which is
[up to 200 times slower]({% post_url 2010-11-17-numbers-everyone-should-know %}).

Migrating to the Actor Model wasn't without its challenges. First of
all, you need to decide which data structures and actions should be
owned by the actor. I believe we got that one right. I found that most
of the same things needed to be done, but by different threads than
previously; so the task we mainly about moving code around. We needed
to refine the data structures that were passed between "query", "cache
manager" and "worker" threads, to make sure that they were
immutable. If, for instance, you want the query thread to find other
useful work to do while it is waiting for a segment, it shouldn't be
modifying a data structure that it put into the cache manager's
request queue.&nbsp;In a future blog post,&nbsp;I'll describe in more
detail the&nbsp;challenges &amp; benefits of migrating one component
of a complex software system to the Actor Model.

Not all caches are equal. Some, like [JBoss Infinispan](https://www.jboss.org/infinispan),
are able to share cache items (in our case, segments
containing cell values) between nodes in a cluster, and to use
redundancy to ensure that cache items are never lost. Infinispan calls
itself a "data grid", which first I dismissed as mere marketing, but I
became convinced that it is genuinely a different kind of beast than a
regular cache. To support data grids, we added hooks so that a cache
can tell Mondrian about segments that have been added to other nodes
in a cluster. This way, Mondrian becomes a genuine cluster. If I
execute query X on node 1, it will put segments into the data grid
that will make the query you are about to submit, query Y on node 2,
execute faster.

As you can tell by the enthusiastic length of this post, I am very
excited about this change to Mondrian's architecture. Outwardly,
Mondrian executes the same MDX queries the same as it ever did. But
the internal engine can scale better when running on a modern CPU with
many cores; due to the external caches, the cache behave much more
predictably; and you can create clusters of Mondrian nodes that share
their work and memory.

The changes will be released soon as Mondrian version
~~3.3.1~~ 3.4, but you can help by downloading from
the main line (or from CI), kicking the tires, and letting us know if
you find any problems.

[Edited 2012/1/16, to fix version number.]
