---
layout: post
title: Twitter makes the realtime web look more like the old web
date: '2009-07-29T00:12:00.000-07:00'
author: Julian Hyde
tags:
- twitter realtime web streaming sql
modified_time: '2009-07-29T02:18:10.878-07:00'
thumbnail: https://2.bp.blogspot.com/_BVv0WTpeWTs/SnAFD_hMacI/AAAAAAAAADc/00URfIVDIyU/s72-c/twitter.png
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-5656456503582779922
blogger_orig_url: https://julianhyde.blogspot.com/2009/07/twitter-makes-realtime-web-look-more.html
---

Twitter has a [new home page](https://search.twitter.com),
in the time-honored style of a search engine home page. Claire Cain
Miller [writes in the New York Times](http://bits.blogs.nytimes.com/2009/07/28/twitter-plays-up-search-with-new-home-page/):

> It has become a cliché that first-time visitors to Twitter respond
> with some version of: "I don’t get it." [The new home page] tries to
> solve that problem.

<a onblur="try {parent.deselectBloggerImageGracefully();} catch(e) {}"
    href="/assets/img/twitter.png">
  <img style="float:right; margin:0 0 10px 10px;cursor:pointer; cursor:hand;width: 320px; height: 206px;"
    src="/assets/img/twitter.png" border="0" alt="Twitter's search page"
    id="BLOGGER_PHOTO_ID_5363792722346666434" />
</a>

That problem is worth solving, but the home page is also an
interesting sign of the melding of the old and the new.

# Old web, new web

The old web is that vast repository of content, ranked by how many
people reference that content, and navigated by search engines such as
Google. The new web is populated by dynamic content, where what
happened in the last minute is much more important than what happened
yesterday.

The new, real-time web has been a wild frontier. There's a cachet to
being a Twitter user; you're among pioneers, one of the elite who 'get
it', not one of the ordinary folks. That's a problem for Twitter,
because they need those millions of ordinary folks first to 'get it',
then to get something useful out of it, come back, and start spending
their click-through dollars.

But harnessing the power of the real-time web is no easy
problem. First of all, streaming content is a new paradigm. Facebook
are doing well at introducing a lot of people to that idea of the
ever-changing home page; Twitter's minimalist concept needs more
getting used to, but the search engine front-end to the stream of
chatter will surely help.

Second, you need different tools to convert the noise in Twitter other
social media feeds into information. A search engine is not going to
cut it. The new tools cannot work on the streaming data alone; they
have to combine the new data with old, organize the data, and cluster
the data with other data that is similar based on subject matter,
geographical proximity, or proximity of users in the social
network. The stream of content hurtling past our eyes looks like
chatter, just noise, until we rank it, look for trends, and put it
into historical context.

# Old analytics, new analytics

I find it interesting because at [SQLstream](https://www.sqlstream.com)
we are dealing with a
very similar problems for enterprise data. Business users would like
to see the full spectrum of data, from right now to the distant past,
but when making decisions, they want more recent data to carry more
weight; they also want to take into account similarity of subject
matter, geographical proximity, and the structure of social networks.

Traditional analytic solutions use data warehouses, analogous to the
old, static, web and its search engine guardians. A data warehouse
treats all data equally, regardless of its age. There is so much data
that it has to be stored on disk, and it takes several hours to
organize that data, so while a typical data warehouse will contain
data from five years ago until close of business yesterday, the most
important data -- what happened today -- hasn't reached the data
warehouse yet.

SQLstream melds the old (the data warehouse) with the new (streaming
events and transactions arriving over the wire), presenting a unified
view via the SQL query language. We say that we "query the future",
meaning that you can place standing queries that react when events of
interest occur. These queries cache their working sets in memory, so
the response time is a few milliseconds, and throughput tens or
hundreds of thousands of records per second.

The [data sources that SQLstream can handle]({% post_url 2008-02-27-streaming-sql-meets-olap %})
are diverse. Some of the data
comes from traditional sources, like corporate transaction processing
systems. Some sources are often considered too high-volume to process
in a data warehouse, such as click-stream and system monitoring
data. And there are
[new sources like Twitter, social media, Atom and RSS feeds](http://www.intelligententerprise.com/blog/archives/2008/12/bi_on_content_f.html).

The problems of the real-time web and the real-time enterprise are
surprisingly similar. Without tools to filter, aggregate, rank, and
provide historical context, all of these data sources just look like
noise and have little apparent value. At SQLstream, we are providing
the tools to convert streams into valuable information.
