---
layout: post
title: 'Streaming content feeds part 2: forging the Streaming Web'
date: '2008-12-15T02:48:00.000-08:00'
author: Julian Hyde
tags:
- streaming web rss twitter
modified_time: '2008-12-15T04:24:16.362-08:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-2250947655583837195
blogger_orig_url: https://julianhyde.blogspot.com/2008/12/streaming-content-feeds-part-2-forging.html
---

My previous blog post
"[Streaming analytics over content feeds (and how content feeds could be
better)]({% post_url 2008-12-12-streaming-analytics-over-content-feeds %})"
drew some excellent comments, so I thought I'd follow up
with some more thoughts about a protocol for streaming web content,
and a vision that I'll dub the "Streaming Web".

To [John Kalucki](http://www.blogger.com/profile/13334730102829428646)'s
points first. I absolutely agree that the driver for
this protocol is latency. But it is difficult to answer the question
"what latency is necessary?", because we don't yet know what
applications people will devise.

(An illustration of how latency changes everything, from a very
different business: when my wife worked for
[Niman Ranch](https://www.nimanranch.com/), I was amazed to
hear that they dispatch steaks via FedEx (packed in ice and
insulation, and sent overnight); this would be out of the question
using the USPS and a three day delivery time.)

I believe that real-time web content feeds are a game changer. I call
it the Streaming Web -- a web where every piece of content is
accessible via a URL and you can subscribe to be alerted immediately
if a piece of content changes. Every page would become a potential
feed, and there would be agents that allow us to collect and filter
content we are interested in: be it a friend's photo album or the
price of a plane ticket.

A huge effort is required to make the Streaming Web a reality. The
first steps, the web content formats such as RSS and Atom, are already
in place. The next step is to introduce a protocols so that
subscribers are notified of changes as soon as they happen.

John says:

> What experience can you offer with feeds at a 50ms push latency vs a
> 180,000ms pull latency? If a machine is consuming the feed, not
> much. If a human is immediately consuming a feed, perhaps a great
> deal.

I agree that a human can benefit from low-latency content, although
there is little benefit for content arriving faster than the human's
think time -- say 5,000ms. But if a computer is the consumer, ideal
latencies span a broad spectrum: a mail server would operate more
efficiently if it is allowed latencies in the minutes or hours,
whereas an automated stock trading system needs information to arrive
within 50ms.

Today, not much web content is of interest to automated stock trading
systems. Most web content feeds today are textual -- written by humans,
and consumed by humans -- but I believe that once we remove the latency
constraints and introduce some standard protocols, we will start to
see more structured data in feeds. Also, we will see algorithms for
[extracting information from textual feeds](http://www.intelligententerprise.com/blog/archives/2008/11/up_next_bi_on_s.html).

As for the right protocol for the job, I am not really the best judge,
so I am going to punt for now, and focus on the architecture.
[Richard Taylor](http://www.blogger.com/profile/14036876973506495788)
suggests [XMPP](https://www.xmpp.org). It seems
to have the right qualifications, and I'm sure that it could be made
to work technically. (And I see that XMPP is already a
[central part of the Twitter ecosystem](http://www.techcrunch.com/2008/05/05/twitter-can-be-liberated-heres-how/).)
It comes down to power versus
simplicity: the power of an established standard versus the simplicity
required to reach a new audience of developers.

I've been around long enough to see new approaches overturn
"over-complex" existing technologies and then, in time, acquire the
features that made their predecessors complicated. Take for example
[SOAP](https://en.wikipedia.org/wiki/SOAP_%28protocol%29)
overturning [CORBA](https://en.wikipedia.org/wiki/CORBA), or
PCs overturning minicomputers. I'm not going to take sides: these
revolutions are part of the process of how technology moves
forward. But it does seem that each revolution will only be successful
if the new technology serves a new audience. And, to borrow Einstein's
words, a protocol should be as simple as possible, but no simpler;
otherwise, even if the technology finds its initial audience, it will
not survive its growing pains.

I'm not a big fan of XML as a protocol for transmitting data over a
network, mainly because it is bulky, and that makes it expensive to
produce and consume at high data rates. But for this protocol, I would
choose XML over a binary format. If you're a developer learning a new
protocol, it's a lot easier to debug your code if you can read the
messages being sent over the nextwork as text.

Which brings us to the audience for this protocol. I do agree with
[Stefan Tilkov](http://www.innoq.com/blog/st/) that "[f]or the
majority of use cases, [the polling] approach is vastly superior to a
push model". That majority is already well served, so I'm focusing on
the minority that need low latency. I think those use cases are
important, and we'll all be using them if the "streaming web" thing
catches on.

To achieve low latency feeds, push is more efficient than
high-frequency polling, but it is still more expensive than
low-frequency polling, which is what people are doing today. So, if
every web content aggregator and RSS reader switched to a low-latency
push protocol overnight, the system would collapse.

But luckily, there is no need for those millions of clients who would
like to receive low-latency feed updates to connect using this new
protocol. If those clients are humans, they will be happy to receive
their updates via XMPP or SMS, or slower protocols like email. A
single server could speak the streaming web feed protocol to various
source feeds, and route the results to thousands of end users via XMPP
or SMS. This approach means that each source feed is serving a modest
number of downstream servers.

I'd describe it as a 'wholesale' architecture. A food producer has a
central depot, where it loads its goods onto the trucks of several
client stores. The food company allows consumers to buy from the
depot, if they are prepared to buy their goods in bulk, but most
consumers opt for the convenience of visiting a local store and buying
their goods in smaller quantities.

(If you're Twitter,
[no problem is ever small](http://louisgray.com/live/2008/11/twitter-planning-to-open-up-firehose-by.html),
so that 'modest number' is probably in the
tens of thousands. But I suppose that problem can be solved using
multiple tiers of servers and fanning out streams between one tier and
the next.)

The next step in the evolution of the architecture would be to
introduce a query language. Queries present a more convenient
interface for clients, but they would have architectural
advantages. For example, using a query, a client can specify more
precisely which content it is interested in. It would save CPU effort
on the client and possibly the server, and bandwidth for everyone, so
there would be a strong incentive to use queries rather than raw
feeds.

Queries would also allow feeds to be virtualized: rather than talk
directly to [blogger](http://www.blogger.com/) and
[typepad](https://www.typepad.com), a client could talk to a
third party that aggregates the content into a single feed.

[Streaming SQL](https://www.sqlstream.com) would be a good
candidate for expressing these queries, but is by no means the only
choice. And in fact the architecture and protocol would work well
enough for clients that did not use queries and wanted to consume only
raw feeds.

The resulting system, the Streaming Web, would enable applications yet
to be imagined.
