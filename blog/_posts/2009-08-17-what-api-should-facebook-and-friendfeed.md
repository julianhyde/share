---
layout: post
title: What API should Facebook and FriendFeed use to publish the social stream?
date: '2009-08-17T04:11:00.000-07:00'
author: Julian Hyde
tags:
- streaming sql social media rss twitter friendfeed facebook
modified_time: '2009-08-17T05:57:28.653-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-7615067644962095988
blogger_orig_url: https://julianhyde.blogspot.com/2009/08/what-api-should-facebook-and-friendfeed.html
---

Ars Technica [reports that](http://arstechnica.com/web/news/2009/08/stream-resistance-is-futile-facebook-assimilates-friendfeed.ars)

> social networking giant Facebook has acquired FriendFeed. This deal
> reflects Facebook's growing fixation on the social stream, but it's
> hard to see how the two services will be merged. [...]
>
> [Facebook's] powerful but esoteric SQL-like query system all add up
> to a steep learning curve. By comparison, FriendFeed has a simple
> and elegant API that exposes a lot of information and is much more
> accommodating to developers.

It seems to me that streaming SQL is the correct solution to this
problem. Not a SQL-like language, not an API (although you of course
have to use an API to execute queries and get the results), and not
just traditional SQL on finite relations, but SQL where streams are a
first-class construct.

I'm not a big believer in 'SQL-like' languages; they give SQL a bad
name. Someone once said that the C programming language combines all
the power of assembly language with all the ease-of-use of assembly
language. The same could be said for 'SQL-like' languages: they tend
offer limited capabilities of a fixed API, but you have to learn a new
language to do so.

Full SQL is difficult to implement because it must be possible to
combine the relational operators (join, filter, union, project, and so
forth), and other language features such as types and built-in
operators, in any combination. Implementors often give up on this
(what language designers call [orthogonality](https://en.wikipedia.org/wiki/Orthogonal#Computer_science)),
and what they get to is termed a SQL-like language. The full power of
SQL only accrues when the implementor has implemented the whole
language, and achieved orthogonality.

Nor can the problem be solved particularly easily or efficiently using
regular SQL, because every query is going to be of the form 'tell me
what has changed since I last ran the query'. That kind of activity
throws a conventional database into a tailspin.

So, streaming SQL could solve this problem. Has anyone tried it?
