---
layout: post
title: olap4j heading for 1.0
date: '2010-07-22T15:49:00.000-07:00'
author: Julian Hyde
tags:
- olap4j beta production
modified_time: '2010-07-22T15:49:03.342-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-4728627971244287150
blogger_orig_url: https://julianhyde.blogspot.com/2010/07/olap4j-heading-for-10.html
---

Luc Boudreau this week
[announced plans to take olap4j to version 1.0](http://blog.devdonkey.org/?p=31).

It is four years since I released the draft of the
[olap4j API](http://www.olap4j.org/). People have joked that
olap4j, like google mail, has been in
"[perpetual beta](https://en.wikipedia.org/wiki/Perpetual_beta)"
since then. But olap4j's maturity belies its humble version number. It
has been in use by production applications, is the foundation of
several OLAP clients, and there are at least two drivers. (Mondrian's
primary interface is its olap4j driver, and the XMLA driver has
variants for Mondrian, Microsoft Analysis Services and SAP BW.)

In software development (and particularly open source) culture,
version 1.0 of an API is a symbolic milestone. It means that the API
is stable, well tested, and will not be changed except at a major
release, and then only with due consultation.&nbsp;So, version 1.0 of
olap4j will be something to celebrate, but before then, we need to
undertake a review of what is in the API.

Some parts of olap4j (such as the
[query model](http://www.olap4j.org/api/org/olap4j/query/Query.html),
[advanced drill through](https://forums.pentaho.org/showthread.php?t=69327),
[cell write back](https://julianhyde.blogspot.com/2009/06/cell-writeback-in-mondrian.html)
and [notifications](https://julianhyde.blogspot.com/2010/06/olap-change-notification-and.html))
are still under active development, and it is not in anyone's
interests to freeze these parts of the API just yet. So, sections such
as these will be marked 'experimental', and likely to change (with
consultation of the community, as usual) in future.

Whether you are an olap4j developer, part of the existing olap4j user
community, or are just interested in using OLAP from within the Java
without being tied to a particular vendor's API, please get involved
in the review process.
