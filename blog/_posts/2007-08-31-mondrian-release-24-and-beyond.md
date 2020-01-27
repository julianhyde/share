---
layout: post
title: mondrian release 2.4 and beyond
date: '2007-08-31T15:10:00.000-07:00'
author: Julian Hyde
tags:
modified_time: '2007-08-31T16:19:42.294-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-3533412866533530286
blogger_orig_url: https://julianhyde.blogspot.com/2007/08/mondrian-release-24-and-beyond.html
---

I've just released mondrian-2.4 production. In the five months since
mondrian-2.3 there have been
[some significant new features and a host of bug fixes](https://sourceforge.net/project/shownotes.php?group_id=35302&release_id=536343).

The features I'm most pleased with are the ability to aggregate
distinct-count measures (much harder to implement than you'd think!)
and the ability to generate SQL containing the new
[GROUPING SETS clause](http://www.ss64.com/orasyntax/an_grouping.html)
if the database supports it.

### No more release lag

In past releases, we've been criticized for a several month lag
between a mondrian release and the Pentaho release which contains it -
and rightly so. We've now put that right. On Monday, Pentaho just
released the 2nd Release Candidate of their 1.6 release, containing
mondrian-2.4.1-RC1. Pentaho 1.6 will shortly go production containing
the production mondrian-2.4.2.

### The future is... olap4j

If you follow this blog, you'll know that I think that the world
really needs an open API for accessing OLAP data from Java. I've been
working with several other companies and organizations on
[the olap4j specification](http://www.olap4j.org).

Since the original draft
[olap4j specification, version 0.5](https://sourceforge.net/project/showfiles.php?group_id=168953),
released in September 2006, a lot of work has done behind the
scenes. There are several new modules in olap4j, including an MDX
parser, parse tree model, and a query model, and there are partially
complete drivers for mondrian and XMLA.

It's time for a new release of olap4j, and now I have mondrian-2.4 out
of the door, I should be able to pull together olap4j release 0.9 by
October.

olap4j will also feature prominently in the next mondrian
release. Mondrian release 3.0 will include an olap4j driver compatible
with the olap4j 0.9 specification. (There will be some other cool
features too; see
[the mondrian roadmap](https://mondrian.pentaho.org/documentation/roadmap.php#Release_3.0)
for more details.)

Eventually olap4j will be mondrian's main API. Ever since I started
developing mondrian, my mantra has been "don't innovate in the
specification: innovate in the implementation". When there are several
OLAP servers out there supporting olap4j, and several clients which
can talk to any olap4j server, I want people to be choosing mondrian
because it is the best implementation. The idea of olap4j is to level
the playing field, and may the best OLAP technology win!

### Future plans for backwards compatibility

Mondrian's current API will exist unchanged in mondrian-3.0. The API
will be deprecated after that, and by mondrian-3.1 we won't guarantee
that existing applications will still work. For most applications,
moving to olap4j won't be hard: the olap4j API is simpler than
mondrian's current API, familiar to anyone who has used JDBC, and
better documented too.

There are some other APIs which are deprecated in mondrian-2.4 and
will be removed in mondrian-3.0. For details of these, see
[the roadmap](https://mondrian.pentaho.org/documentation/roadmap.php#Mondrian_2.4)
once again.
