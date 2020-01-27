---
layout: post
title: First draft of olap4j specification released
date: '2006-09-03T15:21:00.000-07:00'
author: Julian Hyde
tags:
modified_time: '2006-09-03T15:54:35.411-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-2258255298078489050
blogger_orig_url: https://julianhyde.blogspot.com/2006/09/first-draft-of-olap4j-specification.html
---

Last night I [released the first draft specification](https://sourceforge.net/project/showfiles.php?group_id=168953)
of a new, simple, open API for Java-based OLAP. This specification is
called [olap4j](http://www.olap4j.org/).

Who's behind olap4j? Well, I initiated it, because strongly feel that
a standard API will benefit [mondrian](https://mondrian.sourceforge.net/).
But other people agree that it will be good for BI in general. Various
companies and projects are represented on the specification committee,
and we're open to more.

What does olap4j mean for mondrian? At first glance, it would seem
that an open standard would make it easier for people to migrate away
from mondrian, and would reduce mondrian's market share.

But that's not how it works. It's well known that open source projects
thrive when a standard API allows them to compete with each other and
with commercial products. We expect olap4j to have the same effect on
mondrian. Using olap4j, it will be possible to write an OLAP
application on one server, say mondrian, and run it on another, say
[Microsoft Analysis Services](https://www.microsoft.com/sql/technologies/analysis/)
via olap4j's XML/A driver. I would also like to see drivers for other
open-source OLAP servers such as [PALO](http://www.palo.net/)
and [OpenOLAP](http://www.iafc.co.jp/products/openolap.htm). And
olap4j should spur development of new OLAP client tools. Users will be
free to choose whichever OLAP server and OLAP client fits their needs
best - which should increase adoption of mondrian.

Transitioning to a new API won't be painless for existing mondrian users. I made
[a post to mondrian's Open Discussion forum](https://sourceforge.net/forum/forum.php?thread_id=1566036&forum_id=111375)
to discuss how olap4j will affect them.

To find out more, visit [http://www.olap4j.org](http://www.olap4j.org/). Download the
[specification](https://sourceforge.net/project/showfiles.php?group_id=168953&amp;package_id=192743&amp;release_id=444412),
and subscribe to the olap4j forum and mailing list at
[SourceForge](https://sourceforge.net/projects/olap4j). Most
important, get involved in the review process, and let us know what
you'd like to see in an OLAP API.

And watch this space. As the specification evolves over the next few
months, I'll be writing more about it at this blog.
