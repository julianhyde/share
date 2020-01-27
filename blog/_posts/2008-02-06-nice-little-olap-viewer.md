---
layout: post
title: A nice little OLAP viewer
date: '2008-02-06T15:01:00.000-08:00'
author: Julian Hyde
tags:
- olap4j gwt slice dice olap mdx
modified_time: '2008-02-07T19:27:23.603-08:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-5520317633252226864
blogger_orig_url: https://julianhyde.blogspot.com/2008/02/nice-little-olap-viewer.html
---

Bill Seyler and Will Gorman from [Pentaho](https://www.pentaho.com/)
have put together a nice little OLAP viewer in their spare time, called
[Halogen](https://code.google.com/p/halogen/). It isn't fully
baked (by a long stretch) but it shows what you can do if you pair up
[GWT](https://code.google.com/webtoolkit/) with
[olap4j](http://www.olap4j.org/).

<a onblur="try {parent.deselectBloggerImageGracefully();} catch(e) {}"
    href="http://billandlizz.com/images/Report_Screen.jpg">
  <img style="margin: 0pt 0pt 10px 10px; cursor: pointer; width: 320px;"
    src="http://billandlizz.com/images/Report_Screen.jpg"
    align="right" alt="Halogen viewer" border="1" />
</a>

I think it shows off the strengths of both GWT and olap4j nicely. Both
technologies have a strong portability message. Because of GWT,
Halogen has really nice AJAX usability and can run in any
browser. Because of olap4j, it can run against mondrian and an XMLA
provider without changing a line of code. (I haven't tried it against
against [Microsoft SQL Server Analysis Services](https://www.microsoft.com/sql/solutions/bi/bianalysis.mspx),
for instance, but it shouldn't be hard to get it working.)

This isn't an official Pentaho product, more of a proof of concept
with the potential to grow into an alternative to
[JPivot](https://jpivot.sourceforge.net/) if the community
thinks it is cool and we get some momentum behind it. To make it
easier for people to contribute, we made a point of releasing it under
the commercial-friendly license, namely the
[Mozilla Public License](http://www.mozilla.org/MPL/).

Check it out.
