---
layout: post
title: Pentaho Analysis Tool
date: '2009-01-22T10:08:00.000-08:00'
author: Julian Hyde
tags:
- pivot olap4j gwt
modified_time: '2009-01-23T12:59:11.182-08:00'
thumbnail: https://4.bp.blogspot.com/_BVv0WTpeWTs/SXi68fb0cNI/AAAAAAAAACU/NgZjWFo2lWk/s72-c/PAT.png
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-1715744444015766165
blogger_orig_url: https://julianhyde.blogspot.com/2009/01/pentaho-analysis-tool.html
---

<a onblur="try {parent.deselectBloggerImageGracefully();} catch(e) {}"
  href="/assets/img/pat.png">
<img style="margin: 0pt 0pt 10px 10px; float: right; cursor: pointer; width: 200px; height: 128px;"
 src="/assets/img/pat-thumb.png" alt="Pentaho Analysis Tool"
 id="BLOGGER_PHOTO_ID_5294186910367117522" border="0" />
</a>

Some folks are working on an
[olap4j](http://www.olap4j.org/)-based viewer as an
alternative to [JPivot](https://jpivot.sourceforge.net/),
called [Pentaho Analysis Tool](https://code.google.com/p/pentahoanalysistool/).

The key developers
[Tom](https://pentahomusings.blogspot.com/2009/01/pentaho-analysis-tool.html)
and Luc tell me that they noticed that the
[halogen project](https://code.google.com/p/halogen/) hadn't
changed in a few months, so they took the halogen source code (based
on [GWT](https://code.google.com/webtoolkit/), by the way)
and started to take it in the direction of the OLAP viewer they'd like
to see.

(*Edit*: There are actually three key developers. I forgot to mention
Paul Stöllberger. Sorry Paul!)

No hard feelings! I, and some other key Pentaho folks, are delighted
that this project is happening, and will support it any way we
can.

It's ironic that when Pentaho seeded a project to build an
olap4j-based viewer, they chose an organic, open-sourcey name
'halogen', yet these folks (none of whom works for Pentaho) chose a
name that whiffs of corporate branding.

A rose by any other name, as they say.  And despite the name, the
viewer should work on top of any olap4j data source (which today means
[Mondrian](https://mondrian.pentaho.org/) and any OLAP
engine with an XMLA interface).
