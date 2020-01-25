---
layout: post
title: Pentaho Analysis Tool
date: '2009-01-22T10:08:00.000-08:00'
author: Julian Hyde
tags:
- pivot olap4j gwt
modified_time: '2009-01-23T12:59:11.182-08:00'
thumbnail: http://4.bp.blogspot.com/_BVv0WTpeWTs/SXi68fb0cNI/AAAAAAAAACU/NgZjWFo2lWk/s72-c/PAT.png
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-1715744444015766165
blogger_orig_url: https://julianhyde.blogspot.com/2009/01/pentaho-analysis-tool.html
---

<a onblur="try {parent.deselectBloggerImageGracefully();} catch(e) {}" href="http://4.bp.blogspot.com/_BVv0WTpeWTs/SXi68fb0cNI/AAAAAAAAACU/NgZjWFo2lWk/s1600-h/PAT.png"><img style="margin: 0pt 0pt 10px 10px; float: right; cursor: pointer; width: 200px; height: 128px;" src="http://4.bp.blogspot.com/_BVv0WTpeWTs/SXi68fb0cNI/AAAAAAAAACU/NgZjWFo2lWk/s200/PAT.png" alt="" id="BLOGGER_PHOTO_ID_5294186910367117522" border="0" /></a>Some folks are working on an <a href="http://www.olap4j.org/">olap4j</a>-based viewer as an alternative to <a href="http://jpivot.sourceforge.net/">JPivot</a>, called <a href="http://code.google.com/p/pentahoanalysistool/">Pentaho Analysis Tool</a>.<div><br /></div><div>The key developers <a href="http://pentahomusings.blogspot.com/2009/01/pentaho-analysis-tool.html">Tom</a> and Luc tell me that they noticed that the <a href="http://code.google.com/p/halogen/">halogen project</a> hadn't changed in a few months, so they took the halogen source code (based on <a href="http://code.google.com/webtoolkit/">GWT</a>, by the way) and started to take it in the direction of the OLAP viewer they'd like to see.<br /><br />(<span style="font-style: italic;">Edit</span>: There are actually three key developers. I forgot to mention Paul Stöllberger. Sorry Paul!)<br /></div><div><br /></div><div>No hard feelings! I, and some other key Pentaho folks, are delighted that this project is happening, and will support it any way we can.</div><div><br /></div><div>It's ironic that when Pentaho seeded a project to build an olap4j-based viewer, they chose an organic, open-sourcey name 'halogen', yet these folks (none of whom works for Pentaho) chose a name that whiffs of corporate branding.</div><div><br /></div><div>A rose by any other name, as they say.  And despite the name, the viewer should work on top of any olap4j data source (which today means <a href="http://mondrian.pentaho.org/">Mondrian</a> and any OLAP engine with an XMLA interface).</div>