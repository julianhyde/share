---
layout: post
title: SQLstream powers Firefox 3.5 realtime downloads monitor
date: '2009-06-30T16:04:00.000-07:00'
author: Julian Hyde
tags:
- mozilla firefox 3.5 sqlstream
modified_time: '2009-06-30T16:46:02.870-07:00'
thumbnail: https://2.bp.blogspot.com/_BVv0WTpeWTs/SkqjjCsFmtI/AAAAAAAAADM/UvqG-XUOhHA/s72-c/heatmap.jpg
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-6307425355657845641
blogger_orig_url: https://julianhyde.blogspot.com/2009/06/sqlstream-powers-firefox-35-realtime.html
---

Mozilla launched [Firefox 3.5](http://www.mozilla.com/firefox) today,
and with it, a neat applet, powered by [SQLstream](https://www.sqlstream.com),
to monitor downloads in real time.

<a onblur="try {parent.deselectBloggerImageGracefully();} catch(e) {}"
    href="/assets/img/heatmap.jpg">
  <img style="float:right; margin:0 0 10px 10px;cursor:pointer; cursor:hand;width: 320px; height: 316px;"
     src="/assets/img/heatmap.jpg" border="0"
     alt="" id="BLOGGER_PHOTO_ID_5353270929495792338" />
</a>

You can see the results at
[Mozilla's download stats page](http://downloadstats.mozilla.com).

A few weeks ago,
[Apple's Hyperwall](http://www.creativereview.co.uk/cr-blog/2009/june/apple-hyperwall)
was awe-inspiring as a piece of visual art, but it was less impressive
as a piece of real-time data integration, because the data was
[delayed five minutes](http://www.appleinsider.com/articles/09/06/09/apple_stuns_wwdc_crowd_with_pulsating_app_store_hyperwall.html)
from the app store.

SQLstream gathers data from Mozilla's download centers around the
world, assigns each record a latitude and longitude, and summarizes
the information in a continuously executing SQL query. Data is read
with sub-second latencies, and then aggregated (using SQLstream's
streaming GROUP BY operator) into summary records each describing a
second of activity.

A server-side Java program reads the data using JDBC, serializes it as
JSON, and transmits it to all connected web clients. Clients render
the charts using the Canvas tag, newly introduced in
[HTML 5](https://en.wikipedia.org/wiki/HTML_5). The results are
very impressive visually, but to a back-end guy like myself, the
plumbing is impressive too.

The amazing thing is that SQLstream makes this so easy. Our official
company blurb talks about "shortening data integration projects from
months to weeks", but this project took just a couple of days of work.

By the way, don't try to view the page in Microsoft's Internet
Explorer. Ten years ago, Internet Explorer led the charge to enhance
the capabilities of the web browser, introducing dynamic HTML (DHTML),
XML handling in the browser, ActiveX controls and other capabilities,
but those days are over. With HTML 5 there is a renaissance in web
standards; Firefox is leading the pack, with other 'modern' browsers
such as [Safari](http://apple.com/safari), [Opera](http://www.opera.com)
and [Chrome](https://www.google.com/chrome) not far behind.
