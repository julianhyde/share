---
layout: post
title: Scalable caching in Mondrian
date: '2011-02-04T13:13:00.000-08:00'
author: Julian Hyde
tags:
- scalable olap ehcache terracotta jboss infinispan
modified_time: '2011-02-04T13:20:45.738-08:00'
thumbnail: https://1.bp.blogspot.com/_qNJXxQcuKDM/TUtk3p9so8I/AAAAAAAAAEI/lHcS8yzIq0k/s72-c/MondrianSegmentCacheSPI.png
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-4134355498819032849
blogger_orig_url: https://julianhyde.blogspot.com/2011/02/scalable-caching-in-mondrian.html
---

Wouldn't it be great if
[Mondrian](https://mondrian.pentaho.org/)'s cache could be
shared between several Mondrian instances, use memory outside the JVM
or even across several machines, and scale as the data size or
computation effort increases? That is the vision of Pentaho's
"enterprise cache" initiative.

<table align="center" cellpadding="0" cellspacing="0" class="tr-caption-container"
    style="margin-left: auto; margin-right: auto; text-align: center;">
<tbody>
<tr>
<td style="text-align: center;">
<a href="/assets/img/MondrianSegmentCacheSPI.png"
    imageanchor="1" style="margin-left: auto; margin-right: auto;">
  <img border="0" height="191" width="400"
      src="/assets/img/MondrianSegmentCacheSPI.png" />
</a>
</td>
</tr>
<tr>
<td class="tr-caption" style="text-align: center;">Mondrian cell-caching architecture, including pluggable external cache.</td>
</tr>
</tbody>
</table>

Luc Boudreau has been leading this effort, has just checked in the
first revision of the new
[mondrian.rolap.agg.SegmentCache](https://mondrian.pentaho.com/headapi/mondrian/rolap/agg/SegmentCache.html) interface,
and has [written a blog post describing how it will work](https://devdonkey.blogspot.com/2011/02/mondrian-spi-segmentcache.html).
(Note: This [SPI](https://en.wikipedia.org/wiki/Service_provider_interface)
is likely to change before we release it.)

Pluggable caching will be in Mondrian release 3.3, probably Q2 or Q3
this year.In the community edition will be the SPI and a default
implementation that uses JVM memory. Of course the community will be
able to contribute alternative implementations. In the enterprise
edition of Mondrian 3.3, there will be scalable, highly manageable
implementation based on something like
[Terracotta BigMemory](http://www.terracotta.org/bigmemory),
[ehCache](http://ehcache.org/) or
[JBoss Infinispan](https://www.jboss.org/infinispan).

In future releases, you can expect to see further work in the
area. Maybe alternative implementations of the caching SPI, and
certainly tuning of Mondrian's caching and evaluation strategies, as
we apply Mondrian to some of the biggest data sets out there.
