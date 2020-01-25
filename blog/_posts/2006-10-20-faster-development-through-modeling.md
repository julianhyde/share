---
layout: post
title: Faster Development Through Modeling
date: '2006-10-20T12:04:00.000-07:00'
author: Julian Hyde
tags:
- cwm mondrian metadata xmi mof
modified_time: '2006-10-20T12:45:10.780-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-2187375519085666440
blogger_orig_url: https://julianhyde.blogspot.com/2006/10/faster-development-through-modeling.html
---

Mondrian is used as a running example in an article in this month's Doctor Dobbs Journal, "<a href="http://www.ddj.com/dept/architect/193104822">Faster Development Through Modeling</a>". In the article, Jeff Cahoon explains how to use model-driven architecture (MDA) to generate your application's code and configuration files from a single source of metadata.<br /><br />In the case of Mondrian, the source is a <a href="http://en.wikipedia.org/wiki/Common_Warehouse_Metamodel">CWM</a> schema written using <a href="http://www.fing.edu.uy/inco/ens/aplicaciones/MofPlaza/web/mofplaza/mofeditor.htm">MofEditor</a>, and the example generates a Mondrian schema file containing cube and dimension definitions.<br /><br />I've been a fan of model-driven data warehousing for some time, and CWM in particular, but it can be heavyweight. What's good for managing GE's enterprise data model is not necessarily good for a small company which just wants to display some trend data on its website. It would be more work to implement in Mondrian, and make Mondrian less easy to integrate with tools built on a different standard, and most importantly, would increase the amount of documentation someone would need to read in order for them to build their first working Mondrian application. So, I left CWM support out of Mondrian, and waited to see what would happen.<br /><br />It looks like the open source community is coming up with the tools and techniques to deal with metadata and CWM, which is good news. A few months ago, <a href="http://thinkwaitfast.blogspot.com/2006/06/get-yer-mondrian-metadata-here.html">John Sichi described a technique to convert Mondrian models to CWM</a>, and Jeff Cahoon's article describes how to go in the other direction.<br /><br />What's the 'official' way to write a Mondrian schema? I don't know, you tell me...