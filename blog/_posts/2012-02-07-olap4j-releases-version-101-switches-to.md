---
layout: post
title: olap4j releases version 1.0.1, switches to Apache license
date: '2012-02-07T23:04:00.000-08:00'
author: Julian Hyde
tags:
modified_time: '2012-02-07T23:04:01.441-08:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-3060897648018986887
blogger_orig_url: https://julianhyde.blogspot.com/2012/02/olap4j-releases-version-101-switches-to.html
---

I am pleased to announce the release of
[olap4j version 1.0.1](https://sourceforge.net/projects/olap4j/files/olap4j/olap4j-1.0.1/).

As the version number implies, this is
basically a maintenance release. It is backwards compatible with
version 1.0.0, meaning that any driver or application written for
olap4j 1.0.0 should work with 1.0.1.

There is a year's worth of bug fixes, which should help the stability
and performance of the XMLA driver in particular.

But more significant than the code changes is the change in
license. Olap4j is now released under the
[Apache License, Version 2.0](https://www.apache.org/licenses/LICENSE-2.0.html) (ASL). Our goal is to
maximize the number of applications that use olap4j, and the number of
drivers. ASL is a more permissive license than olap4j's previous license,
[Eclipse Public License](http://www.eclipse.org/legal/epl-v10.html) (EPL),
so helps drive adoption.

For instance, under ASL, if you create a driver by forking an existing
driver, you are not required to publish your modified source code, and
you may embed the driver in a non-ASL project or product. We hope that
this will increase the number of commercial olap4j drivers. (Of
course, we hope you will see the wisdom of contributing back your
changes, but you are not obliged to.)

Before you ask. It is quite coincidental that this license change
occurred in the same week that [Pentaho Data Integration (Kettle) also switched to
Apache Software License](https://www.infoworld.com/d/business-intelligence/pentaho-open-sources-big-data-integration-tools-under-apache-20-185258).
Although I'm sure that Pentaho's motivations were similar to ours.

Thanks to everyone who has contributed fixes and valuable feedback
since olap4j 1.0.0, and in particular to Luc for wrangling the release
out of the door.
