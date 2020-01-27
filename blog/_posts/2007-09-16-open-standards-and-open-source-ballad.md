---
layout: post
title: 'Open standards and open-source: The ballad of BIRT and olap4j'
date: '2007-09-16T20:15:00.000-07:00'
author: Julian Hyde
tags:
modified_time: '2007-09-17T10:29:47.596-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-6239873971305436374
blogger_orig_url: https://julianhyde.blogspot.com/2007/09/open-standards-and-open-source-ballad.html
---

This week, I
[criticized the Eclipse BIRT project](http://dev.eclipse.org/newslists/news.eclipse.birt/msg22980.html)
for choosing JOLAP as the API between
their [Dynamic Crosstab support](http://www.eclipse.org/birt/phoenix/project/notable2.2.php#jump_2)
and their cubing engine. I believe that an open-source project should
be judged by the extent of its adoption by the community -- after all,
if a project fails to serve its community, the community will simply
go elsewhere -- and so I hesitate to criticize a project which is
clearly pleasing its community.

My argument hinges on the particular importance of open standards for
open-source projects, and what I believe is the obligation of an
open-source project to embrace standards.

### Open standards, open-source

I am not the first person to observe that
[open standards help open-source projects succeed](http://www.dwheeler.com/essays/open-standards-open-source.html).
An open-source project is likely to find a ready market if it is a
better, cheaper, plug-in replacement for an existing software module,
and if there is a standard, the boundaries of that module are already
well-defined.

But open standards are almost invariably created by commercial
software vendors. The open-source community is beholden to these
vendors, and the vendors are often in no particular hurry to create
open standards.

### Barriers to open standards

In public, vendors claim to love open standards as if they were
virtues comparable with apple pie and motherhood; in private, they
fight them tooth and nail. A couple of years ago, a software vendor
asked me to create an MDX interface to their product using
[mondrian](https://mondrian.pentaho.org), to satisfy the
growing number of their customers who wanted MDX. In private, they
admitted that MDX was a good idea, and they would move to it if the
majority of their customers demanded it, while in public they derided
MDX as inferior to their API. They vendor knew that there is a tipping
point when a market moves to an open-standard, and wanted to delay
that tipping point as long as possible.

Sadly, this thinking is not confined to commercial software. In
commercial and open-source software alike, if a software
vendor/project is in a dominant market position, they can choose to
ignore open standards. If they deliver a collection of software
components connected by proprietary interfaces, customers have to take
all or nothing.

Open-source projects build in functionality for which they could have
used an open standard API. There are some legitimate reasons for
this. Sometimes [license balkanization](http://blogs.zdnet.com/BTL/index.php?p=990)
can be blamed, and it would be legally impossible to
distribute the two components under the same license; sometimes an
open standard exists, but there is no suitable open-source
implementation of that standard. But all too commonly, open-source
developers fall victim to
[not-invented-here syndrome](https://en.wikipedia.org/wiki/Not_Invented_Here),
and build a technology where a good open-source alternative exists.

The pressures are particularly intense when the open-source project is
backed by a commercial organization, as is the case with Eclipse
BIRT. (BIRT is run by [Actuate Corporation](http://www.actuate.com),
a commercial BI vendor, and all of its leaders and
key developers work for that company.) The designers naturally want to
provide a coherent experience for their end-users, but there is also a
temptation to create an architecture which excludes technology of the
parent organization's competitors.

I am proud of the efforts we have made to embrace standards in the
[Pentaho BI Suite](https://www.pentaho.com/), of which mondrian
is a part. Among the standards used in the Pentaho suite are CWM,
XForm, XPDL, XSLT, XPath, SVG, and XUL, as well as all the standards
implicit in the Java and J2EE environments, such as JDBC and JNDI.

Pentaho CTO James Dixon has [pointed out](https://wiki.pentaho.org/display/BEEKEEPER)
that open-source is about more than giving your source code away for
free: it requires a process which is transparent (allowing others to
know where the project is going) and open (allowing others to
participate). Such a process encourages re-use of and contribution to
other projects, and in-house projects are ruthlessly 'right-sized'
into the smallest module which can be self-contained and sustain its
own community. Dependencies between projects are minimized, to make
them usable by the largest possible audience. This is a true
open-source culture, and it naturally embraces open standards.

### A new open standard: olap4j

So it is saddening that there is no API for Java-based OLAP. The only
effective OLAP standards are OLE DB for OLAP (fine if you're running
Windows) and XML for Analysis (fine if you're prepared to write a
program to handle reams of XML). Java developers were promised
JSR-069, also known as JOLAP, but the
[vendors involved (principally IBM, Oracle and Hyperion) failed to agree on a final version](http://jcp.org/en/jsr/results?id=2648).
No server vendor released an implementation of the API,
and no client tool was released which used it. After it became
apparent that JOLAP was truly dead, I removed mondrian's JOLAP
interface; mondrian-2.2 was the last release with JOLAP support.

As I noted above, standards are traditionally created by commercial
vendors, yet in the case of Java-based OLAP, the vendors have
singularly failed to create a standard. The need for an interoperable
OLAP client still exists, so I believe it now falls to the open-source
community to create one.

Last year, I proposed creating an API called olap4j, and since then,
there has been an initial draft of the specification, and work is
under way for drivers for both mondrian and for generic XMLA servers.

### olap4j and BIRT

As an important part of the open-source BI movement, I naturally tried
to include Eclipse BIRT in the olap4j process. Unfortunately the
spirit of openness and transparency seems to be lacking in the BIRT
project. I have made several attempts, privately and publicly, to
engage with BIRT leaders and start collaboration between our
projects. They have not responded to my requests, and I can only
speculate why that might be. There must be a temptation to create a
single BI suite, all parts of which are supplied by Actuate (albeit
under an open-source license) rather than present an architecture
based on open standards where the users have a genuine choice of
components.

olap4j would be a superior API for BIRT to use in order to access
dimensional data. JOLAP is dead: it never became finalized as a
specification, and there has been no further development, releases of
the specification, or implementations since the specification stalled
in 2004. olap4j has an active community, interoperates with XMLA, and
has multiple implementations available or under development.

The benefit to BIRT's community would be considerable. They would be
able to use BIRT's facilities for designing reports, acknowledged as
among the best in open-source BI, and run them against the OLAP server
of their choice. This would allow reporting users to choose the best
OLAP server for their performance requirements and data volumes. Users
who already own an OLAP server could use BIRT as an alternative
interface to the reporting stack provided by their OLAP vendor.

BIRT could even continue to develop a cubing engine. If successful,
that engine could be used as an alternative to mondrian or XMLA by
olap4j clients.

### Conclusion

I am not optimistic that we will see a change of heart from BIRT, but
I wanted to lay things out as I see them.

If you are a member of the open-source BI community -- whether you use
Eclipse, Pentaho or just mondrian -- please let the BIRT team know
that their decision is making BIRT less open, delivering less value to
BIRT's users, and will damage the open-source BI for everyone.
