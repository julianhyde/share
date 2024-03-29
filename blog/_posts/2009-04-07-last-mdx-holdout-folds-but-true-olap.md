---
layout: post
title: The last MDX holdout folds, but true OLAP interop is still a long way off
date: '2009-04-07T23:58:00.000-07:00'
author: Julian Hyde
tags:
- oracle simba mdx olap olap4j standardization
modified_time: '2009-04-08T01:36:27.927-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-7466747269030858612
blogger_orig_url: https://julianhyde.blogspot.com/2009/04/last-mdx-holdout-folds-but-true-olap.html
---

Oracle, the last major OLAP vendor to embrace
[MDX](https://en.wikipedia.org/wiki/Multidimensional_Expressions),
has finally
[added MDX support to its server](https://www.simba.com/MDX-Provider-for-Oracle-OLAP.htm).
The MDX Provider for Oracle OLAP, developed
in partnership with [Simba](https://www.simba.com),
implements the OLE DB for OLAP API and the MDX query language, and
went beta this week.

The most obvious application of this technology, and I'm sure the
initial revenue driver, will be to allow end-users to use Excel 2007
as their client for slicing and dicing.

Simba's architecture diagram shows the MDX provider loaded onto the
same machine as the Excel client. It wouldn't seem technically
difficult to run the MDX provider as a server, and have multiple
clients connect via OLE DB for OLAP or via XML for
Analysis. (Licensing may be a different matter.)

<a onblur="try {parent.deselectBloggerImageGracefully();} catch(e) {}"
    href="https://www.simba.com/images/MDX-Connector-for-Oracle-OLAP.gif">
  <img style="display:block; margin:0px auto 10px; text-align:center;cursor:pointer; cursor:hand;width: 383px; height: 317px;"
    src="https://www.simba.com/images/MDX-Connector-for-Oracle-OLAP.gif"
    border="0" alt="MDX connector for Oracle OLAP" />
</a>

This announcement means that now it is possible to talk MDX to every
major OLAP server. (Are there any OLAP servers that do not speak MDX?
I can't think of any.) The OLAP market has moved very slowly towards
standardization, but this is a significant moment, even a tipping
point. In a conversation five years ago, Oracle executives agreed that
MDX was a fine language, but said they would not support it, because
that would be to acknowledge that Microsoft was the thought-leader in
the OLAP marketplace. It's that old PR strategy: deny in public, agree
in private. And in a sense their strategy worked, because without a
standard language, the OLAP market could not begin to
commoditize.

There is still a long way to go towards OLAP interoperability. Servers
differ widely in their support of MDX. Unlike SQL, the MDX language is
not in the hands of an independent standards organization; even the
originators of the de facto standard, Microsoft, have not released a
specification for MDX or [XMLA](http://www.xmla.org) for
several years.

A query language is no good without an API to issue queries, and APIs
only exist in Microsoft's own technologies: COM (OLE DB for OLAP),
.NET (adomd.net) and web services (XMLA).

I have been advocating [olap4j](http://www.olap4j.org) as
the standard API for Java-based OLAP, but it has yet to receive public
backing from vendors outside the open source community. And there are
no OLAP APIs for languages such as python, perl, and
php.

The final point of concern is the emergence of Simba as virtually the
sole supplier of MDX, OLE DB for OLAP and XMLA technology. Simba is an
excellent company, who understand MDX very well, and have invested in
building a technology stack. But they also benefit from a close
relationship with Microsoft. (Remember those specifications for MDX
and XMLA I referred to earlier? Though they have not seen public
updates for several years, I'm sure those specifications still exist
behind the walls of Castle Redmond, and are available to Microsoft's
partners.)

As far as I am aware, Simba have been responsible for all of the
projects in the last few years to bolt MDX support on to existing
servers and applications. (With a sole exception: I was never able to
find out where JasperSoft sourced the technology for its
[ODBO Connect](http://www.jaspersoft.com/jaspersoft_app16.html) product.)

To summarize, this is a milestone moment in the development of OLAP
technology, but there is still cause for concern. OLAP APIs exist only
for a small number of languages, vendors show little inclination to
provide true interoperability, and the key technology is provided by a
small number of players.

You can help. If you are a user of OLAP technology it is in your
interests to see the emergence of standards in the OLAP
marketplace. So, please ask your vendor what they are doing about
interoperability. Ask them whether there are OLAP clients, other than
their own, that run on their server. And ask them for APIs to connect
to their server from all of the languages you use in your
organization. Then, we may move a little closer to the goal of OLAP
for all.

