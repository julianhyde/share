---
layout: post
title: olap4j now talks to Palo and SAP BW
date: '2010-08-02T17:58:00.000-07:00'
author: Julian Hyde
tags:
- olap4j palo "sap bw" ssas mondrian jedox pentaho
modified_time: '2011-07-20T12:09:51.359-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-2314703459638413813
blogger_orig_url: https://julianhyde.blogspot.com/2010/08/olap4j-now-talks-to-palo-and-sap-bw.html
---

As [olap4j heads towards release 1.0]({% post_url 2010-07-22-olap4j-heading-for-10 %}),
there are further signs that it is
coming of age, in the form of drivers for the
[Palo MOLAP engine](https://en.wikipedia.org/wiki/Palo_(OLAP_database)) and
[SAP BW](https://en.wikipedia.org/wiki/SAP_NetWeaver_Business_Intelligence).

A few days ago
[Paul Stoellberger announced](https://twitter.com/pstoellberger/status/18983516133) that olap4j's
[XMLA](http://www.xmla.org/) driver could connect to SAP BW,
and posted pictures of PAT, PRD and PDI to prove it.

And just a week later, Jedox CEO
[Kristian Raue writes about how to connect to Palo](http://www.paloinsider.com/palo/palo-talks-olap4j-finally/).
His post includes a
blessedly short Java program to do it. Only one line of
Kristian Raue's program -- the connect string -- would be different if
the program were talking to [Mondrian](https://mondrian.pentaho.com/),
[Microsoft SQL Server Analysis Services](https://msdn.microsoft.com/en-us/library/bb522607.aspx)
or SAP BW via XMLA.

This is a success for both open standards and for open source
software. Now applications built on [olap4j](http://www.olap4j.org)
have two open source OLAP
engines -- Palo and Mondrian -- available to them, and can choose
which is best according to the characteristics of their OLAP
application.

**Clarification, 2011/7/20**: *Palo's engine is open source, but as
Christian Warden points out in a comment to this post, their XMLA
server is not. I was therefore incorrect to give the impression that
olap4j can talk to Palo as part of a 100% open source stack.*

Behind those open source projects are companies who need to show a
profit. Palo is backed by [Jedox](https://www.jedox.com/),
and Mondrian is backed by
[Pentaho](https://www.pentaho.com/). Are the business people at
those companies concerned that their engineers are working with each
other, or that their customers now have a choice of OLAP engines? Not
at all. The move makes the open source BI ecosystem stronger, and both
companies benefit.

Vendors who embrace open source and open standards are effectively
saying, "We have built our platform on open standards. We know that if
we don't live up to your expectations, you can just walk away. So we
know that we have to remain the best platform for your application."

Customers love to have choices, and Pentaho and Jedox are giving
customers the greatest choice of all: the choice to walk away.
