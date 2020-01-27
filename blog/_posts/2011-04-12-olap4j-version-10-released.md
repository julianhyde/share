---
layout: post
title: olap4j version 1.0 released
date: '2011-04-12T02:21:00.000-07:00'
author: Julian Hyde
tags:
- olap4j java olap analysis api
modified_time: '2011-04-12T03:05:59.583-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-9217801446574600968
blogger_orig_url: https://julianhyde.blogspot.com/2011/04/olap4j-version-10-released.html
---

Today
[we launched version 1.0 of olap4j](https://www.pentaho.com/news/releases/pentaho-announces-a-new-era-in-open-standards-for-analytics/),
the open standard API for accessing analytic databases.

It's worth mentioning that version 1.0 is a big deal for an open
source project. The tag implies maturity and stability, both of which
are true for olap4j. The project is over 4 years old, has two robust
driver implementations, and many applications in production.

The
[olap4j driver for Mondrian](https://mondrian.pentaho.com/api/mondrian/olap4j/MondrianOlap4jDriver.html)
has been the official way to access Mondrian since version 3.0, and the
[olap4j driver for XML/A](http://www.olap4j.org/api/org/olap4j/driver/xmla/XmlaOlap4jDriver.html)
allows access to many XML/A-compliant analytic engines, including
[Microsoft SQL Server Analysis Services](https://www.microsoft.com/sqlserver/2008/en/us/analysis-services.aspx),
[Mondrian](https://mondrian.pentaho.com),
[Palo](https://www.jedox.com/en/products/Palo-Suite/palo-olap-server.html),
and [SAP BW](https://en.wikipedia.org/wiki/SAP_NetWeaver_Business_Intelligence).

olap4j was created to address the lack of an open standard API for
Java access to OLAP servers. Microsoft had created APIs for the
Windows platform ([OLE DB for OLAP](https://en.wikipedia.org/wiki/OLE_DB_for_OLAP), and later
[ADOMD.NET](https://msdn.microsoft.com/en-us/library/ms123483.aspx))
and for web services
([XML for Analysis](http://news.xmlforanalysis.com/what-is-xmla.html))
and in due course other vendors adopted those APIs as
standards, but on Java, the main platform for enterprise applications,
you were always tied to the API provided by your OLAP server vendor.

There had been previous attempts to create Java APIs for OLAP, but
they foundered because the main vendors could not -- or would not
-- overcome the technical differences between their
products. Since OLAP is concerned with constructing dynamic queries to
assist an end-user in interactively exploring a data set, most vendors
constructed queries using a complex proprietary API to "build" a query
using a sequence of transforms.

Relational database APIs such as
[ODBC](https://en.wikipedia.org/wiki/Open_Database_Connectivity)
and [JDBC](https://en.wikipedia.org/wiki/Java_Database_Connectivity)
take a different approach: the query is a string in the SQL
language. This allowed the APIs to be simpler, because the semantics
of the query language need to be understood by the SQL parser and
validator on the server, not by the API itself. And it has allowed the
query language to be standardized without affecting the API too
much. But the OLAP vendors maintained that such a simplifying approach
could not be applied to OLAP.

Microsoft started to prove them wrong when in 1998 they launched
[SQL Server OLAP Services](https://en.wikipedia.org/wiki/Microsoft_Analysis_Services),
the OLE DB for OLAP API, and the MDX query
language. This was the first time (to my knowledge) that an OLAP
vendor had built its API around a query language as opposed to a set
of transforms. MDX played a major role in the success of XML/A: a web
services API would have been much harder to use if the queries had
built using an object model. Other vendors started to adopt OLE DB for
OLAP and XML/A, leaving a void on the one platform Microsoft had no
interest in: Java.

Those of us in the open source world felt that void most acutely. Open
source projects are organized into discrete components, each talking a
standard API, and able to replace a proprietary component by being
better, cheaper, faster. If there are no standard APIs, the product
stacks sprawl across many components, from client-side to server-side,
all made by the same vendor; there is nowhere for open source to get a
foothold, and the customer has no choice but to accept the whole hog
sold by the vendor.

To redress this, I decided to create a new API. The software would be
developed as an open source project, but perhaps more importantly, the
specification would be created using an open standards process. As a
result, the participants in olap4j read as a who's who of open source
BI. Barry Klawans, then chief architect of [JasperSoft](http://www.jaspersoft.com),
co-authored the original draft; Pentaho's chief geek,
[James Dixon](https://jamesdixon.wordpress.com/), authored the
query model; [Luc Boudreau](https://devdonkey.blogspot.com/),
first with the University of Montreal, then with SQL
Power, and now at Pentaho, is the XMLA driver's most active committer
and co-leads the project; Paul Stoellberger and Tom Barber have proven
and showcased olap4j by developing the first graphical client,
Saiku. Paul has also got the XMLA driver working against SAP BW. And
we've worked closely with Palo developers: Michael Raue worked with us
on the spec, and [Vladislav Malicevic](https://twitter.com/vmalic)
has gotten the XMLA driver working against Palo.

I knew that to be successful, olap4j needed to be simple and familiar,
so I mandated that it would be an extension to JDBC and would use MDX
as its query language. The other participants in the specification
process took it from there.

Because olap4j is an extension to JDBC, any developer who has accessed
databases from Java can easily pick it up. And it can leverage
standard JDBC services such as connection pools and driver managers.

Microsoft had proven that an API could be built around the MDX
language; there were differences between servers, but these would be
mostly in the dialect of MDX supported; just about any server could
support the basic metamodel of catalogs, cubes, dimensions, and
measures. Some clients would want to build their own queries, and
parse existing MDX queries; for these, we added a
[query model](http://www.olap4j.org/api/org/olap4j/query/Query.html) and an
[MDX parser](http://www.olap4j.org/api/org/olap4j/mdx/parser/MdxParser.html)
to olap4j. Use of the query model and MDX parser is
optional: if you have an MDX query string, you can just execute it.

We have recently added more advanced features such as
[scenarios (write-back)](https://julianhyde.blogspot.com/2009/06/cell-writeback-in-mondrian.html)
and [notifications](https://julianhyde.blogspot.com/2010/06/olap-change-notification-and.html).
These features are still experimental (unlike the rest of the API, they may
change post-1.0) and are optional for any olap4j provider. But we hope
to see more providers implementing them, and clients making use of
them. And we hope to see more features added to olap4j in future
versions.

The goal of olap4j was to foster development of analytic clients,
servers, and integrated analytic apps by providing an open standard
for connectivity. That goal has been realized. There is a native
driver for mondrian and an XMLA driver that works against Microsoft
SQL Server Analysis Services, SAP BW, Jedox Palo. There are several
clients, both open and closed source: several components in Pentaho's
own suite, the [Community Dashboard Framework (CDF)](https://code.google.com/p/pentaho-cdf/),
[Saiku](http://www.analytical-labs.com/),
[ADANS](https://code.google.com/p/adans/),
[SQL Power Wabit](http://www.sqlpower.ca/page/wabit), and more.

People are using olap4j in ways that I couldn't imagine when I started
the project four years ago. That's the exciting thing about an open
source project becomes successful and starts to gain momentum: you can
expect the unexpected.

Thank you to everyone who helped us get to this milestone.

Visit [www.olap4j.org](http://www.olap4j.org), and download
the release 1.0 of the specification and the software.
