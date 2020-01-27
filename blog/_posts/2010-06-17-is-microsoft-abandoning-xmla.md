---
layout: post
title: Is Microsoft abandoning XML/A?
date: '2010-06-17T14:46:00.000-07:00'
author: Julian Hyde
tags:
- xmla microsoft olap4j "native xml web services" "sql server 2008"
modified_time: '2010-06-18T09:56:40.296-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-501367300976370506
blogger_orig_url: https://julianhyde.blogspot.com/2010/06/is-microsoft-abandoning-xmla.html
---

Microsoft are deprecating
[Native XML Web Services in SQL Server 2008](https://technet.microsoft.com/en-us/library/cc280436.aspx),
and if I understand the
productmanagerese correctly, that means that they are abandoning
[XML for Analysis (XML/A)](http://www.xmla.org/) as an
interface to Microsoft Analysis Services.

(I may be mistaken. Can someone who is closer to Microsoft's roadmap
clarify how OLAP applications on non-Windows systems are supposed to
access Microsoft Analysis Services?)

**UPDATE, 2010/6/18 09:50 PDT**: *It turns out that I am mistaken, and
I received several comments pointing this out. The real story is that
Microsoft is deprecating native XML access to SQL Server (the
relational database, not the OLAP server). I have left the rest of the
blog post as I originally wrote it, but please read it in the light of
the new evidence.*

But if true, that would be a worrying development for those of us who
want to build interoperable OLAP applications. (In particular, those
with clients on non-Windows platforms such as Linux.)

It's funny; Microsoft was the leading company pushing web services
back in 2000. Everyone thought that it was too verbose a protocol for
passing large data sets around, but Microsoft had a big problem to
solve -- its aging [DCOM](https://msdn.microsoft.com/en-us/library/ms809311.aspx)
infrastructure -- and pushed it through.

I was at a meeting in Redmond (in 1999, if memory serves) when
Microsoft launched XMLA. The SQL Server OLAP Services team demoed the
interoperability by showing a Java program running on Solaris (or
possibly Linux... my memory is fading...) connecting to a Microsoft
OLAP Services server. They joked that they could have been fired for
having a non-Windows machine in the building. But nevertheless they
made their point: XMLA was interoperable, and that was unprecedented
among OLAP applications at the time.

Soon afterwards Microsoft started using
[compressed XML](http://sqlblog.com/blogs/mosha/archive/2005/12/02/analysis-services-2005-protocol-xmla-over-tcp-ip.aspx)
for its XMLA calls and responses, thereby reducing the
problem. Unfortunately their compression technology was proprietary,
so the rest of us had to carry on using uncompressed SOAP calls. It
gave Microsoft's drivers an unfair advantage over other drivers
attempting to talk to Analysis Services.

It's ironic that Microsoft should abandon a standard that they
created, and which has been astoundingly successful. I suspect that
they have gotten tired of maintaining it when their own drivers use
more efficient, proprietary wire protocols.

If Microsoft is deprecating XMLA, I doubt that it will disappear for
some years to come, but it is bound to be a concern for people
building applications now that they intend to be running for several
years.

Of course, one thing people can do to insulate themselves from the
future is to build their OLAP applications in Java using
[olap4j](http://www.olap4j.org). Whatever net protocol
Microsoft adopts to replace XMLA, we will keep the
[olap4j driver for Analysis Services](http://www.olap4j.org/api/org/olap4j/driver/xmla/XmlaOlap4jDriver.html) working,
so you shouldn't need to change your application.

Likewise, if you are building your application in JavaScript, consider
using Roland Bouman's excellent [xmla4js](https://code.google.com/p/xmla4js) library.
