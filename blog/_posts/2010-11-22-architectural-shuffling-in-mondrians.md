---
layout: post
title: Architectural shuffling in mondrian's XMLA and olap4j servers
date: '2010-11-22T12:00:00.000-08:00'
author: Julian Hyde
tags:
- mondrian architecture olap4j xmla
modified_time: '2010-11-24T11:26:41.746-08:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-7768829724068073683
blogger_orig_url: https://julianhyde.blogspot.com/2010/11/architectural-shuffling-in-mondrians.html
---

As a software architect, some of my most interesting work doesn't
deliver any additional functionality to end-users, but reorganizes the
architecture to make great things possible in future. Since mondrian
is an open source project, those great things will, likely as not, be
dreamt up by someone else; my job as leader of the
[mondrian](https://mondrian.pentaho.com) project is to
reorganize things to make that possible.

Case in point, my
[recent check in, change 13929](http://perforce.eigenbase.org:8080/@md=d&amp;cd=//&amp;c=D2C@/13929?ac=10).
It contains three new pieces of functionality.

### Make mondrian's XMLA server run off the olap4j API

Mondrian's legacy API ([mondrian.olap.Connection](https://mondrian.pentaho.com/headapi/mondrian/olap/Connection.html),
etc.) has been deprecated for some time; olap4j is the official API by
which applications should speak to mondrian. Mondrian's XMLA server,
that takes incoming SOAP requests over HTTP to execute queries or
retrieve metadata, processes them using mondrian, and returns the
results as SOAP or JSON over HTTP, has not used the olap4j API until
now.

As part of this change, I converted the
[XMLA server](https://mondrian.pentaho.com/headapi/mondrian/xmla/XmlaHandler.html) to use olap4j.
In the process, I achieved some beneficial
side effects. First, I discovered and fixed a few bugs in mondrian's
olap4j driver; this will make the olap4j driver more stable for
everyone.

Second, I discovered a few essential pieces of metadata that the
olap4j API does not return. I have not yet extended olap4j to include
them: that may happen as we move towards olap4j 1.1 or olap4j 2.0, if
they make sense for other olap4j stakeholders. I created the
[XmlaExtra](https://mondrian.pentaho.com/headapi/mondrian/xmla/XmlaHandler.XmlaExtra.html)
interface as a loophole, to allow the XMLA server get mondrian's
legacy API; this interface serves to document what's missing from
olap4j.

Third, and most exciting, the XMLA server should now run against any
olap4j driver. It needs to be repackaged a bit -- it still lives
within the mondrian codebase, in the
[mondrian.xmla](https://mondrian.pentaho.com/headapi/mondrian/xmla/package-summary.html)
package -- but if you are developing an olap4j driver, contact
me, and we can consider spinning it out.<br />

### Make mondrian into a real server -- for those who want one

You'll notice that I tend to refer to mondrian as an OLAP engine. I've
always hesitated to call it a 'server', because a server has an
independent existence (its own process id, for instance),
configuration information, and services such as authentication.

This is no accident: I deliberately architected mondrian as an engine,
so that it could be embedded into another application or server and
inherit those services from that application. That's why you need to
tell mondrian the URI of the catalog, the JDBC information of the data
warehouse, and the role that you would like mondrian to use to execute
queries. It has no concept of users and passwords, because it assumes
that the enclosing application is performing authentication, then
mapping authenticated users to roles.

This architecture makes as much sense now as it did when I started,
and it isn't going to change. Core mondrian will remain an engine. But
the XMLA server, as its name suggests, performs some of the functions
that one associates with a server. In particular, it reads a
datasources.xml file that contains the name, catalog URI, and JDBC
information of multiple catalogs. My idea was to create an alternate
olap4j driver,
[MondrianOlap4jEngineDriver](https://mondrian.pentaho.com/headapi/mondrian/olap4j/MondrianOlap4jEngineDriver.html),
that extends the default driver
[MondrianOlap4jDriver](https://mondrian.pentaho.com/headapi/mondrian/olap4j/MondrianOlap4jDriver.html),
and move the catalog functionality from the XMLA server to the new
olap4j driver.

The new driver is added as part of this change, but is not
complete. In a later change, I will move the catalog functionality out
of the XMLA server. I don't have plans to add other server features,
such as mechanisms to authenticate users or map user names to
roles. But I've provided the hook where this functionality can be
added, and I encourage you in the mondrian community to contribute
that functionality.<br />

### Lock box

Last, I came up with an elegant (I think) solution to a problem that
has been perplexing us for a while. The problem is that the JDBC API
requires all parameters to be passed as strings when you are making a
connection. If you are creating an olap4j connection to mondrian, and
access to the underlying data warehouse is via a
[javax.sql.DataSource](https://download.oracle.com/javase/6/docs/api/javax/sql/DataSource.html)
object, not a connect string, then you cannot pass in that
DataSource. If you have created your own Role object to do customized
access-control, you cannot pass in the object, you have to pass in the
name of a role already defined in the mondrian schema (or a
comma-separated list of role names).

I invented a
[LockBox](https://mondrian.pentaho.com/headapi/mondrian/util/LockBox.html)
class, that acts as a special kind of map that has some of the
characteristics of a directory service. There is one lock box per
server. If you have an object you wish to pass in, then you register
it with the lock box, and the lock box gives you a string moniker to
reference that object. That moniker is unique for the duration of the
server, and near impossible for an unauthorized component guess. You
can pass it to other components, and they can access the object.

The lock box automatically garbage collects unused objects. When an
object is registered, the lock box returns an entry object to the
caller that contains both the string moniker and the object
itself. The entry is the key to a
[WeakHashMap](https://download.oracle.com/javase/6/docs/api/java/util/WeakHashMap.html),
so when the client forgets the entry, the object is eligible to be
garbage-collected out of the lock box. This guarantees that the lock
box will not fill up over time due to clients forgetting to deregister
objects.

LockBox does not purport to be a full directory service -- in
particular, objects are only accessible within the same JVM -- but it
carries out a simple purpose, efficiently and elegantly, and may be
useful to other applications.
