---
layout: post
title: Gathering requirements for olap4j 2.0
date: '2013-06-03T10:35:00.000-07:00'
author: Julian Hyde
tags:
modified_time: '2013-06-03T10:35:00.926-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-7997117779151337818
blogger_orig_url: https://julianhyde.blogspot.com/2013/06/gathering-requirements-for-olap4j-20.html
---

It's time to start thinking about olap4j version 2.0.

My initial goal for olap4j version 1.0 was to decouple application
developers from Mondrian's legacy API. We've far surpassed that
goal. Many applications are using olap4j to connect to OLAP servers
like Microsoft SQL Server Analysis Services, Palo and SAP BW. And
projects are leveraging the `olap4j-xmlaserver` sister project to
provide an XMLA interface on their own OLAP server. The need is
greater than ever to comply with the latest standards.

The difference between products and APIs is that you can't change APIs
without pissing people off. Even if you improve the API, you force the
developers of the drivers to implement the improvements, and the users
of the API get upset because they don't have their new drivers
yet. There are plenty of improvements to make to olap4j, so let's try
to do it without pissing too many people off!

Since olap4j version 1.0, there has been a new release of Mondrian
(well, 4.0 is not released officially yet, but the metamodel and API
are very nearly fully baked) and a new release of SQL Server Analysis
Services, the home of the de facto XMLA standard.

Also, the Mondrian team have spun out their XMLA server as a
[separate project (olap4j-xmlaserver)](https://github.com/olap4j/olap4j-xmlaserver)
that can run against any olap4j driver. If this server is to implement
the latest XMLA specification, it needs the underlying olap4j driver
to give it all the metadata it needs.

Here's
[an example of](https://sourceforge.net/p/olap4j/discussion/577988/thread/d5bacb80/)
the kind of issue that we'd like to fix. In olap4j 1.x,
you can't tell whether a hierarchy is a parent-child hierarchy. People
have asked for a method

{% highlight java %}
boolean isParentChild();
{% endhighlight %}

Inspired by the the `STRUCTURE` attribute of the
[`MDSCHEMA_HIERARCHIES` XMLA request](https://msdn.microsoft.com/en-us/library/ms126062.aspx),
we instead propose to add

{% highlight java %}
enum Structure {
  FULLYBALANCED,
  RAGGEDBALANCED,
  RAGGED,
  NETWORK
}
Structure isParentChild();
{% endhighlight %}

We can't add this without requiring a new revision of all drivers, but
let's be careful gather all the requirements so we can do it just this
once.

Here are my goals for olap4j 2.0:

* Support Analysis Services 2012 metamodel and XMLA as of Analysis
  Services 2012.
* Create an `enum` for each XMLA enum. (`Structure`, above, is an
  example.)
* Support Mondrian 4.0 metamodel. Many of the new Mondrian features,
  such as measure groups and attributes, are already in SSAS and XMLA.
* Allow user-specified metadata, such as those specified in Mondrian's
  schema as annotations, to be passed through the olap4j API and XMLA
  driver.
* We'll know that we've done the right thing if we can remove
  [MondrianOlap4jExtra](https://mondrian.pentaho.com/api/mondrian/olap4j/MondrianOlap4jExtra.html).

I'd also like to maintain backwards compatibility. As I already said,
drivers will need to be changed. But any application that worked
against olap4j 1.1 should work against olap4j 2.0, and any driver for
olap4j 2.0 should also function as an olap4j 1.x driver. That should
simplify things for the users.

I'll be gathering a detailed list of API improvements in the
[olap4j 2.0 specification](https://github.com/olap4j/olap4j/blob/master/olap4j_version_2_specification.md).
If you have ideas for what should be in olap4j version 2.0, now is the time to
[get involved](https://lists.sourceforge.net/lists/listinfo/olap4j-devel)!
