---
layout: post
title: 'A what-if scenario: musing about adding writeback capability to Mondrian'
date: '2009-04-05T15:50:00.000-07:00'
author: Julian Hyde
tags:
- mondrian olap writeback splash olap4j palo jpalo
modified_time: '2009-04-05T16:06:48.795-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-903810947836140325
blogger_orig_url: https://julianhyde.blogspot.com/2009/04/what-if-scenario-musing-about-adding.html
---

The [Pentaho Partner Summit](https://www.pentaho.com/summit09/)
last week was a great chance to meet people who are using
-- and being successful -- with Mondrian.

As always, people are thinking of using it in ways that I hadn't
imagined. A couple of comments got me thinking about adding writeback
support, something we'd long talked about, but seriously considered
implementing.

Writeback allows the OLAP end-user to modify cell values and see the
effects ripple through their spreadsheet. As you can imagine, it is
useful for doing what-if analysis, especially budgeting.

If the cell is a sum of finer-grained cells, we need to modify those
finer-grained cells also, and all of the totals of other
dimensionalities created from those finer-grained cells, otherwise
things just don't add up. This is hard to implement, because you
sometimes need to modify a lot of cells, and even harder for ROLAP
engines like Mondrian, because such engines don't store cells, they
read directly from the unaggregated fact table.

First, I went looking for existing APIs for writeback.

Microsoft offers support for writeback via the
[UPDATE CUBE](https://msdn.microsoft.com/en-us/library/ms145568.aspx)
MDX statement. As always with Microsoft's MDX support, it's
difficult to tell whether this is 'standard MDX', but the command
seems to be well thought-out. The fact that it is an MDX command
rather than an API call allows them to use an MDX expression as the
rule by which to pro-rate changes to child cells.

I also looked at the [JPalo Java API](http://www.jpalo.com/en/products/palo_java_api.html).
(I've always wanted to work more closely with [Palo](http://www.palo.net/).
Although they're an OLAP engine,
they have a different architecture (C and MOLAP) and core target
audience (Excel users), and they're open source, so I see a lot of
benefit to them and us if we pool resources. I invited them to join
the [olap4j](http://www.olap4j.org/) process early on, but
they preferred to define their own Palo-specific Java API. I'm still
hopeful.)

I [downloaded their most recent release from SourceForge](https://sourceforge.net/project/showfiles.php?group_id=209776)
and found that it was a release out of date (2.0 versus 2.5) and
didn't contain the source code. There is a more up-to-date version in
[subversion](https://jpalo.svn.sourceforge.net/viewvc/jpalo/trunk/). In
`DbConnection` I found the `setDataNumericSplashed` method:

{% highlight java %}
/**
  * Sets the given double value at the specified cell.
  * The splashMode paramater is only important for consolidated cells and
  * determines how the value is scattered among the consolidated elements.
  * Please use the defined class constants for valid values. Although more
  * modes are currently defined only three are supported, namely:
  * SPLASH_MODE_DEFAULT, SPLASH_MODE_BASE_SET and SPLASH_MODE_BASE_ADD
  *
  * @param cube {@link CubeInfo} representation
  * @param coordinates {@link ElementInfo} representations which specify the
  * coordinates
  * @param value the new value
  * @param splashMode the splash mode, use defined class constants
  */
 public void setDataNumericSplashed(CubeInfo cube, ElementInfo[] coordinate,
     double value, int splashMode);
{% endhighlight %}

I couldn't find any more documentation than that, but 'splash mode'
seems to be equivalent to Microsoft's update strategies
`USE_EQUAL_ALLOCATION` etc.

There are
several remaining questions. What are the right changes to the olap4j
API to support writeback? Support for the `UPDATE CUBE` statement is the
leading contender. I'd love to hear what the olap4j community --
especially the folks building the
[Pentaho Analysis Tool](https://wiki.pentaho.com/display/COM/Pentaho+Analysis+Tool) --
think of this API, and how they would expose writeback in their UI.

I presume we'll need a scheme for transaction management. End-users
will want to save their work, come back another day and continue where
they left off. Several end-users might be using Mondrian at the same
time, and want to see their numbers, not anyone else's. So, I think
we'll need to introduce a concept I'd call a 'scenario', which is a
property of a connection and can be persisted.

We'll need to figure out how to implement writeback within a
Mondrian's ROLAP-with-caching architecture. Writing to the fact table
is not tenable, because the modified cells can be of a multitude of
dimensionalities. Neither is writing to an aggregate table, for the
same reason. Ideal would be to write to disk a minimal description of
the cells the user has modified -- in XML, say -- and do the other magic
in the caching layer.

Lastly, I just need to find time to implement it.
