---
layout: post
title: Mondrian 2.2, Cube Designer, and CurrentDateMember
date: '2006-10-15T23:12:00.000-07:00'
author: Julian Hyde
tags:
modified_time: '2006-10-16T00:32:12.913-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-6211598162303899041
blogger_orig_url: https://julianhyde.blogspot.com/2006/10/mondrian-22-cube-designer-and.html
---

I made the first release candidate of mondrian-2.2 a few days ago.

The most exciting feature is Pentaho's brand new
[Cube Designer](https://sourceforge.net/project/shownotes.php?group_id=35302&release_id=454703):

> The Pentaho Cube Designer simplifies and expedites the analysis
> process by eliminating the need for administrators to use XML to
> create new OLAP schemas. Instead, Cube Designer’s wizard-driven
> graphical user interface walks administrators through the steps of
> connecting to relational data sources, defining analytical dimensions,
> identifying measures for analysis, and publishing the schema to the
> Pentaho server.

At last, a serious UI for building Mondrian schemas! I've been helping
the Pentaho folks develop this for the last few months, and was
delighted when they decided to release it open-source.

It's been almost four months since Mondrian release 2.1. There are no
earth-shatteringly huge new features in the core Mondrian 2.2, because
I've been busy with the Cube Designer and [olap4j](http://www.olap4j.org).
But if you peruse the
[change log](https://sourceforge.net/project/shownotes.php?release_id=454606&amp;group_id=35302),
you'll see there have been lots of development activity, resulting in
numerous fixes in areas such as performance and native SQL generation.

A useful addition is *a set of functions for accessing the current
date*, contributed by Zelaine Fong and Benny Chow. If MDX had a date
datatype, we would have implemented these functions long ago. But MDX
doesn't have a date datatype, because everyone who uses dates wants to
access them as a dimension (say, `[Time].[2006].[October].[15]`), not
scalar values. To make things worse, we noticed that everyone's Time
dimension is structured differently: some have quarters while some
don't; some use month names while others use month numbers; and some
time dimensions are structured on weeks rather than months.

So, Benny and Zelaine created a function which formats the current
date as a string, then looks up a member of the time dimension. For
example,

{% highlight sql %}
SELECT { [Measures].[Unit Sales] } ON COLUMNS,
  { CurrentDateMember([Time],
      '[\"Time\"]\\.[yyyy]\\.[\"Q\"q]\\.[m]\\.[d]').Lag(3)
  : CurrentDateMember([Time],
      '[\"Time\"]\\.[yyyy]\\.[\"Q\"q]\\.[m]\\.[d]') } ON ROWS
FROM [Sales]
{% endhighlight %}

prints the sales total for today and the previous three days.

The trick is with the format string,
'`[\"Time\"]\\.[yyyy]\\.[\"Q\"q]\\.[m]\\.[d]`'. This is substituted
with the current date, to give `[Time].[2006].[Q4].[10].[15]`. Then
Mondrian looks in the `[Time]` dimension to find a member with that
unique name.

The quotes around `"Time"` and the backslashes before the dots (`\\.`)
are needed to prevent the formatting facility from replacing things
that it shouldn't. It makes the code a little difficult to read, but
it allowed us to use
[the existing formatting facility](https://mondrian.sourceforge.net/api/mondrian/util/Format.html),
which is well-tested and well-documented, rather than inventing a new
one just for this function.

Now suppose that the granularity of the time dimension is less than
daily. In a monthly time dimension, should 15th October fall resolve
to `[2006].[10]` or `[2006].[11]`? The `BEFORE`, `AFTER` and `EXACT`
keywords help Mondrian decide. So,

{% highlight mdx %}
CurrentDateMember([Time],
  '[\"Time\"]\\.[yyyy]\\.[\"Q\"q]\\.[m]',
  BEFORE)
{% endhighlight %}

would evaluate to the greatest member which is less than the current
date, namely, `[Time].[2006].[10]`.

This function fits well with another feature in mondrian-2.2, which I
intend to blog about in a few days, is the *extension of Parameters to
system, schema, and session scope*. Parameters are now much, much more
than bind variables, and are a great way of sharing constants and
expressions across your whole application.

So, while you're waiting for that blog entry... go and download the
new mondrian release candidate, and try it out. Hopefully, the full
release will be out this week or next.
