---
layout: post
title: How Mondrian names hierarchies
date: '2011-08-24T10:40:00.000-07:00'
author: Julian Hyde
tags:
- mondrian mdx parsing
modified_time: '2011-08-24T10:40:29.458-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-5520527643985077935
blogger_orig_url: https://julianhyde.blogspot.com/2011/08/how-mondrian-names-hierarchies.html
---

You may or may not be aware of the property
[mondrian.olap.SsasCompatibleNaming](https://mondrian.pentaho.com/api/mondrian/olap/MondrianProperties.html#SsasCompatibleNaming).
It controls the naming of elements, in particular how Mondrian names
hierarchies when there are multiple hierarchies in the same
dimension.

Let's suppose that there is a dimension called `Time`, and it contains
hierarchies called `Time` and `Weekly`.

If `SsasCompatibleNaming` is `false`, the dimension and the first
hierarchy will both be called `[Time]`, and the other hierarchy will
be called `[Time.Weekly]`.

If `SsasCompatibleNaming` is `true`, the dimension will be called
`[Time]`, the first hierarchy be called `[Time].[Time]`, and the other
hierarchy will be called `[Time].[Weekly]`.

As you can see, `SsasCompatibleNaming` makes life simpler, if slightly
more verbose, because it gives each element a distinct name. There are
knock-on effects, beyond the naming of hierarchies. The most subtle
and confusing effect is in the naming of levels when the dimension,
hierarchy and level all have the same name. If `SsasCompatibleNaming`
is `false`, then `[Gender].[Gender].Members` is asking for the members
of the gender *level*, and yields two members. If
`SsasCompatibleNaming` is `true`, then `[Gender].[Gender].Members` is
asking for the members of the gender *hierarchy*, and yields three
members (`all`, `F` and `M`).

Usually, however, Mondrian is forgiving in how it resolves names, and
if elements have different names, it will usually find the element you
intend.

The default value is `false`. However, that leads to naming behavior
which is not compatible with other MDX implementations, in particular
Microsoft SQL Server Analysis Services (versions 2005 and later).

From mondrian-4 onwards, the property will be set to `true`. (You
won't be able to set it to `false`.)  This makes sense, because in
mondrian-4, with attribute-hierarchies, there will typically be
several hierarchies in each dimension. We will really need to get our
naming straight.

What do we recommend? If you are using Pentaho Analyzer, Saiku or
JPivot today, we recommend that you use the default value,
`false`. But if you are writing your own MDX (or have built your own
client), try setting the value to `true`. The new naming convention
actually makes more sense, and moving to it now will minimize the
disruption when you move to mondrian-4.

I am just about to check in a change that uses a new, and better name
resolution algorithm. It will be more forgiving, and
standards-compliant, in how it resolves the names of calculated
members. However, it might break compatibility, so it will only be
enabled if `SsasCompatibleNaming` is `true`.

Are you using this property today? Let us know how it's working for
you.
