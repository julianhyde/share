---
layout: post
title: mondrian heading for 4.0
date: '2010-07-23T17:45:00.000-07:00'
author: Julian Hyde
tags:
- mondrian 4.0 xml schema unit test
modified_time: '2010-07-23T20:27:43.132-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-1308386280460423815
blogger_orig_url: https://julianhyde.blogspot.com/2010/07/mondrian-heading-for-40.html
---

I am currently working on mondrian-4.0 and am reworking a lot of
mondrian's internals, particularly in how schemas are loaded and
validated, and in the mapping of levels and measures onto star
schemas. Regression tests for bugs and features are always important,
but they are especially important right now.

### Regression tests, now more than ever

Usually when I am changing mondrian, if I see a piece of logic in the
code, I will try to preserve that logic, and will rework it if
necessary when internal APIs change. That goes for other committers
too. But I don't propose to do that for mondrian-4.0: the changes to
the code are quite widespread, and besides, it's chance to clean out
some of the cruft that has built up over the years.

Yes, it's true. Even in mondrian's impeccably clean code -- almost
300,000 lines of it -- there are some pieces of code that we're not
sure are actually used. My natural inclination as a developer is to
remove that code, and see whether anything breaks. If that piece of
code is a something you contributed but didn't write a test for, then
nothing will break, and your code will be on the cutting room floor of
history.

So, if you have contributed a feature or bug fix to mondrian over the
past years or months, make sure that there is a test case checked in
as part of mondrian's regression test suite. I will do my best to make
sure that the test case stays working, even if the code underneath is
all different. **If there isn't a test, your beloved feature may just
disappear in mondrian-4.0**.

### Schema changes, and compatibility

In mondrian-4.0 there will be changes to how schemas are structured. A
few examples:
* Virtual cubes will be obsolete, or more precisely, any cube can have
  multiple groups of measures, each group based on a separate fact
  table.
* Linkages between snowflake tables, currently specified using the
  `<Join>` element, will be specified using new `<PhysicalSchema>`
  element that declares table usages, relationships, and derived
  columns for the whole schema.
* The present XML grammar isn't very forgiving if you get things in
  the wrong order: if you define a `<NamedSet>` before your
  `<CalculatedMember>`s in a cube, mondrian currently ignores all
  calculated members. The new XML grammar will be more forgiving.

I recently decided that the XML grammar is sufficiently different that
I would create a new XML grammar. But
[I promised](https://wiki.pentaho.com/display/analysis/Physical+Schema+Design+Discussion)
that mondrian would be backwards compatible, and I will
stand by that. There will be a converter that will recognize an
old-style schema, convert it to a new-style schema in memory, and then
proceed to load the new-style schema. So, old-style schemas should
continue to work. Mondrian-4.0

In a few weeks I will be ready to release the specification for the
new-style schemas. I would appreciate review of the new schemas. Since
it is a major change, mondrian-4.0 will have a long beta phase. During
which time I could use your help testing both new features and
backwards compatibility.
