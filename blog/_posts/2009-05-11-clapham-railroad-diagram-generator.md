---
layout: post
title: 'Clapham: A railroad diagram generator'
date: '2009-05-11T01:29:00.000-07:00'
author: Julian Hyde
tags:
- bnf javacc parser grammar generator
modified_time: '2009-05-11T02:07:43.057-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-6355218775713867132
blogger_orig_url: https://julianhyde.blogspot.com/2009/05/clapham-railroad-diagram-generator.html
---

I don't work with the Oracle database very much anymore, and one thing
I miss is their server documentation. I still have my old copy of the
Oracle 7.3 SQL Language Reference, and sometimes I reach for it when
the SQL:2008 standard has fuddled my brain and I want to be reassured
that SQL can be simple, powerful and trustworthy. The calming effect
is partly due to the authoritative tone, but the railroad diagrams
[describing the syntax of each command say 'Don't worry'](https://download.oracle.com/docs/cd/B19306_01/server.102/b14200/statements_7002.htm).

For example, here is
[Oracle 10.2's CREATE TABLE](https://download.oracle.com/docs/cd/B19306_01/server.102/b14200/statements_7002.htm):

<a onblur="try {parent.deselectBloggerImageGracefully();} catch(e) {}"
    href="/assets/img/relational_table.gif">
  <img style="margin: 0px auto 10px; display: block; text-align: center; cursor: pointer; width: 596px; height: 218px;"
    src="/assets/img/relational_table.gif"
    alt="CREATE TABLE railroad diagram (Oracle 10.2)" border="0" />
</a>

Yes, railroad diagrams. You can easily get lost in something as large
as the SQL language, with its hundreds of commands, keywords and
unexpected clauses, and railroad diagrams are the map.

When it came to writing our documentation for
[SQLstream](https://www.sqlstream.com/), we of course wanted to
include railroad diagrams to illustrate our dialect of SQL. It's
possible to construct the diagrams by hand, but it's tedious, error
prone, and it's difficult to get the diagrams to look
consistent. Unbelievably, we couldn't find a tool to generate them, so
we ended up writing them by hand.

Now I've gotten a little breathing room after the release of SQLstream
2.0, I took a couple of days to write an open-source railroad diagram
generator. I've released it on
[Sourceforge, and named it Clapham](https://sourceforge.net/projects/clapham),
after the South London town which is home to the most
complicated railway junction you ever saw.

This has been a nice return to old-school open source, with its
mantras "release early, release often"; and "don't whine:
contribute". The diagrams aren't yet as pretty as Oracle's, but we're
getting there. Even though this is the very first release, and the
project is barely alpha, it has already
[generated charts for LucidDB's not inconsiderable SQL grammar](http://clapham.hydromatic.net/farrago/).

More details at the [home page](http://clapham.hydromatic.net/),
and you can
[download release clapham-0.1.003 from SourceForge](https://sourceforge.net/project/showfiles.php?group_id=243703&amp;package_id=297002&amp;release_id=681840).
Contributions welcome, of course.
