---
layout: post
title:  "Blog reboot"
date:   2020-02-18 10:51:00 -0800
author: Julian Hyde
image:  /assets/img/P2019515_j6d_IMG_0383_lb.JPG
tweet:  https://twitter.com/julianhyde/status/1229847506403479553
---
![Julian Hyde (in Chicago, May 2019)](/assets/img/P2019515_j6d_IMG_0383_lb.JPG "Julian Hyde (in Chicago, May 2019)"){:style="float: right;margin: 10px;border: 1px solid black;"}

Welcome back!

Almost six years since my last blog post, I'm sharpening my electronic
pencil to start writing again.

# From Blogger to Jekyll

Previously at [Blogger](http://julianhyde.blogspot.com/), the blog is
now self-hosted at
[blog.hydromatic.net](http://blog.hydromatic.net). I figured that the
readers wouldn't miss the cheesy ads, I won't miss the spam comments,
and the conversations started by the posts can all happen on
[Twitter](https://twitter.com/julianhyde) or in other people's blogs.

I've ported all of the previous posts to
[Jekyll](https://jekyllrb.com/),
[Markdown](https://kramdown.gettalong.org/) and stored them in
[GitHub](https://github.com/julianhyde/share/tree/main/blog).
They're available in the [index](/). I must say, having full control
over my content is quite a relief.

# Update

A few things have changed since my last post, but many things have
stayed the same. Around 2012, I was writing a lot about the [Mondrian
OLAP engine](https://en.wikipedia.org/wiki/Mondrian_OLAP_server) and
[olap4j](http://olap4j.org) API, but my work on Mondrian has since
tailed off. I had a promising SQL parsing/planning project called
[Optiq](https://github.com/julianhyde/optiq) that has since moved to the
[Apache Software Foundation](https://apache.org/)
and is thriving as [Apache Calcite](https://calcite.apache.org/).

My work on streaming SQL, which started at
[SQLstream](https://sqlstream.com), has continued in
[Apache Calcite](https://calcite.apache.org/docs/stream.html), found
collaborators in Apache Beam and Flink, and resulted in a
[well-received paper](https://blog.acolyer.org/2019/07/03/one-sql-to-rule-them-all/)
at [SIGMOD 2019](https://arxiv.org/pdf/1905.12133.pdf). With
luck, a future version of the SQL standard will have extensions for
streaming queries.

At that time I worked for SQLstream and
[Pentaho](https://pentaho.com); I have since worked on Apache Calcite,
Hive and Hadoop at [Hortonworks](https://hortonworks.com), and I now
work at [Looker](https://looker.com) (which last week completed its
[merger with Google](https://techcrunch.com/2020/02/13/google-closes-2-6b-looker-acquisition/)).

# Why blog?

Why did I stop blogging? At that time, Calcite was starting to grow
really fast and my energies went into Calcite features, releases and
conference talks. Oh, and I also had
[a newborn and a 3 year old]({% post_url 2009-02-16-welcome-sebastian-hyde %}).

Twitter played its part. As the leading
[microblogging](https://en.wikipedia.org/wiki/Microblogging) service,
it allowed me to vent my passion and bounce those idea off
audience. But by the time I had blown off steam in 140 characters, I
no longer had the passion or outrage to rework the idea into a blog
post.

But I've come to realize that ideas need more room to breathe. In
technology, *why* you are doing something is often more important than
*what* you are doing. You can develop an idea over several posts, and
bring your audience along. After the product is complete, the blog
will show the thinking that went into it (and perhaps a few wrong
turns along the way). That's what I hope to do here.

# The cool stuff

I am interested in databases and business intelligence (BI),
especially from the perspective of relational algebra and query
optimization. I want to extend the database paradigm, to areas such as
streaming queries and geospatial data, and on language design, to make
database technology more useful.  Lastly, I want to bring the
technology to a wide audience via open source software.

I'll be writing about those things here. I'm especially keen to
introduce you to [Morel](https://github.com/julianhyde/morel/), a
language that I am developing. It is a small functional language
derived from [Standard ML](https://en.wikipedia.org/wiki/Standard_ML)
that is also an elegant and powerful database query language, and I
think it has a bright future.

Watch this space.

# Comments

If you have comments, please reply on Twitter:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>
