---
layout: post
title: Pentaho Analyzer
date: '2009-10-05T17:37:00.000-07:00'
author: Julian Hyde
tags:
- pentaho analyzer jpivot pat open source olap viewer
modified_time: '2009-10-06T09:24:18.255-07:00'
thumbnail: https://3.bp.blogspot.com/_BVv0WTpeWTs/SsqXldTlnwI/AAAAAAAAAD0/Pga8sxxd-zs/s72-c/analyzer_table.png
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-1464049853761794320
blogger_orig_url: https://julianhyde.blogspot.com/2009/10/pentaho-analyzer.html
---

Pentaho today [announced a new OLAP viewer](https://www.pentaho.com/news/releases/20091005_pentaho_announces_strategic_technology_acquisition.php),
called Pentaho Analyzer Enterprise Edition,
based on LucidEra's ClearView component.

<a onblur="try {parent.deselectBloggerImageGracefully();} catch(e) {}"
    href="/assets/img/analyzer_table.png">
  <img style="float:right; margin:0 0 10px 10px;cursor:pointer; cursor:hand;width: 320px; height: 256px;"
      src="/assets/img/analyzer_table.png" border="0" alt="Pentaho Analyzer"
      id="BLOGGER_PHOTO_ID_5389286573879959298" />
</a>

This is great news for Pentaho customers, the community, and the BI
world at large. While [Pentaho Analysis (Mondrian)](https://www.pentaho.com/products/analysis/)
is one of its strongest components, the current OLAP
viewer (based on [JPivot](https://jpivot.sourceforge.net/))
has been one of its weakest.

The new viewer puts Pentaho at the top of the heap, in competition
with best-of-breed OLAP viewers. It is designed to be intuitive for
business users (yes, those people who don't speak MDX!), is built
using the latest web technologies, and integrates seamlessly with
Mondrian and the rest of the Pentaho suite.

It is going to revolutionize the experience of using OLAP within the
Pentaho suite.

Naturally, [there are concerns](https://www.tholis.com/news/pentaho-quo-vadis-/).
First, the new viewer is only part of Pentaho's
Enterprise Edition (EE) suite. If Pentaho is committed to open source
BI, why not release it open source? Second, what will happen to
[Pentaho Analysis Tool (PAT)](https://code.google.com/p/pentahoanalysistool/),
the successor to JPivot being developed by the Pentaho
community? I'd like to take the opportunity to answer these concerns,
because I think this is news that everyone should be celebrating.

## Why is the new Analyzer not open source?

There's been a lot of talk about open source business models, 'open
core', good and evil, and all that. Releasing ClearView as part of
Enterprise Edition is perfectly in sync with Pentaho's business model
and with my intuitions about what makes sense for open source. Here's
my rationale.

If you release a piece of software open source out of sheer, 'I love
the world!' altruism, you won't necessarily see much benefit. Pentaho
is a for-profit business, and they are savvy about leveraging the
benefits of open source software. And let's not kid ourselves, there
are considerable downsides to releasing something open source. Your
competitors can pick up the software and incorporate your hard work
into their suite. And your customers may decide that the free version
is so good that they aren't going to give you any of their money.

Open source allows you to bring a component to a wider audience, an
audience that will test, document and improve the component, and will
support each other on the forums. Only the Community Edition (CE)
components get that boost. Therefore, Pentaho's strategy is to release
the core functionality in CE. That means the high-performance core of
the system, the code paths that get run trillions of times an hour,
and that means all the components that are necessary to build a
functional and useful BI application.

In particular, people ask me whether there is a high-performance
'Mondrian on steroids' in EE. No there isn't. None of us want to
maintain alternative code-paths, because the extra complexity would
slow down future development. If I were to create a performance
optimization in EE, the community would probably replicate that
optimization in CE within a few weeks. Improving the core Mondrian
system for everyone brings more people into the community, and that
brings more people to EE.

And by the way, this doesn't just apply to the Pentaho Analysis part
of the suite. Pentaho adds major new functionality to the suite each
release, and most of that goes into open source components.

So, what's left to go into EE? Bells and whistles, things that make
the product easier to use, easier to manage, and things that make your
boss want to reach for his or her checkbook. And of course support,
releases that are certified and indemnified, and more regular. I don't
think that's a bad deal, however you look at it.

It also helps if the components are delivered under a
business-friendly license like [LGPL](http://www.gnu.org/copyleft/lesser.html)
or [EPL](http://www.eclipse.org/legal/epl-v10.html). Otherwise
you will not attract contributions from OEM vendors, who are the
companies with the skills to extend components as complex as Mondrian
or Pentaho Data Integration (Kettle). Once again, Pentaho is taking a
risk by using business-friendly licenses, because there is always a
chance that Pentaho's competitors will scoop up the fruits of its
labors. (As in fact [they do](http://www.jaspersoft.com/jasperanalysis).)

<a onblur="try {parent.deselectBloggerImageGracefully();} catch(e) {}"
    href="/assets/img/analyzer_chart.png">
  <img style="float:left; margin:0 10px 10px 0;cursor:pointer; cursor:hand;width: 320px; height: 256px;"
    src="/assets/img/analyzer_chart.png"
    border="0" alt="" id="BLOGGER_PHOTO_ID_5389287018522115922" />
</a>

But Pentaho's faith in the open source process pays off. ClearView is
proof of that. If Mondrian had not been available under a
business-friendly open source license, LucidEra would probably have
written it on top of another vendor's engine, and Pentaho would not
have been able to use it. Incidentally, LucidEra has contributed many
important enhancements to Mondrian in areas of both performance and
functionality over the past three years. This has improved Mondrian
for everyone, and we know that ClearView performs very well against
Mondrian.

## What will happen to PAT?

To restate what I said above, there is a network effect when you make
a component open source. The more people that use a component, the
more people are going to contribute to it. We want as many people to
use Mondrian as possible, and in particular we want the right people
to use it (the people who are going to make major improvements).

So, for Mondrian's continuing health as an open source component, we
need the Community Edition of Mondrian to be good enough to build
business applications on. For that, we need to make PAT successful.

I personally have been laying the ground work for PAT for a number of
years. I spearheaded the [olap4j](http://www.olap4j.org/)
API, knowing that the community would be more likely to write the next
generation OLAP viewer if it was guaranteed to be portable across OLAP
engines. Then I kicked off the halogen project, a collaboration
between Pentaho developers and the community to build a viewer using
olap4j and GWT. Pentaho developers contributed code and user interface
design to that project, even working in their spare time when the
current Pentaho sprint used up all of their 'official' cycles. And the
PAT project used the halogen code, and the knowledge of the halogen
developers, as a starting point.

It's not healthy to have too close a relationship between an OLAP
server and viewer. There should always be room for competition, an
opportunity to use a new viewer or (gasp!) different OLAP server if
the 'standard' one isn't ideal. I created olap4j with competition in
mind, and the experiment seems to be working: PAT can run against
Mondrian's native interface, Mondrian's XMLA server, and against SQL
Server Analysis Services via XMLA.

I want to make it easier to build alternative front-ends on top of
olap4j, so I have been encouraging PAT developers to contribute to
olap4j's query model and library of transforms. I would like to see
Analyzer move to olap4j internally (it currently uses Mondrian's
native API), and perhaps migrate some of the logic in Analyzer to
olap4j so that we can share the costs of maintaining it with the
community.

Lastly, as I realized at the recent community meetup in Barcelona, we
have a great team, and we need to harness their energy. After a beer
or two with PAT developers [Tom and Paul](https://twitpic.com/ia5go),
some inspiring demos from [Pedro and Daniel](https://twitpic.com/ia1oi),
we hatched ideas of incorporating spark lines and writeback into PAT,
and I'm sure the ideas will keep on flowing. With this much
inspiration and hard work coming from the community, how can we
possibly fail?
