---
layout: post
title: What's in a name?
date: '2006-12-03T15:13:00.000-08:00'
author: Julian Hyde
tags:
modified_time: '2006-12-03T18:07:52.871-08:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-864542441104022480
blogger_orig_url: https://julianhyde.blogspot.com/2006/12/its-not-often-that-i-am-criticized-for.html
---

It's not often that I am criticized for being technically
imprecise. After all, I'm an open-source code monkey and proud of
it. This time, the criticism came from a marketing guy, and even more
surprisingly, he was right.

I was discussing [the launch](https://www.niallkennedy.com/blog/archives/2006/11/google-mondrian.html)
of an internal Google tool called 'Google Mondrian' with
Lance Walter, who runs marketing at Pentaho, but is also technical
enough to tell a big-endian half-adder from a Python cross-compiler if
he were to trip over one in his front lawn. Google Mondrian is a
web-based code-review tool written by Guido van Russum, the author of
Python, and is apparently in widespread use internally within Google.

Our concern was that a high-profile announcement would somehow
overshadow the Mondrian open-source OLAP project, but we fairly
quickly concluded that this was unlikely. Supposing that this thing
becomes as big for online code-reviews as, say,
[CruiseControl](https://cruisecontrol.sourceforge.net/) is for
continuous integration, would that prevent people from finding and using
Mondrian? Probably not. And the nice thing about the launch was the
[slashdottage from people leaping to our defense](http://developers.slashdot.org/comments.pl?sid=209388&cid=17073782).

But, I concluded, "we'd probably better use the phrase 'Mondrian OLAP
Server' so that no one gets confused".

Lance wasn't having that. "I swear I heard you at one point say that
it was technically innaccurate/misleading to call Mondrian an OLAP
*Server*. You designed Mondrian to be lightweight, so it could just as
easily be embedded in an application as run as a server in a
three-tier architecture."

He was right, of course. I was not being consistent. Mondrian is
variously described as
"[an OLAP (online analytical processing) database written in Java](https://sourceforge.net/projects/mondrian)",
"[an OLAP server written in Java](https://mondrian.pentaho.org/)" and
"[an Open Source OLAP (online analytical processing) server, written in the Java
programming language](https://en.wikipedia.org/wiki/Mondrian_OLAP_server)".
(I didn't write the last one.) "The Mondrian OLAP engine" is a more
accurate term, because "engine" has been used to describe embeddable
relational databases like Apache Derby and the Microsoft JET
Engine. It's easy to accidentally use the phrase "OLAP Server" because
the first commercial OLAP engines all had
I'm-the-center-of-the-Universe architectures -- the logical thing to
do if you're trying to shift $1M of software, because people will pay
a lot more for a server than for a library -- and so that's become the
standard term.

It's a Faustian dilemma which often crops up in technical
marketing. Do you describe what your product actually is and does, or
do you tailor your description to what you think your audience is
looking for, even if that is a little inaccurate? Incidentally, I make
no apologies for 'marketing' Mondrian. Even if I believed that no one
should ever pay another cent for software (and I don't -- I'm not that
much of an idealist), I would still have a duty to describe and
publicize my project so that its target audience can find it. I'd
rather that people find Mondrian by googling [see <span style="color:
rgb(204, 102, 0);">note below</span>] "olap server", then read the
fine print and discover, "Wow, it's embeddable too!", than not to find
it at all.

So, I suspect that Mondrian will remain the "Mondrian OLAP Server" or,
to its friends, just "Mondrian".

<hr />

<small><span style="color: rgb(153, 51, 0);">Note to Google's
legal department</span>: I know you don't like me to
[use 'google' as a verb](https://en.wikipedia.org/wiki/Google_%28verb%29).
It must be horrible to be so successful that you are part
of the English language, as in 'I xeroxed a polariod of my jeep's
collision with a dumpster'. I really feel for you. But since we're now
sharing the term 'mondrian', I figure you owe me one.</small>
