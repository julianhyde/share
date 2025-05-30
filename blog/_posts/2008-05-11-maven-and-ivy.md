---
layout: post
title: Maven and Ivy
date: '2008-05-11T13:59:00.000-07:00'
author: Julian Hyde
tags:
- maven ivy mondrian
modified_time: '2008-05-11T14:34:32.236-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-5421935830813185509
blogger_orig_url: https://julianhyde.blogspot.com/2008/05/maven-and-ivy.html
---

We experimented a few years ago using
[Apache Maven](https://maven.apache.org/) to manage the Java
libraries that mondrian depends on. Maven looked great on paper, but
it was tricky to set up, and since simplicity was the goal in the
first place, we gave up.

Since then Maven2 has been released. Version 2 has a new architecture
and is by all accounts a great improvement. I have recently been using
Maven for a project with a lot of dependencies on other projects and
it helps a lot. It imposes a structure on the dependencies by forcing
you to name and version your projects in a certain way, and provides a
central repository to put them in. You can provide local repositories
for projects only you or your project team are using.

Still, Maven seems to be a one-trick pony, even though it's a great
trick. Maven generates distributions, javadoc, project pages, code
coverage, and so forth, but for mondrian the only job I want it to do
is dependency management. I don't want to throw away the considerable
investment we've made in mondrian's ant scripts. I'd rather have
something that adds dependency management to my existing framework.

I've just come across [Ivy](https://ant.apache.org/ivy/),
and it seems to be just the ticket. Ivy aims to do
dependency-management within an ant framework, and it uses Maven's
repository and metadata protocols to manage its libraries. It's just
been accepted by Apache as a sub-project of
[Ant](https://ant.apache.org/), so we know that its integration
with ant will continue to get better.

I went through
[Ivy's tutorial](https://ant.apache.org/ivy/history/latest-milestone/tutorial.html)
and was impressed that Ivy could bootstrap itself using
just ant 1.6, JDK 1.4 (or higher) and a
[build.xml](https://ant.apache.org/ivy/history/latest-milestone/samples/build.xml)
file. (Try it! Just download that file and type 'ant'.)

So, I'll be looking to add Ivy support to mondrian in the next week or
so. The big benefit will be smaller distributions. If you download a
source distribution, it will no longer contain libraries such as
olap4j.jar, javacup.jar, commons-pool.jar, and so forth. The build
process will download these libraries the first time you build. It
takes quite a lot of effort, each release, to make sure that a source
distribution contains all dependencies, so we hope to same some time
there. We'll be able to delete these libraries from our source control
system -- always a strange place for libraries, I thought.

And, for those of you who use mondrian with different libraries than
we ship with (say you use a different version of log4j or apache
commons than we do) you should be able to easily modify your
dependencies and recompile the source distribution.
