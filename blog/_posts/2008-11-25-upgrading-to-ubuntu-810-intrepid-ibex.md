---
layout: post
title: Upgrading to ubuntu 8.10 Intrepid Ibex
date: '2008-11-25T11:41:00.000-08:00'
author: Julian Hyde
tags:
- ubuntu intrepid ibex fennel eigenbase
modified_time: '2008-11-25T12:02:08.119-08:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-2862978729703113039
blogger_orig_url: https://julianhyde.blogspot.com/2008/11/upgrading-to-ubuntu-810-intrepid-ibex.html
---

Someone had to take the plunge. I upgraded one of my development environments, my laptop (a Dell D630 which dual-boots to Vista) to <a href="http://www.ubuntu.com/">Ubuntu 8.10</a> last night just to kick the tires.<br /><br />Since 8.04 was a LTS (long-term support) release, 8.04 users will not get automatically upgraded. You have to explicitly ask for it, as described <a href="http://www.ubuntu.com/getubuntu/upgrading">on the Ubuntu site</a>.<br /><br />(If you are surprised that 8.10 is the successor to 8.04, you need to know that Ubuntu releases occur approximately six months apart and are numbered {year}.{month}. So 8.10 was released in October 2008. As for where the {adjective} {animal} release names come from, I have never been bold enough to ask.)<br /><br />The upgrade went smoothly. Everything worked right out of the box. Kudos to the Ubuntu folks, yet again: I have been able to do distribution upgrades since Ubuntu 7.06.<br /><br />As for features, I haven't noticed anything different. I like to stay up to date, and for now I'm pleased that everything still works and looks the same. I'm sure I'll come across the good stuff in due course.<br /><br />Not so great for <a href="http://www.eigenbase.org">Eigenbase</a> developers, though. Fennel has problems building, which is not unsurprising considering its dependencies on C++ libraries and build tools. Ubuntu 8.10 installs libtool-2.2 (8.04 was libtool-1.5.26). I got some syntax errors in what looked like a generated bash script, possibly related to libtool.<br /><br />I'm not going to attempt to track down and solve the problems here. I will do that on the <a href="http://n2.nabble.com/fennel-developers-f1374754.html">fennel-dev</a> list, in the next few weeks. Eigenbase developers should note that 8.10 is not yet a viable development environment; for everyone else, it's just fine.