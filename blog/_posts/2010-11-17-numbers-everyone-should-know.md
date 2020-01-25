---
layout: post
title: Numbers everyone should know
date: '2010-11-17T15:04:00.000-08:00'
author: Julian Hyde
tags:
- system design
modified_time: '2010-11-17T15:04:17.508-08:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-3969700668946149667
blogger_orig_url: https://julianhyde.blogspot.com/2010/11/numbers-everyone-should-know.html
---

Jeffrey Dean recently gave a talk "Building Software Systems at Google and Lessons Learned" at Stanford (<a href="http://goo.gl/0MznW">video</a>). One of his slides was the following list of numbers:<br /><br /><table><tbody><tr><td>L1 cache reference</td><td align="right">0.5</td><td>ns</td></tr><tr><td>Branch mispredict                    </td><td align="right">5 ns</td></tr><tr><td>L2 cache reference                   </td><td align="right">7 ns</td></tr><tr><td>Mutex lock/unlock                    </td><td align="right">25 ns</td></tr><tr><td>Main memory reference                </td><td align="right">100 ns</td></tr><tr><td>Compress 1K bytes w/ cheap algorithm </td><td align="right">3,000 ns</td></tr><tr><td>Send 2K bytes over 1 Gbps network    </td><td align="right">20,000 ns</td></tr><tr><td>Read 1 MB sequentially from memory   </td><td align="right">250,000 ns</td></tr><tr><td>Round trip within same datacenter    </td><td align="right">500,000 ns</td></tr><tr><td>Disk seek                           </td><td align="right">10,000,000 ns</td></tr><tr><td>Read 1 MB sequentially from disk    </td><td align="right">20,000,000 ns</td></tr><tr><td>Send packet CA-&gt;Netherlands-&gt;CA</td><td align="right">150,000,000 ns</td></tr></tbody></table><br />Everyone who wants to design high-performance, scalable systems should memorize these numbers. There are many, many lessons to be learned.