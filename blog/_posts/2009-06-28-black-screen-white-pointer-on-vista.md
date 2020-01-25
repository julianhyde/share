---
layout: post
title: Black screen, white pointer on Vista
date: '2009-06-28T13:24:00.000-07:00'
author: Julian Hyde
tags:
- vista fail
modified_time: '2009-06-28T13:40:18.394-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-3633192392818034961
blogger_orig_url: https://julianhyde.blogspot.com/2009/06/black-screen-white-pointer-on-vista.html
---

Last night I had a problem where Vista gives me a black screen and white pointer. You can move the pointer around, but you can't do anything useful (except, as noted below, press the Shift key 5 times). I tried restarting in safe mode, and I got a black screen with 'Safe mode' in each corner of the screen, but otherwise the same experience.<br /><br />I had a huge sinking feeling. I've had this problem twice before in the last twelve months. On the other two occasions, Dell technical support asked the usual questions for an hour or so, then told me to re-install Vista. An operating system that needs to be re-installed every 6 months is not a productive operating system, even if the operating system is great in between times. Which Vista isn't, anyway.<br /><br />Luckily, this time I found <a href="http://social.technet.microsoft.com/forums/en-US/itprovistadesktopui/thread/193b7008-ce4b-4d03-acc3-b8d7ffe610d5/">this post at Microsoft TechNet</a>. The problem was exactly as described in the post, and so was the cause (corrupted windows event log files) and the solution (rename the directory, or delete the event log files).<br /><br />I'm pleased to say I discovered the same hack that they did: press the shift key five times, which gives you the 'Do you want to turn on Sticky Keys?' dialog. (Yes, this is literally the ONLY meaningful interaction you can have with what is obviously an instance of Vista which is functioning but just not listening.) Then you click on the 'Go to the Ease of Access Center do disable the keyboard shortcut', and because this brings up an explorer window, you can then type in the address bar to launch all kinds of other commands.<br /><br />Thanks to <a href="http://social.technet.microsoft.com/Profile/en-US/?user=towz">towz</a> and others who posted to that forum; proving that even for Microsoft products, the crowd can sometimes provide better technical support than the professionals.