---
layout: post
title: Your point, exactly? My point. Exactly.
date: '2006-08-17T18:29:00.000-07:00'
author: Julian Hyde
tags:
modified_time: '2006-08-17T18:33:56.125-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-3247597408878276310
blogger_orig_url: https://julianhyde.blogspot.com/2006/08/your-point-exactly-my-point-exactly.html
---

A few years ago I started an open source project called
[mondrian](https://mondrian.sourceforge.net). I had been building some
business intelligence applications, and had taken a liking to OLAP. I
was hoping to see OLAP applications popping up here and there, on
websites, in desktop applications like Quicken, and it just wasn't
happening.

I soon figured out why. OLAP was cool, but it was complicated and the
software was expensive. These two facts are not unrelated. If you're a
software vendor who has just developed a cool new technology, you're
going to want to charge a lot of money for it. But your customer, who
is paying a lot of money for it, wants to see a lot more in the box
than a CD and some packing peanuts. So, the software tends to get
expensive, to match the customer's expectations.

Now, don't get me wrong. Business Intelligence is a complicated
process. It involves getting a business person to talk to a computer
person, which is something neither of them enjoys very much. In fact
there are probably several business people, and systems
administrators, and lawyers, and consultants to get them all to talk
to each other, and lots and lots of data.

But does the software have to be complicated too? Not really, I thought.

So, mainly to prove a point, I wrote the mondrian OLAP server.

And things worked out fine. Today a lot of people use mondrian, and
mondrian does a lot of things. In the early days, it was a combination
of engineering pragmatism, dumb luck, and dogged persistence.

The engineering pragmatism was that since I was one guy, working code
at the weekend, this thing had better be really simple. Java and
XML. Could Java possibly be fast enough? Well, it had better be, I
thought. Someone's gone to the trouble of writing a DBMS already, and
it's great at storing data and crunching numbers, so let's make the
database do as much of the work as possible.

The dumb luck was that on the other side of the world, almost at
exactly the same time I brought out the first prototype, Andreas Voss
was thinking of writing an OLAP client which would fit perfectly with
the server I was writing. That client was
[jpivot](https://jpivot.sourceforge.net), of course. An OLAP server
isn't much use without a client to do *ad hoc* queries.

The dogged persistence was not so much me, writing the code, as the
initial set of users, using mondrian and jpivot for heavens knows
what, but using it anyway, and fixing bugs, and just getting involved.

By the way, I'm still not sure exactly what mondrian users do with
mondrian. It's one of the peculiar side-effects of open
source. Because no money changes hands, because there's no purchase
order or legal agreement, the relationship with the users is
fleeting. You bump into some of them in bars at conferences, and of
course you hear some war stories, but typically you see a few French
table names in a bug report, and think "Cool. Someone's using Mondrian
to read electricity meters. Who'd have thought?"

But the main thing is, mondrian just gets better and better. And my
job as the lead developer is to stop it from getting too complicated,
while it gets better.  I'll have more to say about that in the next
few posts.
