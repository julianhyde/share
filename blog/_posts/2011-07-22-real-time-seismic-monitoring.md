---
layout: post
title: Real-Time Seismic Monitoring
date: '2011-07-22T10:51:00.000-07:00'
author: Julian Hyde
tags:
- sqlstream signal processing rabbitmq amqp seismic
modified_time: '2011-07-22T10:51:08.674-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-8770738448624096027
blogger_orig_url: https://julianhyde.blogspot.com/2011/07/real-time-seismic-monitoring.html
---

Marc Berkowitz wrote a
[blog post describing an application of SQLstream to power a seismic
monitoring project](https://www.sqlstream.com/blog/2011/07/real-time-seismic-monitoring-in-the-cloud-with-sqlstream/)
that is a collaboration between several leading research institutions.

The project is interesting in several respects:

* The project involves signal processing. Unlike the
  "event-processing" application that we see most often at SQLstream,
  events arrive at a regular rate (generally 40 readings every second,
  per sensor). In signal processing, events are more likely to be
  processed using complex mathematical formulas (such as
  [Fourier transforms](https://en.wikipedia.org/wiki/Fourier_transform))
  than by boolean logic (event A happened, then event
  B happened). Using SQLstream's user-defined function framework, we
  were easily able to accommodate this form of processing.
* It illustrates how a stream-computing "fabric" can be created,
  connecting multiple SQLstream processing nodes using
  [RabbitMQ](https://www.rabbitmq.com/).
* One of the reasons for building a distributed system was to allow an
  agile approach. Researchers can easily deploy new algorithms without
  affecting the performance or correctness of other algorithms running
  in the cloud.
* Another goal of the distributed system was performance and
  scalability. Nodes can easily be added to accommodate greater
  numbers of sensors. The system is not
  [embarassingly parallel](https://en.wikipedia.org/wiki/Embarrassingly_parallel),
  but we were still able to parallelize the solution effectively.
* Lastly, the system needs to be both continuous
  and real-time. "Continuous" meaning that data is processed as it
  arrives; a smoother, more predictable and more efficient mode of
  operation than ETL. "Real-time" because some of the potential
  outputs of the system, such as tsunami alerts, need to be delivered
  as soon as possible in order to be useful.

In all, a very interesting case study of what SQLstream is capable
of. Marc plans to make follow-up posts describing the solution in more
detail, so [stay tuned](https://www.sqlstream.com/blog/).
