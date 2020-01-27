---
layout: post
title: An experiment with the Linux scheduler
date: '2010-12-19T21:26:00.000-08:00'
author: Julian Hyde
tags:
- linux thread scheduler
modified_time: '2010-12-19T21:26:17.786-08:00'
thumbnail: https://4.bp.blogspot.com/_BVv0WTpeWTs/TQ7ngj1xKVI/AAAAAAAAAEw/dphV-tXC2hM/s72-c/chart
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-1037821703076693964
blogger_orig_url: https://julianhyde.blogspot.com/2010/12/experiment-with-linux-scheduler.html
---

I was curious to see how the Linux scheduler would manifest from a
program's perspective, so today I did an experiment.

I wrote a single-threaded program running a simple loop. All the loop
does is to compute the number of milliseconds since the last
iteration, and store the result in a histogram. We are not so much
interested in the performance of the loop (it does about a million
iterations per second) but in the variations in the intervals between
loop iterations. These variations are presumably caused by the Linux
scheduler.

Here are the numbers I achieved, and the same numbers in a chart (with
a logarithmic y-axis).

<a href="/assets/img/chart.png" imageanchor="1"
    style="clear: left; float: left; margin-bottom: 1em; margin-right: 1em;">
  <img border="0" height="172" src="/assets/img/chart.png" width="320" />
</a>

| Interval (milliseconds) | Frequency |
| ----------------------: | --------: |
| 0   |  450,080,302
| 1   |  909,044
| 2   |  4,642
| 3   |  1,696
| 4   |  853
| 5   |  561
| 6   |  557
| 7   |  335
| 8   |  1,098
| 9   |  152
| 10  |  86
| 11  |  52
| 12  |  98
| 13  |  17
| 14  |  13
| 15  |  6
| 16  |  21
| 17  |  5
| 19  |  3
| 20  |  1
| 21  |  2
| 22  |  2
| 23  |  0
| 24  |  2
| 25  |  0
| 26  |  0
| 27  |  0
| 28  |  1
| 29  |  0

The vast majority of iterations occur zero milliseconds after the
previous iteration. No surprise there; Java's clock granularity, 1
millisecond, is coarse enough to execute over a million instructions.

If the thread was never interrupted, one would expect the loop to tick
forward 1 millisecond 1,000 times per second, or about 500,000 times
in all. It actually ticks 909,044 times, so interrupts are at work:
about 400,000 of them.

Longer delays also occur: 2, 3, 4, up to 28 milliseconds, occurring
with exponentially decreasing frequency. Only 8 delays of 20
milliseconds or longer occur in the 7.5 minute run. The chart shows
the exponential decay clearly. The chart plots log-frequency, and the
trend line is indeed flat from 2 milliseconds onwards, so it is
accurate to characterize the line as exponential.

The one surprising thing: significant bumps at 8, 12 and 16
milliseconds. Although the trend of the line is pretty consistently
down, each of those interval durations has more distinctly occurrences
than the previous interval. Does anyone know anything about the Linux
scheduler that might explain this?
