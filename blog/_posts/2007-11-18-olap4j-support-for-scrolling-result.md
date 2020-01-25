---
layout: post
title: olap4j support for scrolling result sets
date: '2007-11-18T18:13:00.000-08:00'
author: Julian Hyde
tags: 
modified_time: '2007-11-18T18:35:24.627-08:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-4290443096554852098
blogger_orig_url: https://julianhyde.blogspot.com/2007/11/olap4j-support-for-scrolling-result.html
---

<a href="http://www.olap4j.org/">olap4j</a> version 0.9 (beta) is almost ready for release. The specification has been expanded and clarified considerably (<a href="http://olap4j.svn.sourceforge.net/viewvc/*checkout*/olap4j/trunk/doc/olap4j_fs.html">latest spec</a>). There is now a beta-quality <a href="http://mondrian.pentaho.org/">mondrian</a> driver for olap4j, and a comprehensive set of tests in the TCK (technology compatibility kit). It will be released before the end of the month.<br /><br />But before we release 0.9, I need to solve a dilemma regarding how olap4j should handle scrolling result sets.<br /><br />We have already established that olap4j will allow clients to access the positions on an axis both via a random-access list and via a bi-directional iterator. Hence <a href="http://www.olap4j.org/api/org/olap4j/CellSetAxis.html">CellSetAxis</a> implements <span style="font-family:courier new;">Iterable&lt;Position&gt;</span> and has methods<br /><ul><li><span style="font-family:courier new;">List&lt;Position&gt; getPositions()</position></span></li><li><span style="font-family:courier new;">ListIterator&lt;Position&gt; iterator()</position></span></li></ul>With these two methods, I have no doubt that client applications will find it easy to navigate a <span style="font-family:courier new;">CellSet</span> with concise, clear code. The issue is more whether the writer of an olap4j driver can write an efficient implementation. To do that, the client needs a clear way to signal their intended access pattern to the driver, or conversely, the driver needs to infer the client's access pattern from the methods it calls.<br /><br />Now, one might say, let's use a smart list which does paging and generally behaves like an iterator behind the scenes. The problem is that the driver can't infer the client's intended access pattern. The smart list would not know whether to page out previous positions (and have to ask the server for them again, at considerable cost to the server and communication cost) or to try to keep them in memory. So, I reject the smart list approach. Let's stick with the dual list and iterator, and see what the driver could infer from the client.<br /><br />Consider these cases:<br /><br /><span style="font-style: italic;">1. Small number of columns and rows</span>. No memory or performance issues here. Client may prefer the convenience of the list, but the iterator will work fine too.<br /><br /><span style="font-style: italic;">2. Small number of columns, large number of rows</span>. Client will probably want to use a list for columns, so that they can scan the list repeatedly, but use an iterator for rows, to signal that earlier rows can be released from memory.<br /><br /><span style="font-style: italic;">3. Large number of columns and rows</span>. Client will probably want to use an iterator for both columns and rows. Certainly the number of cells will be so large that they will have to be paged, probably in blocks. The driver can infer the access pattern from the iterators and be reasonably intelligent how to manage cell values.<br /><br /><span style="font-style: italic;">4. Unknown number of columns and rows</span>. Not as common as you would think, because if the client is going to receive many thousands of rows, the application writer generally knows that this is characteristic of their application (say managing mailing lists) and chooses an appropriate client. I suppose the client could test the water using an iterator to find the size of the axis before switching to a list access method; or the driver writer could implement a smart list which reads large blocks of positions (say 1,000) at a time but for small-to-moderate sized cell sets behaves essentially the same as a dumb list.<br /><br /><span style="font-size:130%;"><span style="font-weight: bold;">Problem #1: Cell ordinals</span></span><br /><br />In case #3, it is not possible to access cells via an integer cell ordinal computed using the formula <span style="font-style: italic;">cellOrdinal == columnOrdinal + columnCount * rowOrdinal</span>, because <span style="font-style: italic;">columnCount</span> is not known until the entire columns axis has been evaluated.<br /><br />I propose that you can open a cell set in two modes, random-access-mode and cursor-mode. Cursor-mode is for very large result sets. In cursor-mode, the driver would not attempt to find the length of the columns axis, and therefore you could not call <span style="font-family:courier new;">Cell.getOrdinal()</span> or <span style="font-family:courier new;">CellSet.getCell(int ordinal)</span> or <span style="font-family:courier new;">Cell.getProperty(StandardCellProperty.CELL_ORDINAL)</span>: you would get a runtime error if you called any of these methods. You could still call <span style="font-family:courier new;">CellSet.getCell(List<integer> coords)</integer></span>, <span style="font-family:courier new;">CellSet.getCell(Position... positions)</span>, <span style="font-family:courier new;">List<integer> Cell.getCoordinates()</integer></span>, and <span style="font-family:courier new;">Cell.getProperty(Property)</span> for any other Property than <span style="font-family:courier new;">CELL_ORDINAL</span>.<br /><br />Random-access mode is for regular cell sets. If you attempted to call one of the above methods, it would find the length of the columns axis, scanning to the end if necessary.<br /><br /><span style="font-size:130%;"><span style="font-weight: bold;">Problem #2: Backwards iteration</span></span><br /><br />If you're writing an OLAP client application, it's really nice that the <span style="font-family:courier new;">ListIterator</span> can go backwards. You can even go back to the start and iterate over the list, as many times as you like. But if you're writing an olap4j driver which is intended to be network and memory efficient, backwards iterators are sheer hell. You have to buffer values just in case the client wants to go back.<br /><br />So, we need a way for the client to declare (or the driver to infer) that the client will not go backwards. My preferred option would be to change the <span style="font-family:courier new;">CellSetAxis</span> method <span style="font-family:courier new;">ListIterator&lt;Position&gt; iterate()</position></span> to <span style="font-family:courier new;">Iterator&lt;Position&gt; iterate()</span>, and throw a runtime error if this method is called more than once.<br /><br />Another option would be to open the <span style="font-family:courier new;">CellSet</span> with an option where the client promises not to drive iterators backwards; but this would not support case #2, where you might want to restart the iterator over columns but not the iterator over rows.<br /><br />Let me know your thoughts at the <a href="http://sourceforge.net/forum/message.php?msg_id=4630374">olap4j Open Discussion forum</a>.