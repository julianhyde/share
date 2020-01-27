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

[olap4j](http://www.olap4j.org/) version 0.9 (beta) is
almost ready for release. The specification has been expanded and
clarified considerably
([latest spec](https://olap4j.svn.sourceforge.net/viewvc/*checkout*/olap4j/trunk/doc/olap4j_fs.html)).
There is now a beta-quality [mondrian](https://mondrian.pentaho.org/) driver for olap4j,
and a comprehensive set of tests in the TCK (technology compatibility
kit). It will be released before the end of the month.

But before we release 0.9, I need to solve a dilemma regarding how
olap4j should handle scrolling result sets.

We have already established that olap4j will allow clients to access
the positions on an axis both via a random-access list and via a
bi-directional iterator. Hence [CellSetAxis](http://www.olap4j.org/api/org/olap4j/CellSetAxis.html)
implements `Iterable<Position>` and has methods

* `List<Position> getPositions()`
* `ListIterator<Position> iterator()`

With these two methods, I have no doubt that client applications will
find it easy to navigate a `CellSet` with concise, clear code. The
issue is more whether the writer of an olap4j driver can write an
efficient implementation. To do that, the client needs a clear way to
signal their intended access pattern to the driver, or conversely, the
driver needs to infer the client's access pattern from the methods it
calls.

Now, one might say, let's use a smart list which does paging and
generally behaves like an iterator behind the scenes. The problem is
that the driver can't infer the client's intended access pattern. The
smart list would not know whether to page out previous positions (and
have to ask the server for them again, at considerable cost to the
server and communication cost) or to try to keep them in memory. So, I
reject the smart list approach. Let's stick with the dual list and
iterator, and see what the driver could infer from the client.

Consider these cases:

*1. Small number of columns and rows*. No memory or performance issues
here. Client may prefer the convenience of the list, but the iterator
will work fine too.

*2. Small number of columns, large number of rows*. Client will
probably want to use a list for columns, so that they can scan the
list repeatedly, but use an iterator for rows, to signal that earlier
rows can be released from memory.

*3. Large number of columns and rows*. Client will probably want
to use an iterator for both columns and rows. Certainly the number of
cells will be so large that they will have to be paged, probably in
blocks. The driver can infer the access pattern from the iterators and
be reasonably intelligent how to manage cell values.

*4. Unknown number of columns and rows*. Not as common as you would
think, because if the client is going to receive many thousands of
rows, the application writer generally knows that this is
characteristic of their application (say managing mailing lists) and
chooses an appropriate client. I suppose the client could test the
water using an iterator to find the size of the axis before switching
to a list access method; or the driver writer could implement a smart
list which reads large blocks of positions (say 1,000) at a time but
for small-to-moderate sized cell sets behaves essentially the same as
a dumb list.

### Problem #1: Cell ordinals

In case #3, it is not possible to access cells via an integer cell
ordinal computed using the formula *cellOrdinal == columnOrdinal +
columnCount * rowOrdinal*, because *columnCount* is not known until
the entire columns axis has been evaluated.

I propose that you can open a cell set in two modes,
random-access-mode and cursor-mode. Cursor-mode is for very large
result sets. In cursor-mode, the driver would not attempt to find the
length of the columns axis, and therefore you could not call
`Cell.getOrdinal` or `CellSet.getCell(int ordinal)` or
`Cell.getProperty(StandardCellProperty.CELL_ORDINAL)`: you would get a
runtime error if you called any of these methods. You could still call
`CellSet.getCell(List<integer> coords)`,
`CellSet.getCell(Position... positions)`, `List<integer>
Cell.getCoordinates()`, and `Cell.getProperty(Property)` for any other
Property than `CELL_ORDINAL`.

Random-access mode is for regular cell sets. If you attempted to call
one of the above methods, it would find the length of the columns
axis, scanning to the end if necessary.

### Problem #2: Backwards iteration

If you're writing an OLAP client application, it's really nice that
the `ListIterator` can go backwards. You can even go back to the start
and iterate over the list, as many times as you like. But if you're
writing an olap4j driver which is intended to be network and memory
efficient, backwards iterators are sheer hell. You have to buffer
values just in case the client wants to go back.

So, we need a way for the client to declare (or the driver to infer)
that the client will not go backwards. My preferred option would be to
change the `CellSetAxis` method `ListIterator<Position> iterate()` to
`Iterator<Position> iterate()`, and throw a runtime error if this
method is called more than once.

Another option would be to open the `CellSet` with an option where the
client promises not to drive iterators backwards; but this would not
support case #2, where you might want to restart the iterator over
columns but not the iterator over rows.

Let me know your thoughts at the
[olap4j Open Discussion forum](https://sourceforge.net/forum/message.php?msg_id=4630374).
