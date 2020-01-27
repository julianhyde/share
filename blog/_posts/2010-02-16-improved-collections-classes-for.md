---
layout: post
title: Improved collections classes for Mondrian's query execution process
date: '2010-02-16T18:24:00.000-08:00'
author: Julian Hyde
tags:
modified_time: '2010-02-16T18:34:09.387-08:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-5056075833326780784
blogger_orig_url: https://julianhyde.blogspot.com/2010/02/improved-collections-classes-for.html
---

Mondrian calculations work predominantly over lists of members and
tuples. Internally, Mondrian represents lists of members as
`List<Member>`, and represents lists of tuples as
`List<Member[]>`. (And similarly for iterators over members and
tuples.)

There are two problems with this. First, the representation of tuple
lists requires an array to be allocated for each element of the
list. Allocations cost time and memory. (Granted, we could allocate
temporary arrays only when tuples are accessed, which would cost only
time. But according to my latest round of profiling, the effort of
allocating lots small arrays is significant.)

Second, the code to deal with members and tuples has to be
different. The most extreme example of this found in the
implementation of CrossJoin. There are over 30 inner classes in
`class CrossJoinFunDef`, to deal with the permutations of iterator
vs. list, mutable vs. immutable, and tuple vs. member.

In short, the java standard `List` and `Iterator` classes are not serving
us well. I think it's appropriate to introduce classes/interfaces that
handle members and tuples more uniformly, and can store, access, and
iterate over collections without lots of small arrays being created.

Here are some collection classes that I think would serve the purpose:

{% highlight java %}
interface TupleList {
  int size();
  int arity();
  Member getMember(int index, int ordinal);
  TupleIterator tupleIterator();
}

interface TupleIterator {
  int arity();
  boolean hasNext();
  // writes the members of the next tuple into given array
  void next(Member[] members);
  // appends members of the next tuple to given list
  void next(List<Member> members);
}
{% endhighlight %}

If *arity* = 1 (i.e. if the list is just a collection of members) then
`TupleList` could easily be implemented using `java.util.ArrayList`.

For other arities, a list of tuples could be represented as a set of
members end-to-end. For instance, the list with two 3-tuple elements
{(A1, B1, C1), (A2, B2, C2)} would be held in a list {A1, B1, C1, A2,
B2, C2} and getMember(index, ordinal) would read element *index *
arity + ordinal* of the list.

Introducing these would require quite a few code changes, mostly in
the mondrian.olap.fun package, which is where the builtin functions
are implemented. There should be no changes to the user API or olap4j.

I am still debating whether this change makes sense. Usually this kind
of penny-pinching architectural change doesn't pay off. But some of
them pay off big. I've learned in Oracle, Broadbase, and SQLstream
that for high-performance data processing you shouldn't be doing any
memory allocations in an inner loop that is executed once per
row. That isn't quite practical in Java, but it's a goal to strive
for. In today's CPU architectures, where memory is slow and
last-level-cache is fast, it pays to keep data contiguous.

If you are a Mondrian developer, I'd be interested to hear what you
think about this proposed change.
