---
layout: post
title: Roll your own high-performance Java collections classes
date: '2011-06-03T12:58:00.000-07:00'
author: Julian Hyde
tags:
- efficient primitive java collections janino
modified_time: '2011-06-04T00:07:02.438-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-8339921548425500424
blogger_orig_url: https://julianhyde.blogspot.com/2011/06/roll-your-own-high-performance-java.html
---

The Java collections framework is great. You can create maps, sets,
lists with various element types, various performance characteristics
(e.g. if you want O(1) insert, use a linked list), iterate over them,
and you can decorate them to give them other behaviors.

But suppose that you want to create a high-performance, memory
efficient immutable list of integers? You'd write

{% highlight java %}
List<Integer> list =
    Collections.unmodifiableList(
        new ArrayList<Integer>(Arrays.asList(1000, 1001, 1002)));
{% endhighlight %}

There will be 6 objects allocated in the JVM: three `Integer` objects,
an array `Object[3]` to hold the `Integer` values, an `ArrayList`, and
an `UnmodifiableRandomAccessList`. Not to mention the
`Arrays.ArrayList` and `Integer[3]` used to construct the list and
quickly thrown away.

The resulting list is no longer high-performance. A call to say `int n
= list.get(2)` requires 3 method calls
(`UnmodifiableRandomAccessList.get`, `ArrayList.get`,
`Integer.intValue`) and 3 indirections. And the sheer number of
objects created reduces the chance that a given stretch of code will
be able to operate solely from the contents of L1 cache.

So, what next? Should I write my own class, like this?

{% highlight java %}
public class UnmodifiableNativeIntArrayList
    implements List<Integer> {
  ...
}
{% endhighlight %}

Well, maybe. But there are rather a lot of variations to cover, and
each one needs to be hand-coded and tested.

Do I use library code? I searched and turned up
[Apache Commons Primitives](https://commons.apache.org/primitives/),
[Primitive Collections for Java (PCJ)](https://pcj.sourceforge.net/),
and [GNU Trove (trove4j)](http://trove.starlight-systems.com/).
Of these, only GNU Trove is still active.

None of the libraries supports features such as maps with two or more
keys, unmodifiable collections, synchronized collections, flat
collections (similar to [Apache `Flat3Map`](https://commons.apache.org/collections/api-3.2/org/apache/commons/collections/map/Flat3Map.html)).
It's not surprising that they don't: each combination
of features would require its own class, so the size of the jar file
would grow exponentially.

So, I'd like to propose an alternate approach. You configure a
factory, specifying the precise kind of collection you would like, and
the factory generates the collection class in bytecode. You can use
the factory to quickly create as many instances of the collection as
you wish. The collection implements the Java collections interfaces,
plus additional interfaces that allow you to efficiently access the
collection without boxing/unboxing.

The above example would be written as follows:

{% highlight java %}
// Initialize the factory when the program is loaded.
// Then the bytecode gets generated just once.
static final Factory factory =
  new FactoryBuilder()
    .list()
    .elementType(Integer.TYPE)
    .modifiable(false)
    .factory();

int[] ints = {1000, 1001, 1002};
IntList list = factory.createIntList(ints);
{% endhighlight %}

Variants are expressed as `FactoryBuilder` methods:

* `FactoryBuilder FactoryBuilder.list()`
* `FactoryBuilder FactoryBuilder.map()`
* `FactoryBuilder FactoryBuilder.set()`
* `FactoryBuilder FactoryBuilder.keyType(Class...)` (for maps only)
* `FactoryBuilder FactoryBuilder.valueType(Class...)` (for maps only)
* `FactoryBuilder FactoryBuilder.elementType(Class...)` (for list and set only)
* `FactoryBuilder FactoryBuilder.sorted(boolean)`
  (cf. the difference between `Set` and `SortedSet`)
* `FactoryBuilder FactoryBuilder.deterministic(boolean)`
  (cf. the difference between `HashMap` and `LinkedHashMap`)
* `FactoryBuilder FactoryBuilder.modifiable(boolean)`
* `FactoryBuilder FactoryBuilder.fixedSize(boolean)`
  (cf. the difference between `Flat3Map` and `Map`)
* `FactoryBuilder FactoryBuilder.synchronized(boolean)`

And so forth. Additional variants could be added as the project
evolved. Templates could be fine-tuned for particular combinations of
variants.

The projects I mentioned above clearly use a template system, and we
could use and extend those templates. The janino facility can easily
convert the generated java code into bytecode. And the JVM would be
able to apply JIT (just-in-time compilation) to these classes; in
fact, these classes would be more amenable to compilation, because
they would be compact and final.

The existing projects have invested a lot of effort designing
high-performance collections. I'd like to build on that work; this
project could even be an extension to those projects.

I'd like to hear if you're interested in working with me on this.
