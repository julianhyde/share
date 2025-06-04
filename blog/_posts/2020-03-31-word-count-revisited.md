---
layout: post
title:  "Word Count revisited"
date:   2020-03-31 12:00:00 -0800
author: Julian Hyde
image:  /assets/img/OldDesignShop_MushroomSpringMorel-240x240.jpg
tweet:  https://twitter.com/julianhyde/status/1245158958366482432
---

WordCount is a problem that has been used to showcase several
generations of data engines. It was introduced in
[MapReduce](https://research.google.com/archive/mapreduce-osdi04.pdf),
and followed by many others, including
[Pig](http://infolab.stanford.edu/~olston/publications/sigmod08.pdf),
[Hive](http://infolab.stanford.edu/~ragho/hive-icde2010.pdf) and
[Spark](https://www.usenix.org/system/files/conference/nsdi12/nsdi12-final138.pdf).

The problem is simple to state: Given a collection of documents, find
the set of words that occur in those documents, and the number of
occurrences of each.

The solution is not so straightforward. It requires functions on scalar
values (to tokenize a string into words), handling nested collections
(because one line or document becomes a set of words), data
parallelism (in case there are millions of documents and thousands of
words), and reading from and writing to files.
[Mike Stonebraker's protestations](https://cacm.acm.org/magazines/2010/1/55743-mapreduce-and-parallel-dbmss-friends-or-foes/fulltext)
notwithstanding, these are not things that an RDBMS does well.

The deficiencies of RDBMS were the impetus for new data processing
languages and frameworks, starting with MepReduce in 2004.

In this post, we shall look at implementations of WordCount in various
languages and engines.  Each implementation blends general-purpose
programming languages, functional programming, and relational algebra
in varying proportions.

# WordCount in MapReduce

```python
map(String input_key, String input_value):
  // input_key: document name
  // input_value: document contents
  for each word w in input_value:
    EmitIntermediate(w, "1");

reduce(String output_key, Iterator intermediate_values):
  // output_key: a word
  // output_values: a list of counts
  int result = 0;
  for each v in intermediate_values:
    result += ParseInt(v);
  Emit(AsString(result));
```

(From [MapReduce: Simplified Data Processing on Large Clusters, by Jeff Dean and Sanjay Ghemawat, 2004](http://research.google.com/archive/mapreduce-osdi04-slides/index-auto-0004.html).)

MapReduce was ground-breaking because it framed data parallelism in
functional programming terms. It demonstrated that a large, complex,
distributed problem could be expressed in terms of two simple
functions.

Functional programming is often thought of as good for solving only
'small' problems, but because functions are pure and stateless they
are an excellent building block for large-scale distributed programs.

Also, you can implement the functions in a powerful general-purpose
programming language, so you can solve the whole problem in one
language. In most dialects of SQL you cannot solve WordCount because
there is no built-in `split` function to split a document into
words. You would have to jump into another language to implement
`split` as a user-defined function, and then import that function into
your SQL session.

My only quibble is that they use confusing names. To functional
programmers, `map` and `reduce` are well-known higher-order functions
that are built into the system; Dean and Ghemawat's `map` and `reduce`
functions are just the arguments to those.

In the following example in
[Standard ML](https://en.wikipedia.org/wiki/Standard_ML), a
functional programming language, you'll see that I rename their
`map` and `reduce` functions to `wc_mapper` and `wc_reducer`, and pass
them as arguments to a higher-order function called `mapReduce`. It
illustrates the connection between MapReduce and functional
programming.  The only difference is that in Dean and Ghemawat's
MapReduce, and in other implementations of MapReduce such as
[Apache Hadoop](https://hadoop.apache.org), the `mapReduce` function
is a powerful distributed system and not 17 lines of Standard ML.

# Word Count in Standard ML

First, let's define the `mapReduce` function. It is a higher-order
function that takes two other functions `mapper` and `reducer` as
arguments, and also the list of input values.

`mapReduce` is a framework. The user can make it do WordCount or a
hundred other tasks by providing different implementations of `mapper`
and `reducer`.

This particular implementation is very inefficient -- the `update` and
`dedup` functions that build a multimap have O(n<sup>2</sup>) running
time, and the program runs in a single thread -- but the point is that
the framework could be made more efficient without the user having to
rewrite their `mapper` and `reducer` functions.

```sml
- fun mapReduce mapper reducer list =
    let
      fun update (key, value, []) = [(key, [value])]
        | update (key, value, ((key2, values) :: tail)) =
            if key = key2 then
              (key, (value :: values)) :: tail
            else
              (key2, values) :: (update (key, value, tail))
      fun dedup ([], dict) = dict
        | dedup ((key, value) :: tail, dict) =
            dedup (tail, update (key, value, dict))
      fun flatMap f list = List.foldl (op @) [] (List.map f list)
      val keyValueList = flatMap mapper list
      val keyValuesList = dedup (keyValueList, [])
    in
      List.map (fn (key, values) => (key, reducer (key, values))) keyValuesList
    end;
val mapReduce = fn
  : ('a -> (''b * 'c) list)
    -> (''b * 'c list -> 'd) -> 'a list -> (''b * 'd) list
```

Now let's define the `wc_mapper` and `wc_reducer` functions that will
power the WordCount algorithm.

```sml
- fun wc_mapper line =
    let
      fun split0 [] word words = word :: words
        | split0 (#" " :: s) word words = split0 s "" (word :: words)
        | split0 (c :: s) word words = split0 s (word ^ (String.str c)) words
      fun split s = List.rev (split0 (String.explode s) "" [])
    in
      List.map (fn w => (w, 1)) (split line)
    end;
val wc_mapper = fn : string -> (string * int) list
- fun wc_reducer (key, values) = foldl (op +) 0 values;
val wc_reducer = fn : 'a * int list -> int
```

Check that they work on discrete values:
```sml
- wc_mapper "a skunk sat on a stump";
val it = [("a",1),("skunk",1),("sat",1),("on",1),("a",1),("stump",1)]
  : (string * int) list
- wc_reducer ("hello", [1, 4, 2]);
val it = 7 : int
```

Bind them to `mapReduce` to create a function tailored to the
WordCount problem:

```sml
- fun wordCount lines = mapReduce wc_mapper wc_reducer lines;
val wordCount = fn : string list -> (string * int) list
```

And check that our `wordCount` function works:

```sml
- wordCount ["a skunk sat on a stump",
    "and thunk the stump stunk",
    "but the stump thunk the skunk stunk"];
val it =
  [("but",1),("the",3),("stump",3),("thunk",2),("skunk",2),("stunk",2),
   ("and",1),("a",2),("sat",1),("on",1)] : (string * int) list
```

# WordCount in Pig

[Apache Pig](https://pig.apache.org) was one of the first high-level
languages for Apache Hadoop. Pig has its trotters firmly planted in
relational algebra, but makes extensive use of nested
collections.

```
input = load 'mary' as (line);
words = foreach input generate flatten(TOKENIZE(line)) as word;
grpd = group words by word;
cntd = foreach grpd generate group, COUNT(words);
dump cntd;
```

(From [Programming Pig by Alan Gates, O'Reilly 2011](http://shop.oreilly.com/product/0636920018087.do).)

Each line of the program is one relational operation. In line 2, a
user-defined function (`TOKENIZE`) generates a collection, which is
then flattened. Line 3 groups occurrences of words, and line 4
generates a count of each collection.

# WordCount in Apache Hive SQL

```sql
SELECT word, COUNT(*)
FROM input
  LATERAL VIEW explode(split(text, ' ')) lTable AS word
GROUP BY word
```

(From [Stack Overflow](https://stackoverflow.com/questions/10039949/word-count-program-in-hive).)

There is not a typical SQL statement, and the interesting stuff all
happens on line 3:
* First, the `split` function converts the `text` column from the
  `input` table into an array of strings.
* Next, the `explode` table-valued function converts an array of
  strings into a relation with one string column.
* Last, the `LATERAL VIEW` keywords are work around oddities in SQL
  semantics. `VIEW` tells Hive to treat the result of a table function
  as a relation (without it, the only thing you can include in the
  `FROM` clause are tables and sub-queries), and `LATERAL` makes
  previous entries in the `FROM` clause (in this case the `text`
  column of the `input` relation) visible inside the function.

(`LATERAL VIEW` is Hive-specific syntax. Standard SQL would use `CROSS
JOIN LATERAL TABLE`; in Oracle, Microsoft SQL Server, and
[Apache Calcite](https://issues.apache.org/jira/browse/CALCITE-1472)
you can also use `CROSS APPLY`.)

The overall effect is nested 'for' loops: first over the rows in the
`input` relation, then over the array yielded by `split(input.text, '
')` for each row. The syntax is different from the Pig solution, but
the semantics are almost identical. The resulting list of words is
then handled by `GROUP BY` and `COUNT(*)` in the usual way.

# WordCount in Apache Spark

[Apache Spark](https://spark.apache.org) is both an extension to the
MapReduce paradigm and a successor to the Hadoop engine. Its binding
to the Scala language makes for concise programs, as followign example
shows. It also has a distributed processing engine that is more
efficient than Hadoop, especially for shorter-lived jobs.

```scala
val textFile = sc.textFile("hdfs://...")
val counts = textFile.flatMap(line => line.split(" "))
                 .map(word => (word, 1))
                 .reduceByKey(_ + _)
counts.saveAsTextFile("hdfs://...")
```

(From [Apache Spark Examples](https://spark.apache.org/examples.html).)

Spark has many more operations than just map and reduce, but this
example clearly shows the same map-reduce structure.

Spark is a platform rather than a language: the calls to methods
`flatMap`, `map` and `reduceByKey` do not actually process data but
build an expression in Spark's algebra. The arguments to those methods
are Scala functions. When `saveAsTextFile` is called, the algebra is
planned and executed.

I call this a 'builder' model, and you can see earlier examples in
[DryadLINQ](https://www.usenix.org/legacy/events/osdi08/tech/full_papers/yu_y/yu_y.pdf),
[FlumeJava](https://research.google/pubs/pub35650/), and
[Cascading](https://www.cascading.org/). While the primary interface
to [Apache Calcite](https://calcite.apache.org) is SQL, its builder API
[RelBuilder](https://calcite.apache.org/docs/algebra.html#algebra-builder)
is popular with people writing query optimizers.

A builder system inevitably has two languages: the host language in
which you write the programs (Scala in the case of Spark) and the
engine's own algebra. For small expressions (for example a filter
condition) some builders have an expression algebra, while others use
fragments of the host language (such as the Scala fragment `word =>
(word, 1)` above).

A builder has an underlying algebra, which means that the large-scale
program can be optimized by re-organizing the algebraic operators. The
mix of languages means that you can use the power of the host language
to write user-defined functions without stepping out of the
environment (the way you would have to, say, leave SQL in order to
write a UDF in Java).

But the seams between the algebra and the host language are always
apparent. They have different type systems, for instance, and if the
language type-checks in the host language it still may not type-check
in the algebra. And those fragments of host language may be opaque to
the optimizer and prevent advanced optimizations.

Reflecting on these problems, I came up with
[Morel](https://github.com/hydromatic/morel), a language that has the
power of a general-purpose language (due to its SML ancestry) but with
support for relational expressions in the language, so that you
naturally express data-oriented problems in relational algebra.

Unlike a builder, Morel is one language. The algebra is the parse tree
of the program, and the query optimizer is built into the language
parser.

# WordCount in Morel

The solution to the WordCount problem in Morel is very concise:

<div class="language-sml highlighter-rouge">
<div class="highlight">
<pre class="highlight"><code><span class="kr">from</span> <span class="nv">line</span> <span class="kr">in</span> <span class="n">lines,</span>
    <span class="nv">word</span> <span class="kr">in</span> <span class="n">split line</span>
  <span class="kr">group</span> <span class="n">word</span> <span class="kr">compute</span> <span class="n">count</span></code></pre>
</div>
</div>

So concise that it needs some explanation. The `from` keyword (an
feature of Morel that is not present in Standard ML) creates a
[list comprehension](https://en.wikipedia.org/wiki/List_comprehension).
You can think of it as a 'for' loop, but declarative rather than
imperative. Inside the loop are not actions but expressions. The whole
`from` is an expression whose value is a list, and the elements of
that list are defined by those inner expressions.

One difference from SQL is that collections can be composed of any
value, not just records; `lines` is a list of strings, and therefore
at any moment during the iteration `line` is a string. (The Hive SQL
example is confusing because it has two single-column relations,
`input` and `lTable`, but it has to use column names, `text` and
`word`, in expressions.)

SQL makes a big distinction between relations (which may appear in a
`FROM` clause) and collections such as arrays (which may only appear
where expressions can appear, such as in the `SELECT` and `WHERE`
clauses). Morel makes no such distinction. `from` works on any
collection-valued expression, which may be a list of strings, a list
of records, or a table stored in a relational database.

As a result, we don't tend to use the term 'query' in Morel. In other
languages, a 'query' is an expression that operates on relations, but
in Morel we just call it an expression.

Is `from` a query operator? It reminds us of `SELECT` because it uses
relational operations -- scan, join and aggregate in this example, and
also filter and sort -- but it's just one of many ways that you can
operate on lists in Morel.

The solution -- all 3 lines of it -- is a single `from` expression:
<ul>

<li>The first line, <span class="highlight"><code>
  <span class="kr">from</span>
  <span class="nv">line</span>
  <span class="kr">in</span>
  <span class="n">lines</span></code></span>, assigns each element of
  <code>lines</code> in turn to a variable <code>line</code> of type
  <code>string</code>.</li>

<li>The second line, <span class="highlight"><code>
  <span class="nv">word</span>
  <span class="kr">in</span>
  <span class="n">split</span>
  <span class="n">line</span></code></span>, applies the
  <code>split</code> function to <code>line</code> to yield an array
  of strings, and assigns each element of of the array in turn to a
  variable <code>word</code>. (We don't need the equivalent of SQL's
  <code>LATERAL</code>, because <code>line</code> is implicitly
  visible in the inner-loop.)</li>

<li>The third line, <span class="highlight"><code>
  <span class="kr">group</span>
  <span class="nv">word</span>
  <span class="kr">compute</span>
  <span class="n">count</span></code></span>, gathers records into
  groups that have the same <code>word</code> value, then applies the
  built-in <code>count</code> aggregate function to those groups. The
  result is a list of records with fields <code>word</code> and
  <code>count</code>.</li>
</ul>

# A more complete solution

The above Morel solution works, but it assumes that a `split` function
is available.  (The other solutions in other languages have the same
problem; this is especially onerous in Pig and Hive, where someone
would have to write a UDF in a language such as Java, compile it,
package it in a JAR file, add the JAR file to the classpath, and
restart the runtime.)

A better solution would solve the problem all in the one language, and
ideally in the same block of code, without requiring an extra
compilation step. Because Morel is a general-purpose language, we can
declare the `split` function inline:

```sml
fun wordCount lines =
  let
    fun split0 [] word words = word :: words
      | split0 (#" " :: s) word words = split0 s "" (word :: words)
      | split0 (c :: s) word words = split0 s (word ^ (String.str c)) words
    fun split s = List.rev (split0 (String.explode s) "" [])
  in
    from line in lines,
        word in split line
    group word compute count
  end;
```

gives signature

```sml
val wordCount = fn : string list -> {count:int, word:string} list
```

Another improvement is that the new solution is not an expression, but
a function. The previous solution was a expression that assumed that
there is a list called `lines` in the environment, but the function
can easily be applied to any value.

Now let's run it:
```sml
wordCount ["a skunk sat on a stump",
    "and thunk the stump stunk",
    "but the stump thunk the skunk stunk"];
val it =
  [{count=2,word="a"},{count=3,word="the"},{count=1,word="but"},
   {count=1,word="sat"},{count=1,word="and"},{count=2,word="stunk"},
   {count=3,word="stump"},{count=1,word="on"},{count=2,word="thunk"},
   {count=2,word="skunk"}] : {count:int, word:string} list
```

# Conclusion

We have seen the solutions to the WordCount problem in 5 languages:
MapReduce, Standard ML, Pig, Hive SQL, Spark, and Morel. Pig and Hive
have powerful high-level query languages but rely on UDFs written in
another language. MapReduce and Spark use the power of their native
language but rely on an external framework (whose real language is a
relational algebra created by a builder) to carry out the processing.

Only Morel brings high-level query operators into a language that can
also solve general-purpose problems.

Morel lies at the intersection of functional programming and query
languages, taking the best of both worlds. Over the next few weeks,
this blog will drill deeper into both of those aspects. We shall look
at how to express SQL concepts such as `GROUP BY` and `ORDER BY` in
Morel, and also what it means to have functions as first-class values
in a query language.

If you have comments, please reply on Twitter:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>

This article
[has been updated](https://github.com/julianhyde/share/commits/main/blog/_posts/2020-03-31-word-count-revisited.md).
