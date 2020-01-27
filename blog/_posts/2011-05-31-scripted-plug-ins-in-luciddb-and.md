---
layout: post
title: Scripted plug-ins in LucidDB and Mondrian
date: '2011-05-31T00:03:00.000-07:00'
author: Julian Hyde
tags:
- mondrian luciddb sqlstream javascript udf
modified_time: '2011-05-31T16:11:03.111-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-2669826520389763611
blogger_orig_url: https://julianhyde.blogspot.com/2011/05/scripted-plug-ins-in-luciddb-and.html
---

I saw a demo last week of scripted user-defined functions in
[LucidDB](http://www.luciddb.org/), and was inspired this
weekend to add them to [Mondrian](https://mondrian.pentaho.com/).

Kevin Secretan of DynamoBI
[has just contributed](http://www.thejach.com/view/2011/5/can_your_sql_database_do_this)
some extensions to LucidDB to allow you to call
script code (such as JavaScript or Python) in any place where you can
have a user-defined function, procedure, or transform. This feature
builds on a [JVM feature introduced in Java 1.6](http://www.jcp.org/en/jsr/detail?id=223),
[scripting engines](https://download.oracle.com/javase/6/docs/technotes/guides/scripting/programmer_guide/index.html).

Scripted functions may be a little slower than Java user-defined
functions, but what they lose in performance they more than make up in
flexibility. Writing user-defined functions in Java has always been
laborious: you need to write a Java class, compile it, put it in a
jar, put the jar on the server's class path, and restart the
server. Each time you find a bug, you need to repeat that process, and
that can easily take a number of minutes each cycle. Because scripted
functions are compiled on the fly, you can cycle faster, and spend
more of your valuable time working on the actual application.

I am speaking about LucidDB (and SQLstream) here, but the same
problems exist for Mondrian plug-ins. Scripting is an opportunity to
radically speed up development of application extensions, because
everything can be done in the schema file. (Or via the
workbench... but that part isn't implemented yet.)

Mondrian has several plug-in types, all today implemented using a Java
SPI. I chose to make scriptable those plug-ins that are defined in a
mondrian schema file: user-defined function, member formatter,
property formatter, and cell formatter. A small syntax change to the
schema file allowed you to chose whether to implement these plug-ins
by specifying the name of a Java class (as before) or an inline
script.

As an example, here is the factorial function defined in JavaScript:

{% highlight xml %}
<UserDefinedFunction name="Factorial">
  <Script language="JavaScript">
    function getParameterTypes() {
      return new Array(new mondrian.olap.type.NumericType());
    }
    function getReturnType(parameterTypes) {
      return new mondrian.olap.type.NumericType();
    }
    function execute(evaluator, arguments) {
      var n = arguments[0].evaluateScalar(evaluator);
      return factorial(n);
    }
    function factorial(n) {
      return n <= 1 ? 1 : n * factorial(n - 1);
    }
  </Script>
</UserDefinedFunction>
{% endhighlight %}

A user-defined function ironically requires several functions in order
to provide the metadata needed by the MDX type system. The member,
property and cell formatters are simpler. They require just one
function, so mondrian dispenses with the function header, and requires
just the 'return' expression inside the Script element. For example,
here is a member formatter:

{% highlight xml %}
<Level name="name" column="column">
  <MemberFormatter>
    <Script language="JavaScript">
      return member.getName().toUpperCase();
    </Script>
  </MemberFormatter>
</Level>
{% endhighlight %}

You can of course write multiple statements, if you wish. Since
JavaScript is embedded in the JVM, your code can call back into Java
methods, and use the full runtime Java library.

There are examples of cell formatters and property formatters in the
latest [schema guide](http://p4webhost.eigenbase.org:8080/open/mondrian/doc/schema.html).

If you are concerned about performance, you could always translate
this code back to a Java UDF when it is fully debugged. However, you
might be pleasantly surprised by the performance of JavaScript: I was
able to invoke a script function about 20,000 times per second. And I
hear that there is a [Janino](http://docs.codehaus.org/display/JANINO/Home)
"scripting engine" that compiles Java code into bytecode on the
fly. In principle, it should be as fast as a real Java UDF.

I'd love to hear about Janino, or in fact any other scripting engine,
with the Mondrian or LucidDB scripted functions.

By the way, you can expect to see scripted functions in a release of
SQLstream not too far in the future. The Eigenbase project makes it
easy to propagate features between projects, and this feature is too
good not to share.
