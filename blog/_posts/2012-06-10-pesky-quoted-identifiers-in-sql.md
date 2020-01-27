---
layout: post
title: Pesky quoted identifiers in SQL
date: '2012-06-10T14:35:00.000-07:00'
author: Julian Hyde
tags:
modified_time: '2012-06-10T14:35:02.339-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-1814599178937302582
blogger_orig_url: https://julianhyde.blogspot.com/2012/06/pesky-quoted-identifiers-in-sql.html
---

The SQL that Mondrian generates is, until now, different than the SQL
that most people would write by hand. Most people don't use spaces or
punctuation in table and column names, and don't enclose identifiers
in quotation marks when writing SQL DDL, DML or queries. Mondrian, on
the other hand, religiously quotes every identifier, whether it needs
it or not.

The two styles are not compatible because on many databases (Oracle is
one example) unquoted identifiers are implicitly converted to
upper-case. If you use lower-case table and column names in Mondrian's
schema, they will not match the upper-case identifiers created during
DDL.

For instance, if you create a table in Oracle using

{% highlight sql %}
CREATE TABLE emp (
  empno INTEGER,
  ename VARCHAR2(30),
  deptno INTEGER);
{% endhighlight %}

then Oracle creates a table called `EMP` with columns `EMPNO`, `ENAME`
and `DEPTNO`. When you query it using

{% highlight sql %}
SELECT ename FROM emp WHERE deptno = 20;
{% endhighlight %}

the effect is as if you had written

{% highlight sql %}
SELECT ENAME FROM EMP WHERE DEPTNO = 20;
{% endhighlight %}

Now, if you'd told Mondrian that the table was called "`emp`",
Mondrian tries to be helpful. It generates the query

{% highlight java %}
SELECT "ename" FROM "emp" WHERE "deptno" = 20;
{% endhighlight %}

Of course, there is no table called "`emp`", only one called "`EMP`",
so on case-sensitive databases such as Oracle this causes an
error. You then need to go back to your schema and change

{% highlight xml %}
<Table name="emp"/>
{% endhighlight %}

to

{% highlight xml %}
<Table name="EMP"/>
{% endhighlight %}

and all other table and column names in your schema. Yuck!

There is now a simpler way. The Schema XML element has a `quoteSql` attribute:

{% highlight xml %}
<Schema name='FoodMart' metamodelVersion='4.0' quoteSql='false'>
{% endhighlight %}

If you set `quoteSql='false'`, Mondrian will not quote identifiers
when generating SQL. (Actually, it will still quote them if they
contain spaces and such. But we recommend that if you use
`quoteSql='false'`, you use sensible table names containing only
alphanumeric characters and '_'.)

More details can be found in
[MONDRIAN-887](https://jira.pentaho.com/browse/MONDRIAN-887). It
is only fixed in the lagunitas branch (i.e., mondrian-4.0 alpha), and
only in new-style schemas (not mondrian-3 style schemas automatically
upgraded). Give it a try and let me know how it works for you.
