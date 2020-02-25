---
layout: post
title:  "FAQs"
date:   2020-02-21 16:32:00 -0800
author: Julian Hyde
image:  todo
tweet:  https://twitter.com/julianhyde/status/todo
---
Here are some FAQs that explain the decisions behind Morel.

## Why build on Standard ML, not Haskell or Scala?

Standard ML is a small language.

Larger languages such as Haskell or Scala are too large for one person
to implement, or to productively hack an existing implementation.

If the Morel experiment is successful, relational extensions could be
added to those languages.

# What is the Morel equivalent of a scalar sub-query?

The following SQL runs a sub-query for each row which returns one row
and one column, and splices in the result:

{% highlight sql %}
SELECT dept.deptno,
  (SELECT COUNT(*)
   FROM hr.emps AS emp
   WHERE emp.deptno = dept.deptno) AS emp_count
FROM hr.depts AS dept
{% endhighlight %}

What is the equivalent in Morel?

{% highlight sml %}
from dept in hr.depts
yield {dept.deptno,
       emp_count =
         hd (from hr.emps as emp
             where emp.deptno = dept.deptno
             group yield count)}
{% endhighlight %}

## Can scalar sub-queries return more than one column?

In SQL a scalar sub-query must return just one column. For example,
the following would be useful but is not legal SQL:

{% highlight sql %}
SELECT emp.ename,
  (SELECT dept.dname, dept.location
   FROM hr.depts AS dept
   WHERE dept.deptno = emp.deptno)
FROM hr.emps AS emp
{% endhighlight %}

Can Morel do better?

{% highlight sml %}
from emp in hr.emps,
  dept = hd (from hr.depts as dept
             where dept.deptno = emp.deptno)
yield {emp.ename, dept.dname, dept.location}
{% endhighlight %}

**Note**: `=` inside `from` is not currently implemented.

## What are some differences between Morel syntax and Standard ML?

Clearly the main extension is the `from` clause (with associated `in`,
`group` and `yield` keywords) for relational comprehensions.

But there are some syntactic sugarings:

# Postfix field references

In Standard ML, fields are accessed via label functions. For example,

{% highlight sml %}
- val shaggy = {name = "Shaggy", age = 19};
val shaggy = {age=19,name="Shaggy"} : {age:int, name:string}
- #name shaggy;
val it = "Shaggy" : string
{% endhighlight %}

In Morel, we allow `expr.field` as an alternative to `#field expr`:
  
{% highlight sml %}
- shaggy.name;
val it = "Shaggy" : string
{% endhighlight %}

Function application and the '.' operator are both left-associative.
If you need to reference a field within a field, the '.' notation is
concise, whereas function application requires parentheses:

{% highlight sml %}
- #zipcode (#address shaggy);
val it = "90210" : string
- shaggy.address.zipcode;
val it = "90210" : string
{% endhighlight %}

## Implicit field names in records

In Standard ML, you have to name each field of a record:

{% highlight sml %}
- val emp = {ename = "Shaggy", empno = 100};
val emp = {empno=100,ename="Shaggy"} : {empno:int, ename:string}
- val dept = {dname = "Marketing", deptno = 10};
val dept = {deptno=10,dname="Marketing"} : {deptno:int, dname:string}
- val dog = "Scooby";
val dog = "Scooby" : string
- val result = {dog = dog, dname = #dname dept, ename = #ename emp, ten = 10};
val result = {dog="Scooby",dname="Marketing",ename="Shaggy",ten=10}
  : {dog:string, dname:string, ename:string, ten:int}
{% endhighlight %}

In Morel, you can omit labels if they can easily be deduced. There are
three cases:

* Label function call: `#field e` has implicit label `field`
* Postfix field reference: `e.field` has implicit label `field`
* Identifier: `id` has implicit label `id`

The previous example can be abbreviated as follows:

{% highlight sml %}
- val result = {dog, #dname dept, emp.ename, ten = 10};
val result = {dog="Scooby",dname="Marketing",ename="Shaggy",ten=10}
  : {dog:string, dname:string, ename:string, ten:int}
{% endhighlight %}

This syntactic sugar can be used anywhere but is especially useful
when the `yield` clause returns a record; the effect is analogous to
omitting `AS` column aliases from the `SELECT` list.  For example

{% highlight sml %}
from dept in hr.depts,
  emp in hr.emps
yield {dept.deptno, emp.ename}
{% endhighlight %}

is shorthand for

{% highlight sml %}
from dept in hr.depts,
  emp in hr.emps
yield {deptno = dept.deptno, ename = emp.ename}
{% endhighlight %}

You can use similar abbreviations in the `group` clause. For example

{% highlight sml %}
from emp in hr.emps
group emp.deptno as deptno
  compute sum of emp.sal as sumSal;
{% endhighlight %}

can be abbreviated as

{% highlight sml %}
from emp in hr.emps
group emp.deptno
  compute sum of emp.sal as sumSal;
{% endhighlight %}

# Comments

If you have comments, please reply on Twitter:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>
