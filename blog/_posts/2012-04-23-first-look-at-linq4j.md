---
layout: post
title: A first look at linq4j
date: '2012-04-23T20:47:00.000-07:00'
author: Julian Hyde
tags:
modified_time: '2012-04-23T22:48:35.250-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-6779529697171172010
blogger_orig_url: https://julianhyde.blogspot.com/2012/04/first-look-at-linq4j.html
---

This is a sneak peek of an exciting new data management
technology. linq4j (short for "Language-Integrated Query for Java") is
inspired by
[Microsoft's LINQ technology](https://msdn.microsoft.com/en-us/library/bb308959.aspx),
previously only available on the .NET platform,
and adapted for Java. (It also builds upon ideas I had in my earlier
[Saffron project](https://saffron.sourceforge.net/overview.html).)

<iframe src="http://player.vimeo.com/video/40910401" width="500" height="281" frameborder="0" webkitAllowFullScreen mozallowfullscreen allowFullScreen></iframe>

I [launched the linq4j project](https://github.com/julianhyde/linq4j/commit/ab31757589ed2423abe6a9454edafb5e77e69ffd)
less than a week ago, but already you can do
select, filter, join and groupBy operations on in-memory and SQL data.

In this demo, I write and execute sample code against the working
system, and explain the differences between the key interfaces
[Iterable](https://docs.oracle.com/javase/7/docs/api/java/lang/Iterable.html),
[Enumerable](http://www.hydromatic.net/linq4j/apidocs/net/hydromatic/linq4j/Enumerable.html),
and [Queryable](http://www.hydromatic.net/linq4j/apidocs/net/hydromatic/linq4j/Queryable.html).

For those of you who want to get a closer look at the real code,
here's one of the queries shown in the demo:

{% highlight java %}
DatabaseProvider provider =
    new DatabaseProvider(Helper.MYSQL_DATA_SOURCE);
provider.emps
    .where(
        new Predicate1<Employee>() {
            public boolean apply(Employee v1) {
                return v1.manager;
        }
    })
    .join(
        provider.depts,
        new Function1<Employee, Integer>() {
            public Integer apply(Employee a0) {
                return a0.deptno;
            }
        },
        new Function1<Department, Integer>() {
            public Integer apply(Department a0) {
                return a0.deptno;
            }
        },
        new Function2<Employee, Department, String>() {
            public String apply(Employee v1,
                                Department v2) {
                return v1.name + " works in " + v2.name;
            }
        }
    )
    .foreach(
        new Function1<String, Void>() {
            public Void apply(String a0) {
                System.out.println(a0);
                return null;
            }
        }
    );
{% endhighlight %}

and here is its (not yet implemented) sugared syntax:

{% highlight java %}
List<String> strings =
    from emp in provider.emps
      join dept in provider.depts on emp.deptno == dept.deptno
    where emp.manager
    orderBy emp.name
    select emp.name + " works in " + dept.name;
{% endhighlight %}

For more information, visit the [linq4j project's home page](https://github.com/julianhyde/linq4j).
