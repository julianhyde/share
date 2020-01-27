---
layout: post
title: Mondrian URLs and Jakarta Commons VFS
date: '2007-01-01T23:04:00.000-08:00'
author: Julian Hyde
tags:
modified_time: '2007-01-02T00:03:25.223-08:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-8161555889276555364
blogger_orig_url: https://julianhyde.blogspot.com/2007/01/mondrian-urls-and-jakarta-commons-vfs.html
---

If you've used Mondrian, you're probably familiar with how Mondrian
loads its schema from a URL embedded in the connect string.

A Mondrian connection is a URL which contains a reference to an XML
file containing a Mondrian schema definition, information to connect
to the JDBC database which holds the data, and various other
parameters. For example,

```
Provider=Mondrian; Jdbc='jdbc:mysql://localhost/foodmart'; JdbcUser=foodmart; JdbcPassword=foodmart; JdbcDrivers=com.mysql.jdbc.Driver; Catalog=file:demo/FoodMart.xml
```

Embedded within the connect string URL is another URL, here
`file:demo/FoodMart.xml`, from where Mondrian should load its schema.

Until now, the URL following the `Catalog` keyword could only one of
the small number of protocols supported by
[java.net.URL](https://java.sun.com/j2se/1.5.0/docs/api/java/net/URL.html),
such as 'http' or 'file'. I've just changed Mondrian to use
[Jakarta Commons VFS](https://jakarta.apache.org/commons/vfs/)
to resolve URLs, which is a more powerful and extensible scheme.

With VFS, you can use the same builtin protocols, some
[new builtin protocols](https://jakarta.apache.org/commons/vfs/filesystems.html),
and even define your own protocol. For example, when used within
[Pentaho BI Platform](https://www.pentaho.com/products/bi_platform/),
Mondrian could use the URL

```
solution:/sales/schemas/my_mondrian_model.xml
```

to reference a Mondrian schema file stored within Pentaho's solution
repository. This is possible because the Pentaho folks have exposed
their solution repository as a custom filesystem.

You can even create a URL which references a file within a JAR within
a zip that exists on an FTP site.

This change will be released as part of mondrian-2.3.
