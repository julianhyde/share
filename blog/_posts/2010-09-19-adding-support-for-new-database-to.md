---
layout: post
title: Adding support for a new database to mondrian
date: '2010-09-19T13:15:00.000-07:00'
author: Julian Hyde
tags:
- mondrian jdbc dialect compatibility contributions
modified_time: '2010-09-20T09:52:24.910-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-3277113486391955441
blogger_orig_url: https://julianhyde.blogspot.com/2010/09/adding-support-for-new-database-to.html
---

Mondrian supports a large number of back-end databases. So many in
fact, that we rely on contributors to add the support, and we have
tried to standardize the steps to support a new database.

I thought it might be worthwhile to reiterate those steps. The steps are:
1. Write a dialect class. The dialect must implement the
   [`Dialect`](https://mondrian.pentaho.com/api/mondrian/spi/Dialect.html)
   interface, and will probably be a subclass of
   [`JdbcDialectImpl`](https://mondrian.pentaho.com/api/mondrian/spi/impl/JdbcDialectImpl.html).
   The only prerequisites are that your database has a JDBC driver and
   supports SQL-92: SELECT .. FROM .. JOIN .. GROUP BY. Mondrian will
   glean as much information as it can, such as how your database
   quotes identifiers that contain mixed-case or spaces, from the JDBC
   driver. But you will need to override methods to provide
   information that the JDBC does not provide (e.g. how to affect
   whether NULL values sort first or last) or if the JDBC driver lies.
2. Add your dialect to the
   [`META-INF/services/mondrian.spi.Dialect`](http://p4web.eigenbase.org/open/mondrian-release/3.2/src/main/META-INF/services/mondrian.spi.Dialect)
   file so that mondrian can find it.
3. Modify `MondrianFoodMartLoader` and (optionally)
   `bin/loadFoodMart.sh` so that you can load data from
   `demo/FoodMartData.sql` into your database. (We prefer not to add a
   dump file for each database to the distro. The FoodMart data set is
   about 5MB compressed, so each dump file would bloat the size of the
   distro.)
4. Get the test suite to pass. We strongly recommend that you focus on
   getting `DialectTest` to pass first; once the dialect is accurate,
   most other mondrian tests should just pass. If you need help, post
   your test output to the
   [mondrian developers email list](https://lists.pentaho.org/mailman/listinfo/mondrian);
   someone is likely to have seen the problem before, on another database.
5. Add a section to `mondrian.properties` describing a typical connect
   string for your database.

When these steps are complete, post the files in a JIRA case. I will
add your database to the list of supported databases and mention it in
the release notes of the next mondrian release.

Then, please join the email list and stay in touch. We are not able to
test all supported databases each release. When we announce a beta of
a mondrian release, run the test suite against your database, and let
us know if we've broken anything. Supporting a large array of
databases is not hard, but it is even easier if we do it as a
community.
