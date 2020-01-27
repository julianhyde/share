---
layout: post
title: Table macros
date: '2014-05-05T15:04:00.000-07:00'
author: Julian Hyde
tags:
modified_time: '2014-05-05T15:04:17.485-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-3411542878260572546
blogger_orig_url: https://julianhyde.blogspot.com/2014/05/table-macros.html
---

Table macros are a
[new Optiq feature](https://issues.apache.org/jira/browse/CALCITE-222)
(since release 0.6) that combine the efficiency of tables with the
flexibility of functions.

Optiq offers a convenient model for presenting data from multiple
external sources via a single, efficient SQL interface. Using
adapters, you create a schema for each external source, and a table
for each data set within a source.

But sometimes the external data source does not consist of a fixed
number of data sets, known ahead of time. Consider, for example,
Optiq’s web adapter,
[optiq-web](https://github.com/HenryOlson/optiq-web), which makes any
HTML table in any web page appear as a SQL table. Today you can create
an Optiq model and define within it several tables.

Optiq-web’s [home page](https://github.com/HenryOlson/optiq-web) shows
an example where you can create a schema with tables “Cities” and
“States” (based on the Wikipedia pages
[List of states and territories of the United States](https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States)
and
[List of United States cities by population](https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population))
and execute a query to find out the proportion of the California’s
population that live in cities:

{% highlight sql %}
SELECT COUNT(*) "City Count",
  SUM(100 * c."Population" / s."Population") "Pct State Population"
FROM "Cities" c, "States" s
WHERE c."State" = s."State" AND s."State" = 'California';
{% endhighlight %}

But what if you want to query a URL that isn’t in the schema? A table
macro will allow you to do this:

{% highlight sql %}
SELECT * FROM TABLE(
  web('https://en.wikipedia.org/wiki/List_of_countries_by_population'));
{% endhighlight %}

`web` is a function that returns a table. That is, a `Table` object,
which is the definition of a table. In Optiq, a table definition
doesn’t need to be assigned a name and put inside a schema, although
most do; this is a free-floating table. A table just needs to be able
to describe its columns, and to be able to convert itself to
relational algebra. Optiq invokes it while the query is being planned.

Here is the `WebTableMacro` class:

{% highlight java %}
public class WebTableMacro {
  public Table eval(String url) {
    Map<String, Object> operands = new HashMap<String, Object>();
    operands.put("url", url);
    return new WebTable(operands, null);
  }
}
{% endhighlight %}

And here is how you define a WEB function based upon it in your JSON
model:

{% highlight json %}
{
  "version": "1.0",
  "defaultSchema": "ADHOC",
  "schemas": [
    {
      "name": "ADHOC",
      "functions": [
        {
          "name": "WEB",
          "className": "com.example.WebTableMacro"
        }
      ]
    }
  ]
}
{% endhighlight %}

Table macros are a special kind of table function. They are defined in
the same in the model, and invoked in the same way from a SQL
statement. A table function can be used at prepare time if (a) its
arguments are constants, and (b) the table it returns implements
`TranslatableTable`. If it fails either of those tests, it
will be invoked at runtime; it will still produce results, but will
have missed out on the advantages of being part of the query
optimization process.

What kind of advantages can the optimization process bring? Suppose a
web page that produces a table supports URL parameters to filter on a
particular column and sort on another. We could write planner rules
that push take a `FilterRel` or `SortRel` on top
of a `WebTableScan` and convert them into a scan with extra
URL parameters. A table that came from the `web` function
would be able to participate in that process.

The name ‘table macros’ is inspired by
[Lisp macros](http://stackoverflow.com/questions/267862/what-makes-lisp-macros-so-special">)
-- functions that are invoked at compile time rather than run
time. Macros are an extremely powerful feature in Lisp and I hope they
will prove to be a powerful addition to SQL. But to SQL users, a more
familiar name might be ‘parameterized views’.

Views and table macros are both expanded to relational algebra before
the query is optimized. Views are specified in SQL, whereas table
macros invoke user code (it takes some logic to handle those
parameters). Under the covers, Optiq’s views are implemented using
table macros. (They always have been -- we’ve only just got
around to making table macros a public feature.)

To sum up. Table macros are powerful new Optiq feature that extend the
reach of Optiq to data sources that have not been pre-configured into
an Optiq model. They are a generalization of SQL views, and share with
views the efficiency of expanding relational expressions at query
compilation time, where they can be optimized. Table macros will help
bring a SQL interface to yet more forms of data.
