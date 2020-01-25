---
layout: post
title: Removing Mondrian's 'high cardinality dimension' feature
date: '2011-06-01T16:26:00.000-07:00'
author: Julian Hyde
tags:
- mondrian high cardinality dimension
modified_time: '2011-06-01T16:26:08.160-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-3831657708509285388
blogger_orig_url: https://julianhyde.blogspot.com/2011/06/removing-mondrians-high-cardinality.html
---

I would like to remove the 'high cardinality dimension' feature in mondrian 4.0.<br /><br />To specify that a dimension is high-cardinality, you set the <a href="http://mondrian.pentaho.com/documentation/xml_schema.php#CubeDimension">highCardinality attribute of the Dimension element</a> to true. This will cause mondrian to scan over the dimension, rather than trying to load all of the children of a given parent member into memory.<br /><br />The goal is a worthy one, but the implementation &mdash; making iterators look like lists &mdash; has a number of architectural problems: it duplicates code; because it allows backtracking for a fixed amount, it works with small dimensions but unpredictably fails with larger ones; and because lists are based on iterators, re-starting an iteration multiple times (e.g. from within a crossjoin) can re-execute complex SQL statements.<br /><br />There are other architectural features designed to help with large dimensions. Many functions can operate in an 'iterable' mode (except that here the iterators are explicit). And for many of the most data-intensive operators, such as crossjoin, filter, semijoin (non-empty), and topcount, we can push down the operator to SQL, and thereby reduce the number of records coming out of the RDBMS.<br /><br />It's always hard to remove a feature. But over the years we have seen numerous inconsistencies, and if we removed this feature in mondrian 4.0, we could better focus our resources. <br /><br />If you are using this feature and getting significant performance benefit, I would like to hear from you. I would like to understand about your use case, and either direct you to another feature that solves the problem, or try to develop an alternative solution in mondrian 4.0. The best place to make comments about these use cases is on the Jira case <a href="http://jira.pentaho.com/browse/MONDRIAN-949">MONDRIAN-949</a>.