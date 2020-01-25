---
layout: post
title: A nice way to populate those pesky closure tables
date: '2007-09-21T13:16:00.000-07:00'
author: Julian Hyde
tags: 
modified_time: '2007-09-21T13:29:51.889-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-1746440897589685236
blogger_orig_url: https://julianhyde.blogspot.com/2007/09/nice-way-to-populate-those-pesky.html
---

If you use mondrian's <a href="http://mondrian.pentaho.org/documentation/schema.php#Parent_child_hierarchies">parent-child hierarchies</a>, you will know that performance sucks unless you create closure tables. Closure tables expand the hierarchy, and allow mondrian to the operations required to roll-up a parent-child hierarchy using raw SQL: really fast.<br /><br />The problem is populating the things. Closure tables contain what computer science profs. call a <a href="http://en.wikipedia.org/wiki/Transitive_closure">transitive closure</a> of the parent-child relation (hence their name), and transitive closures aren't something which relational databases are very good at computing (which is why the database performs so much better when they're around). You can't just define a view, or write a simple SQL statement to populate them. Up til now, you'd have to use a stored procedure (if your database supports them) or write some gnarly JDBC code.<br /><br />Now <a href="http://wiki.pentaho.org/display/EAI/Closure+Generator">Matt Casters has added a Closure Generator step to Pentaho Data Integration (aka Kettle)</a>, which should make everyone's life easier. Yet another reason to use Kettle to load your mondrian schema. The new step debuts in Pentaho 3.0.0-RC1, but it should work with any version of mondrian.<br /><br />Which, I suppose, means we all get to that Friday evening beer a little earlier. I'll drink to that.