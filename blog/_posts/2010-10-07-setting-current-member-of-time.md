---
layout: post
title: Setting the current member of the time dimension automatically
date: '2010-10-07T18:39:00.000-07:00'
author: Julian Hyde
tags:
- mondrian mdx etl pentaho analyzer
modified_time: '2010-10-07T18:39:22.349-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-2265194776603744953
blogger_orig_url: https://julianhyde.blogspot.com/2010/10/setting-current-member-of-time.html
---

The question came up today, "How do I write my report so that the current member of the Time dimension is the most recent member for which transactional data are loaded?"<br /><br />It's a good question, and comes up often. Let's look at some ways that you could solve it.<br /><h4>Attempt #1: CurrentDateMember</h4>One might think that 'today' would suffice (using Mondrian's <a href="http://julianhyde.blogspot.com/2006/10/mondrian-22-cube-designer-and.html">CurrentDateMember</a> MDX function), but since many enterprises only run the ETL process overnight, it isn't always the right answer. Some nights (gasp!) the ETL process fails, so even 'yesterday' may not be right answer.<br /><h4>Attempt #2: defaultMember</h4>The default member of a hierarchy is its 'all' member, or if there is no 'all' member, the first member of the first level. Mondrian allows you to change the default member in the schema file using the defaultMember XML attribute. To do this for the Time hierarchy, you'd write the following:<br /><blockquote>&lt;Dimension name="Time" type="TimeDimension"&gt;<br/>&nbsp;&nbsp;&lt;Hierarchy defaultMember="[Time].[2010].[10].[07]" hasAll="false" primarykey="time_id"&gt;<br/>&nbsp;&nbsp;&nbsp;&nbsp;...</blockquote><br />You'd have to find some way to re-generate the schema XML file each time a load was successful (or use a <a href="http://mondrian.pentaho.com/api/mondrian/spi/DynamicSchemaProcessor.html">DynamicSchemaProcessor</a> to generate a schema on the fly, substituting a template schema that contains a token for the default member). But I wouldn't recommend this approach. Default members of hierarchies don't just affect what appears on the screen; they are the default context for all MDX calculations (where the calculation isn't explicitly set in the formula), and so all calculations will change every time you reload your data warehouse. This probably isn't what your users want.<br /><h4>Attempt #3: Parameter</h4>Define each of your reports with a parameter that holds the initial member of the time hierarchy for that report. Use some kind of scripting (say a custom piece of JavaScript inside <a href="http://code.google.com/p/pentaho-cdf/">Pentaho's CDF</a>, or a Pentaho action sequence) to populate that parameter as the report is launched.<br /><br />This approach is on the right track, but isn't quite perfect. This will give the your users what they want, but you will have to maintain a piece of script for every report you define.<br /><h4>Attempt #4: Parameter with MDX expression as default value</h4>This improves on attempt #3 by putting the expression to initialize the parameter inside the definition of the parameter. You don't need to provide a value of the parameter when you launch the report (unless you want to), and that means you don't need to write those pesky scripts.<br /><br />Although the question called for "the most recent [Time] member for which transactional data are loaded", I'm going to drive home the point with an example that qualifies on another dimension as well. This query will launch with the most recent month for which anyone in the town of Bellflower, California bought Good beer.<br /><blockquote>select [Measures].[Unit Sales] on 0,<br/>&nbsp;[Product].Children on 1<br/>from [Sales]<br/>where Parameter(<br/>&nbsp;&nbsp;"Time period of interest",<br/>&nbsp;&nbsp; [Time],<br/>&nbsp;&nbsp; Tail(<br/>&nbsp;&nbsp; &nbsp; {<br/>&nbsp;&nbsp;&nbsp;&nbsp; [Time],<br/>&nbsp;&nbsp; &nbsp; &nbsp; Filter(<br/>&nbsp;&nbsp; &nbsp; &nbsp; &nbsp; [Time].[Month].Members,<br/>&nbsp;&nbsp; &nbsp; &nbsp; &nbsp; 0 &lt; ([Customers].[USA].[CA].[Bellflower],<br/>&nbsp;&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; [Product].[Drink].[Alcoholic Beverages].[Beer and Wine].[Beer].[Good]))<br/>&nbsp;&nbsp; &nbsp; },<br/>&nbsp;&nbsp; &nbsp; 1),<br/>&nbsp;&nbsp; "Time period of interest for current analysis. By default the most recent month for which transactions exist.")</blockquote>Filter evaluates the sames of Good beer in Bellflower every month and throws out months where no Good beer was sold, and Tail chooses the last. The dummy first element {[Time], ... } is to ensure that if the residents of Bellflower have never bought Good beer, the report still launches with a valid member of the time dimension.<br /><br />The results are as follows:<blockquote><pre>| &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;| Unit Sales |<br />+----------------+------------+<br />| Drink &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;| &nbsp; &nbsp; &nbsp;2,344 |<br />| Food &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; | &nbsp; &nbsp; 18,278 |<br />| Non-Consumable | &nbsp; &nbsp; &nbsp;4,648 |<br /></pre></blockquote><br />and the member in the slicer is [Time].[1997].[11]. (Yes, it's a long time since the unfortunate residents of Bellflower, CA drank Good beer.) This report doesn't contain a great deal of detail, but it can be used as a starting point for an series of slice, dice and pivot operations to interactively explore the data, and the same Time member will be carried forward until the user decides to switch to another time.<br /><br /><b>Attempt #5. Schema parameters</b>I stopped looking for a solution when I had written the above query in attempt #4, but schema parameters are potentially even better. Schema parameters are little-known Mondrian feature that allow you define a parameter once in a schema file, then reference it in any report written against that schema.<br /><br />I haven't tried it, but the solution would look something like the following. To define the parameter, include the following in your schema file:<br /><blockquote>&lt;Parameter defaultvalue="Tail({ [Time], Filter([Time].[Month].Members, 0 &amp;lt; ([Customers].[USA].[CA].[Bellflower], [Product].[Drink].[Alcoholic Beverages].[Beer and Wine].[Beer].[Good])) }, 1)" name="Time period of interest" type="Member"/&gt;</blockquote>and reference the parameter in an MDX query using ParamRef:<br /><blockquote>select [Measures].[Unit Sales] on 0,<br/>&nbsp;[Product].Children on 1<br/>from [Sales]<br/>where ParamRef("Time period of interest")</blockquote><h4>Other solutions?</h4>As you can see there are many ways to attack a problem using Mondrian, Pentaho and MDX. Do you know other techniques to solve this problem? Let me know.