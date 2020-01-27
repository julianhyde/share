---
layout: post
title: Explaining the structure of Mondrian schemas
date: '2009-05-19T17:42:00.000-07:00'
author: Julian Hyde
tags:
- mondrian physical schema bnf xsd clapham
modified_time: '2009-05-19T18:15:10.215-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-6648165930275572690
blogger_orig_url: https://julianhyde.blogspot.com/2009/05/explaining-structure-of-mondrian.html
---

There are some
[major schema changes coming in Mondrian 4.0](https://wiki.pentaho.com/display/analysis/Physical+Schema+Design+Discussion),
and I'm writing up specifications for these so that everyone knows
what's coming and has chance to influence it.

But before I do that, I thought I'd try to improve how we describe the
structure of XML schemas in the present release, just a bit. I have
tried a couple of things. First, I created an XML skeleton that shows
which elements can occur inside which other elements:

<blockquote style="text-indent: -20px">
<div style="padding-left:20px">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_Schema">Schema</a>&gt;</div>
  <div style="padding-left:40px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_Cube">Cube</a>&gt;</div>
      <div style="padding-left:60px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_Table">Table</a>&gt;</div>
          <div style="padding-left:80px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_AggName">AggName</a>&gt;</div>
              <div style="padding-left:100px;">aggElements</div>
          <div style="padding-left:80px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_AggPattern">AggPattern</a>&gt;</div>
              <div style="padding-left:100px;">aggElements</div>
      <div style="padding-left:60px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_Dimension">Dimension</a>&gt;</div>
          <div style="padding-left:80px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_Hierarchy">Hierarchy</a>&gt;</div>
              <div style="padding-left:100px;">relation</div>
              <div style="padding-left:100px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_Closure">Closure</a>/&gt;</div>
              <div style="padding-left:100px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_Level">Level</a>&gt;</div>
                  <div style="padding-left:120px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_KeyExpression">KeyExpression</a>&gt;</div>
                      <div style="padding-left:140px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_SQL">SQL</a>/&gt;</div>
                  <div style="padding-left:120px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_NameExpression">NameExpression</a>&gt;</div>
                      <div style="padding-left:140px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_SQL">SQL</a>/&gt;</div>
                  <div style="padding-left:120px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_CaptionExpression">CaptionExpression</a>&gt;</div>
                      <div style="padding-left:140px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_SQL">SQL</a>/&gt;</div>
                  <div style="padding-left:120px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_OrdinalExpression">OrdinalExpression</a>&gt;</div>
                      <div style="padding-left:140px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_SQL">SQL</a>/&gt;</div>
                  <div style="padding-left:120px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_ParentExpression">ParentExpression</a>&gt;</div>
                      <div style="padding-left:140px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_SQL">SQL</a>/&gt;</div>
                  <div style="padding-left:120px;">&lt;<a  href="https://mondrian.pentaho.com/documentation/schema.php#XML_Property">Property</a>&gt;</div>
                      <div style="padding-left:140px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_PropertyExpression">PropertyExpression</a>&gt;</div>
                          <div style="padding-left:160px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_SQL">SQL</a>/&gt;</div>
      <div style="padding-left:60px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_DimensionUsage">DimensionUsage</a>&gt;</div>
      <div style="padding-left:60px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_Measure">Measure</a>&gt;</div>
          <div style="padding-left:80px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_MeasureExpression">MeasureExpression</a>&gt;</div>
              <div style="padding-left:100px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_SQL">SQL</a>/&gt;</div>
          <div style="padding-left:80px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_MemberProperty">CalculatedMemberProperty</a>/&gt;</div>
      <div style="padding-left:60px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_CalculatedMember">CalculatedMember</a>&gt;</div>
          <div style="padding-left:80px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_Formula">Formula</a>/&gt;</div>
          <div style="padding-left:80px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_MemberProperty">CalculatedMemberProperty</a>/&gt;</div>
      <div style="padding-left:60px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_NamedSet">NamedSet</a>&gt;</div>
          <div style="padding-left:80px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_Formula">Formula</a>/&gt;</div>
  <div style="padding-left:40px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_VirtualCube">VirtualCube</a>&gt;</div>
      <div style="padding-left:60px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_CubeUsages">CubeUsages</a>&gt;</div>
          <div style="padding-left:80px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_CubeUsage">CubeUsage</a>&gt;</div>
      <div style="padding-left:60px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_VirtualCubeDimension">VirtualCubeDimension</a>&gt;</div>
      <div style="padding-left:60px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_VirtualCubeMeasure">VirtualCubeMeasure</a>&gt;</div>
  <div style="padding-left:40px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_Role">Role</a>&gt;</div>
      <div style="padding-left:60px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_SchemaGrant">SchemaGrant</a>&gt;</div>
          <div style="padding-left:80px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_CubeGrant">CubeGrant</a>&gt;</div>
              <div style="padding-left:100px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_HierarchyGrant">HierarchyGrant</a>&gt;</div>
                  <div style="padding-left:120px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_MemberGrant">MemberGrant</a>/&gt;</div>
      <div style="padding-left:60px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_Union">Union</a>&gt;</div>
          <div style="padding-left:80px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_RoleUsage">RoleUsage</a>/&gt;</div>
  <div style="padding-left:40px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_UserDefinedFunction">UserDefinedFunction</a>/&gt;</div>
  <div style="padding-left:40px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_Parameter">Parameter</a>/&gt;</div>
  <br/>
  relation ::=<br/>
  <div style="padding-left:40px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_Table">Table</a>&gt;</div>
      <div style="padding-left:60px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_SQL">SQL</a>/&gt;</div>
  <div style="padding-left:40px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_View">View</a>&gt;</div>
      <div style="padding-left:60px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_SQL">SQL</a>/&gt;</div>
  <div style="padding-left:40px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_InlineTable">InlineTable</a>&gt;</div>
      <div style="padding-left:60px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_ColumnDefs">ColumnDefs</a>&gt;</div>
          <div style="padding-left:80px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_ColumnDef">ColumnDef</a>&gt;</div>
      <div style="padding-left:60px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_Rows">Rows</a>&gt;</div>
          <div style="padding-left:80px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_Row">Row</a>&gt;</div>
              <div style="padding-left:100px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_Value">Value</a>&gt;</div>
  <div style="padding-left:40px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_Join">Join</a>&gt;</div>
      <div style="padding-left:60px;">relation</div>
  <br/>
  aggElement ::=<br/>
  <div style="padding-left:40px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_AggExclude">AggExclude</a>&gt;</div>
  <div style="padding-left:40px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_AggFactCount">AggFactCount</a>&gt;</div>
  <div style="padding-left:40px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_AggIgnoreColumn">AggIgnoreColumn</a>&gt;</div>
  <div style="padding-left:40px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_AggForeignKey">AggForeignKey</a>&gt;</div>
  <div style="padding-left:40px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_AggMeasure">AggMeasure</a>&gt;</div>
  <div style="padding-left:40px;">&lt;<a href="https://mondrian.pentaho.com/documentation/schema.php#XML_AggLevel">AggLevel</a>&gt;</div>
</blockquote>

You can see the full version in the
[Mondrian schema guide](https://mondrian.pentaho.com/documentation/schema.php#Schema_files).

This approach shows where things are located, but it doesn't show how
many of each element can belong to a particular parent element, or the
order in which they are required. So, I wrote up a small
[BNF grammar](http://p4webhost.eigenbase.org:8080/open/mondrian/doc/schema.bnf) and used
[Clapham](http://clapham.hydromatic.net) to generate a
[railroad diagram](http://clapham.hydromatic.net/mondrian-3.1-bnf/). For comparison, the
[railroad diagram for the work-in-progress mondrian-4.0 schema is here](http://clapham.hydromatic.net/mondrian-4.0-bnf/).
