---
layout: post
title: Mondrian, Flex and Openbravo
date: '2008-07-07T11:56:00.000-07:00'
author: Julian Hyde
tags:
modified_time: '2008-07-07T12:05:17.054-07:00'
blogger_id: tag:blogger.com,1999:blog-5672165237896126100.post-3379418843354762954
blogger_orig_url: https://julianhyde.blogspot.com/2008/07/mondrian-flex-and-openbravo.html
---

This guy is using
[Adobe Flex as an OLAP client against Openbravo ERP data](https://opensourceerpguru.com/2008/07/06/flex-client-side-olap-for-open-source-erp-openbravo/).
He now plans to "connect Flex to [the Mondrian] OLAP server and let
the OLAP sever do all the hard work".

Sounds like a great idea: keep the big data on the server side, send
just the multidimensional, aggregated results over SOAP (XML for
Analysis), and let Flex do what it does best: fast, rich client.
