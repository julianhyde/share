---
layout: post
title:  "SQL flaws"
date:   2020-02-20 16:32:00 -0800
author: Julian Hyde
categories: sql ml morel
---
SQL is an incredibly successful programming language.
There are a few things that could have been done better, or at least differently.
I've been mulling these things over considering the design of a new data language.

# SQL flaws

* Different language for table and column expressions
* Outdated type system, with length-limited strings, no generics, algebraic types, awkward handling of null values (via constraints rather than type)
* No higher-order functions or function values

SQL does a lot of things right. Data structures are
immutable, functions do not have side-effects and return the same
result given the same arguments. The language is mathematically pure
and optimizations are possible. These the kind of values you
expect from a functional programming language.

# Fixing the flaws

The flaws may be fixable. SQL continues to evolve, and so the flaws will probably be fixed.

But it may be easier to take a functional programming language and add
support for relations.

The biggest danger is that that language becomes too powerful. Too complex and verbose to write queries concisely 