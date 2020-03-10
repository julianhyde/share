---
layout: post
title:  "Morel release 0.2"
date:   2020-03-10 09:00:00
author: Julian Hyde
image:  /assets/img/OldDesignShop_MushroomSpringMorel-240x240.jpg
tweet:  https://twitter.com/julianhyde/status/1237266305054470144
---

I am pleased to announce Morel release 0.2. Since release 0.1 we have
renamed the project from 'smlj' to 'Morel', made major improvements to
the type system, and continued our relational extensions.

Some highlights of release 0.2:
* Functions and values can have polymorphic types, inferred as part of a
  [Hindley-Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system);
* Relational expressions may now include a `group` clause, so you can
  evaluate aggregate queries (similar to SQL `GROUP BY`);
* [Foreign values](https://github.com/julianhyde/morel/issues/5) allow external
  data, such as the contents of a JDBC database, to be handled as if it is in
  memory;
* Add built-in functions based on the
  [`String`](http://sml-family.org/Basis/string.html) and
  [`List`](http://sml-family.org/Basis/list.html) structures
  in the Standard ML basis library;
* [Postfix field reference syntax](https://github.com/julianhyde/morel/issues/9)
  makes Morel more familiar to SQL users;
* Add [Morel language reference](docs/reference.md).

For more information, see the
[release notes](https://github.com/julianhyde/morel/blob/master/HISTORY.md#02--2020-03-10).

If you have comments, please reply on Twitter:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>
