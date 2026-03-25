---
layout: post
title:  "What Humans Do When Agents Write Code"
date:   2026-03-23 12:00:00 -0800
author: Julian Hyde
image:  /assets/img/tools.jpg
tweet:  https://x.com/julianhyde/status/XXXXXXXXXXXXXXXXX
---

Agents can write code. So what's left for humans to do? The answer,
I'll argue, is architecture &mdash; and specifically, the design of
tools.  That turns out to be hard, important, and distinctly human
work.

![Tools on a workbench]({{ "/assets/img/tools.jpg" | relative_url }})

An agent can turn out code at much faster than any human developer,
but like a human, it has trouble grasping the big picture. A 100K-line
system is too large for a single context, but if it is divided into
smaller pieces, the agent can write and maintain them one at a time.

This is where the human developer &mdash; the architect &mdash; can
help.  I define architecture as a *strategy for organizing
complexity*, but usually involve divides the system into components
with clean interfaces.  An agent (or human) working on one component
needs only a brief, accurate picture of the others — not their source.
Components may take the form of microservices, APIs, libraries, but
the simplest model is the Unix tool: a program with a man page.

## Unix tools

Unix tools are effective components because they have a simple
interface contract (the man page), are easy to invoke (from the
shell), and compose with other tools (in pipelines and scripts).












































<!-- SECTION 2: Architecture as the answer
Divide the system into components with clean interfaces. An agent (or
human) working on one component needs only a brief, accurate picture
of the others — not their source. The right model for this is the
Unix tool: a program with a man page. The man page IS the interface
contract. If your component can't be described in a man page, it
isn't a tool yet.
-->

<!-- SECTION 3: Power through language
`grep` has enormous functionality, but you invoke it concisely because
regexp is a powerful declarative language. This is the pattern: a good
tool comes with a small declarative language. The return of tools is
the return of DSLs.
-->

<!-- SECTION 4: darn as a concrete example
`darn` (https://github.com/hydromatic/morel/issues/345) processes
Markdown, but it also invokes the Morel kernel to validate and execute
embedded code fragments. It has a clear man page, does one thing, and
uses Morel as its language for code-block evaluation. Calcite's
relational algebra interface is another example of a tool with a clean
declarative language at its interface.
-->

<!-- SECTION 5: Designing tools is hard — which makes it good human work
Choosing the right abstraction, picking the right language, writing the
man page (which is the spec) — this requires taste and experience.
The difficulty is the point: it's exactly the kind of high-leverage
work that humans should be doing.
-->

<!-- SECTION 6: The toolchain as a form of architecture
Lint rules that enforce patterns — e.g. enum variants must be sorted —
are a different kind of tool. Even if you don't read agent-generated
code, the compiler rejects code that violates the invariant. This
forces the agent toward consistent patterns and prevents edits that
can't be merged. Humans encode architectural decisions into the
toolchain so they don't have to review every line.
-->

<!-- CONCLUSION
The measure of a well-designed component is whether you can write its
man page. If you can, an agent can use it. Beyond individual tools,
the toolchain itself encodes architecture: a lint rule that rejects
unsorted enum variants doesn't just catch a style nit — it forces the
agent to adopt a pattern that will merge cleanly. In the agentic era,
humans set the constraints; agents work within them.
-->

If you have comments, please reply on
[Bluesky @julianhyde.bsky.social](https://bsky.app/profile/julianhyde.bsky.social)
or Twitter:

<div data_dnt="true">
{% twitter page.tweet limit=5 hide_media=true %}
</div>

<!--
This article
[has been updated](https://github.com/julianhyde/share/commits/main/blog/{{ page.path }}).
-->
