Reflecting on how we build, maintain and verify complex software
systems, I think we place too much emphasis on formal proof and too
little on purpose.

This was crystalized recently listening to the philosopher Daniel
Dennett. Dennett says that there are three ways to look at a system:
physical (what its constituent parts are doing), design (what is its
purpose), and intentional (its beliefs and desires), and that the
design and intentional stances allow us to predict its behavior far
more cheaply than tracking its physics.

When I am reasoning about a system — how to fix a bug, or how to
change it to add a feature, while leaving it in better shape than I
found it — I am almost always using Dennett’s design stance.

It has been observed that you cannot build a complex software system
all at once. You have to start with smaller working systems and scale
them up. Why is that?

I think it is because it allows the pieces to evolve a purpose. (When
I say "piece" I am thinking of a chunk of code — say a dozen lines
that I added to fix a particular bug — but the reasoning also applies
to code on all scales.) As the system evolves, the pieces become
related to other pieces that have a related purpose. And so the
structure takes shape.

While the system works on day one, it takes several years to fulfills
its promise. (There is rarely a single moment when the creator says,
"It’s finished. This is what I wanted to build.") And in the mean
time, many engineers, thinking tasks they consider mere "maintenance",
are actually doing the work of finishing the system.

At the start, I contrasted formal proof with purpose. What is the
relationship between them?

Proof works best on small systems. 

A theorem is concise, generally a couple of lines (even if it is
supported by a few paragraphs of definitions and a few pages of
proof).

Small programs can be verified by stating a single theorem. (For
example, given a list, QuickSort returns a list with the same elements
in sorted order.)

But is it possible to reason about a larger system by combining
theorems? A formal specification is just that — a collection of
theorems — but formal specifications have limited use. They are no fun
to read, no fun to write, and may not even describe the user’s actual
requirements.

Of my college math courses, I enjoyed most the ones whose lecturer
motivated the theorems with a little history: "In 1821, Cauchy
formalized the concept of a limit, with a goal of providing a rigorous
foundation for Newton’s calculus".

This reveals that while a mathematical theory is a complex systems
just like software. It is not a static collection theorems and proofs;
it evolves and grows. Each theorem was introduced for a reason — it
was postulated, and then proved, because a mathematician had a
particular purpose in mind. Sometimes the motivation is outside of the
theory, say to enable a particular calculation in physics or
economics.

Quite often, the purpose is all you need. "I need a way to calculate
the hypotenuse of a right-angled triangle given its other sides" is
enough for a competent mathematician to create, and prove, Pythagoras’
theorem.

To understand, maintain, and grow a complex software system, you need
to know the purpose of the code in that system. Archaeology can help
us understand that purpose -- do "git blame" on that region of code,
look at which bug was being fixed, and which tests were added at the
same time. We also understand purpose, we ask "Why?". Judea Pearl
[reference "Book of Why"] tells us that to distinguish causation from
mere correlation we must perform experiments - perturb the system, and
see what changes. Every engineer knows that to understand a line of
code, you comment it out and see which tests break.

My conception of a complex software system is three things: literate
code, tests, and a literate commit history. Proof is part of that, in
the form of tests that make assertions, comments, and code that
enforces pre- and post-conditions. But purpose is more important. It
pervades the system, and the system cannot be understood without it.

[References and metaphors: a city grows over time, continuing to
function as a city, parts become specialized, superseded or
replaced. Gall’s Law from John Gall’s Systematics (1975). Stewart
Brand, "How Buildings Learn".]
