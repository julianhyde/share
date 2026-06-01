---
layout: post
title:  "Expressing package queries in Morel"
date:   2026-05-29 12:00:00 -0800
author: Julian Hyde
image:  /assets/img/OldDesignShop_MushroomSpringMorel-240x240.jpg
tweet:  https://x.com/julianhyde/status/XXXXXXXXXXXXXXXXX
---

Morel is more than a query language. Because it is a functional
language with relational algebra built in, it can express problems
that a query language can't. It already supports constraint
programming and logic programming, and *package queries* are
another example.

Consider the following problem: given a set of recipes, find the
combination of three gluten-free recipes that total between 2,000 and
2,500 calories and has less saturated fat than all other such
combinations. That constraint is not about any single row; it is about
a *combination* of rows, and no `where` clause can express it. It is
an optimization problem in the shape of a query.

A 2016 paper,
[Scalable Package Queries in Relational Database Systems][paper],
introduced *package queries*, along with PaQL, an extension to SQL.
Given a table, a package query finds a combination of its rows &mdash;
perhaps with repetition &mdash; that together satisfy a set of
constraints while maximizing or minimizing an objective. They
generalize relational queries, and can express integer linear
programming (ILP) problems such as bin-packing. The paper's running
example plans a day of meals:

```sql
SELECT PACKAGE(R) AS P
FROM Recipes R REPEAT 0
WHERE R.gluten = 'free'
SUCH THAT COUNT(P.*) = 3
  AND SUM(P.kcal) BETWEEN 2.0 AND 2.5
MINIMIZE SUM(P.saturated_fat)
```

The recipes must be gluten-free, there must be exactly three, and
`REPEAT 0` says that none may occur more than once. (The paper
measures calories in thousands, so `2.0` means 2,000 kcal.)

Plain SQL has no concise, declarative way to write this. When a
package has a fixed cardinality &mdash; as here, with `COUNT(P.*) = 3`
&mdash; you can fake it with a three-way self-join, and the general
case can be encoded by using recursion to build a powerset table and
testing each subset; but neither is declarative, optimizable, or
efficient. (The paper makes both points.) In Morel it is
straightforward:

<!-- morel silent
fun subLists list =
    List.foldr
      (fn (x, acc) => acc @ List.map (fn s => x :: s) acc)
      [[]]
      list;
> val subLists = fn : 'a list -> 'a list list
fun subBags bag =
    Bag.map Bag.fromList (Bag.fromList (subLists (Bag.toList bag)));
> val subBags = fn : 'a bag -> 'a bag bag
Sys.set ("output", "tabular");
> val it = () : unit
-->
<!-- morel skip
from p in subBags (from r in recipes where r.gluten = "free")
  where Bag.length p = 3
    andalso sum (from r in p yield r.cal) >= 2000
    andalso sum (from r in p yield r.cal) <= 2500
  order sum (from r in p yield r.satFat)
  take 1;
-->

<div class="code-block">
<div class="code-input"><span class="kr">from</span> <span class="nv">p</span> <span class="kr">in</span> <span class="n">subBags</span> <span class="p">(</span><span class="kr">from</span> <span class="nv">r</span> <span class="kr">in</span> <span class="n">recipes</span> <span class="kr">where</span> <span class="nn">r</span><span class="p">.</span><span class="n">gluten</span> <span class="p">=</span> <span class="s2">"free"</span><span class="p">)</span>
  <span class="kr">where</span> <span class="nn">Bag</span><span class="p">.</span><span class="n">length</span> <span class="n">p</span> <span class="p">=</span> <span class="mi">3</span>
    <span class="kr">andalso</span> <span class="n">sum</span> <span class="p">(</span><span class="kr">from</span> <span class="nv">r</span> <span class="kr">in</span> <span class="n">p</span> <span class="kr">yield</span> <span class="nn">r</span><span class="p">.</span><span class="n">cal</span><span class="p">)</span> <span class="o">&gt;</span><span class="p">=</span> <span class="mi">2000</span>
    <span class="kr">andalso</span> <span class="n">sum</span> <span class="p">(</span><span class="kr">from</span> <span class="nv">r</span> <span class="kr">in</span> <span class="n">p</span> <span class="kr">yield</span> <span class="nn">r</span><span class="p">.</span><span class="n">cal</span><span class="p">)</span> <span class="o">&lt;</span><span class="p">=</span> <span class="mi">2500</span>
  <span class="kr">order</span> <span class="n">sum</span> <span class="p">(</span><span class="kr">from</span> <span class="nv">r</span> <span class="kr">in</span> <span class="n">p</span> <span class="kr">yield</span> <span class="nn">r</span><span class="p">.</span><span class="n">satFat</span><span class="p">)</span>
  <span class="kr">take</span> <span class="mi">1</span><span class="p">;</span></div>
</div>

You can make it more concise by computing the count, calories, and fat
up front, and using the new [range-list syntax][morel-372] for the
predicate on calories. Over a small set of recipes:

<!-- morel no-output
val recipes = bag [
  {name = "Grilled Salmon Bowl", gluten = "free",    cal = 700, satFat = 4},
  {name = "Quinoa Veg Stir-fry", gluten = "free",    cal = 550, satFat = 2},
  {name = "Lentil Curry",        gluten = "free",    cal = 600, satFat = 3},
  {name = "Chicken Rice Plate",  gluten = "free",    cal = 850, satFat = 6},
  {name = "Steak & Potatoes",    gluten = "free",    cal = 950, satFat = 12},
  {name = "Avocado Tofu Salad",  gluten = "free",    cal = 500, satFat = 5},
  {name = "Cheese Omelette",     gluten = "free",    cal = 650, satFat = 10},
  {name = "Pasta Carbonara",     gluten = "present", cal = 800, satFat = 14},
  {name = "Pizza Margherita",    gluten = "present", cal = 900, satFat = 11}];
> cal gluten  name                satFat
> --- ------- ------------------- ------
> 700 free    Grilled Salmon Bowl      4
> 550 free    Quinoa Veg Stir-fry      2
> 600 free    Lentil Curry             3
> 850 free    Chicken Rice Plate       6
> 950 free    Steak & Potatoes        12
> 500 free    Avocado Tofu Salad       5
> 650 free    Cheese Omelette         10
> 800 present Pasta Carbonara         14
> 900 present Pizza Margherita        11
>
> val recipes : {cal:int, gluten:string, name:string, satFat:int} bag
-->

<div class="code-block">
<div class="code-input"><span class="kr">val</span> <span class="nv">recipes</span> <span class="p">=</span> <span class="n">bag</span> <span class="p">[</span>
  <span class="p">{</span><span class="n">name</span> <span class="p">=</span> <span class="s2">"Grilled Salmon Bowl"</span><span class="p">,</span> <span class="n">gluten</span> <span class="p">=</span> <span class="s2">"free"</span><span class="p">,</span>    <span class="n">cal</span> <span class="p">=</span> <span class="mi">700</span><span class="p">,</span> <span class="n">satFat</span> <span class="p">=</span> <span class="mi">4</span><span class="p">},</span>
  <span class="p">{</span><span class="n">name</span> <span class="p">=</span> <span class="s2">"Quinoa Veg Stir-fry"</span><span class="p">,</span> <span class="n">gluten</span> <span class="p">=</span> <span class="s2">"free"</span><span class="p">,</span>    <span class="n">cal</span> <span class="p">=</span> <span class="mi">550</span><span class="p">,</span> <span class="n">satFat</span> <span class="p">=</span> <span class="mi">2</span><span class="p">},</span>
  <span class="p">{</span><span class="n">name</span> <span class="p">=</span> <span class="s2">"Lentil Curry"</span><span class="p">,</span>        <span class="n">gluten</span> <span class="p">=</span> <span class="s2">"free"</span><span class="p">,</span>    <span class="n">cal</span> <span class="p">=</span> <span class="mi">600</span><span class="p">,</span> <span class="n">satFat</span> <span class="p">=</span> <span class="mi">3</span><span class="p">},</span>
  <span class="p">{</span><span class="n">name</span> <span class="p">=</span> <span class="s2">"Chicken Rice Plate"</span><span class="p">,</span>  <span class="n">gluten</span> <span class="p">=</span> <span class="s2">"free"</span><span class="p">,</span>    <span class="n">cal</span> <span class="p">=</span> <span class="mi">850</span><span class="p">,</span> <span class="n">satFat</span> <span class="p">=</span> <span class="mi">6</span><span class="p">},</span>
  <span class="p">{</span><span class="n">name</span> <span class="p">=</span> <span class="s2">"Steak &amp; Potatoes"</span><span class="p">,</span>    <span class="n">gluten</span> <span class="p">=</span> <span class="s2">"free"</span><span class="p">,</span>    <span class="n">cal</span> <span class="p">=</span> <span class="mi">950</span><span class="p">,</span> <span class="n">satFat</span> <span class="p">=</span> <span class="mi">12</span><span class="p">},</span>
  <span class="p">{</span><span class="n">name</span> <span class="p">=</span> <span class="s2">"Avocado Tofu Salad"</span><span class="p">,</span>  <span class="n">gluten</span> <span class="p">=</span> <span class="s2">"free"</span><span class="p">,</span>    <span class="n">cal</span> <span class="p">=</span> <span class="mi">500</span><span class="p">,</span> <span class="n">satFat</span> <span class="p">=</span> <span class="mi">5</span><span class="p">},</span>
  <span class="p">{</span><span class="n">name</span> <span class="p">=</span> <span class="s2">"Cheese Omelette"</span><span class="p">,</span>     <span class="n">gluten</span> <span class="p">=</span> <span class="s2">"free"</span><span class="p">,</span>    <span class="n">cal</span> <span class="p">=</span> <span class="mi">650</span><span class="p">,</span> <span class="n">satFat</span> <span class="p">=</span> <span class="mi">10</span><span class="p">},</span>
  <span class="p">{</span><span class="n">name</span> <span class="p">=</span> <span class="s2">"Pasta Carbonara"</span><span class="p">,</span>     <span class="n">gluten</span> <span class="p">=</span> <span class="s2">"present"</span><span class="p">,</span> <span class="n">cal</span> <span class="p">=</span> <span class="mi">800</span><span class="p">,</span> <span class="n">satFat</span> <span class="p">=</span> <span class="mi">14</span><span class="p">},</span>
  <span class="p">{</span><span class="n">name</span> <span class="p">=</span> <span class="s2">"Pizza Margherita"</span><span class="p">,</span>    <span class="n">gluten</span> <span class="p">=</span> <span class="s2">"present"</span><span class="p">,</span> <span class="n">cal</span> <span class="p">=</span> <span class="mi">900</span><span class="p">,</span> <span class="n">satFat</span> <span class="p">=</span> <span class="mi">11</span><span class="p">}];</span></div>
</div>

the more concise query returns:

<!-- morel
from p in subBags (from r in recipes where r.gluten = "free"),
    {c, cal, fat} =
      (from r in p
        compute {c = count over (),
                 cal = sum over r.cal,
                 fat = sum over r.satFat})
  where c = 3 andalso cal elem [2000 .. 2500]
  order fat
  take 1;
> c cal  fat p
>            cal gluten name                satFat
> - ---- --- --- ------ ------------------- ------
> 3 2000  11 550 free   Quinoa Veg Stir-fry      2
>            600 free   Lentil Curry             3
>            850 free   Chicken Rice Plate       6
>
> val it : {c:int, cal:int, fat:int, p:{cal:int, gluten:string, name:string, satFat:int} bag} list
-->

<div class="code-block">
<div class="code-input"><span class="kr">from</span> <span class="nv">p</span> <span class="kr">in</span> <span class="n">subBags</span> <span class="p">(</span><span class="kr">from</span> <span class="nv">r</span> <span class="kr">in</span> <span class="n">recipes</span> <span class="kr">where</span> <span class="nn">r</span><span class="p">.</span><span class="n">gluten</span> <span class="p">=</span> <span class="s2">"free"</span><span class="p">),</span>
    <span class="p">{</span><span class="n">c</span><span class="p">,</span> <span class="n">cal</span><span class="p">,</span> <span class="n">fat</span><span class="p">}</span> <span class="p">=</span>
      <span class="p">(</span><span class="kr">from</span> <span class="nv">r</span> <span class="kr">in</span> <span class="n">p</span>
        <span class="kr">compute</span> <span class="p">{</span><span class="n">c</span> <span class="p">=</span> <span class="n">count</span> <span class="kr">over</span> <span class="p">(),</span>
                 <span class="n">cal</span> <span class="p">=</span> <span class="n">sum</span> <span class="kr">over</span> <span class="nn">r</span><span class="p">.</span><span class="n">cal</span><span class="p">,</span>
                 <span class="n">fat</span> <span class="p">=</span> <span class="n">sum</span> <span class="kr">over</span> <span class="nn">r</span><span class="p">.</span><span class="n">satFat</span><span class="p">})</span>
  <span class="kr">where</span> <span class="n">c</span> <span class="p">=</span> <span class="mi">3</span> <span class="kr">andalso</span> <span class="n">cal</span> <span class="kr">elem</span> <span class="p">[</span><span class="mi">2000</span> <span class="p">..</span> <span class="mi">2500</span><span class="p">]</span>
  <span class="kr">order</span> <span class="n">fat</span>
  <span class="kr">take</span> <span class="mi">1</span><span class="p">;</span></div>
<div class="code-output">c cal  fat p
           cal gluten name                satFat
- ---- --- --- ------ ------------------- ------
3 2000  11 550 free   Quinoa Veg Stir-fry      2
           600 free   Lentil Curry             3
           850 free   Chicken Rice Plate       6

val it : {c:int, cal:int, fat:int, p:{cal:int, gluten:string, name:string, satFat:int} bag} list</div>
</div>

The optimizer does not simply pick the lowest-fat recipes. The two
non-gluten-free recipes are filtered out before any packing. And the
three lowest-fat recipes overall &mdash; Salmon, Quinoa, and Lentil,
at 9 g of saturated fat &mdash; total only 1,850 calories, below the
floor, so the constraint forces the optimizer to drop the salmon in
favor of the higher-calorie Chicken Rice Plate, landing exactly on
2,000 calories at 11 g. Minimizing the objective naively would give
the wrong answer; the constraint and the objective pull against each
other.

That `compute` clause is worth a second look. The row it builds
&mdash; the package `p` together with `c` , `cal` , and `fat` &mdash;
is the *feature vector* of a candidate package. Each of `c` , `cal` ,
and `fat` is a linear function of which tuples we chose (and how many
of each), which is exactly the form an integer linear program wants.

The one thing missing from Morel is the `subBags` function. That's
easy to write:

<!-- morel
fun subLists list =
    List.foldr
      (fn (x, acc) => acc @ List.map (fn s => x :: s) acc)
      [[]]
      list;
> val subLists = fn : 'a list -> 'a list list

fun subBags bag =
    Bag.map Bag.fromList (Bag.fromList (subLists (Bag.toList bag)));
> val subBags = fn : 'a bag -> 'a bag bag
-->

<div class="code-block">
<div class="code-input"><span class="kr">fun</span> <span class="nf">subLists</span> <span class="n">list</span> <span class="p">=</span>
    <span class="nn">List</span><span class="p">.</span><span class="n">foldr</span>
      <span class="p">(</span><span class="kr">fn</span> <span class="p">(</span><span class="n">x</span><span class="p">,</span> <span class="n">acc</span><span class="p">)</span> <span class="o">=&gt;</span> <span class="n">acc</span> @ <span class="nn">List</span><span class="p">.</span><span class="n">map</span> <span class="p">(</span><span class="kr">fn</span> <span class="n">s</span> <span class="o">=&gt;</span> <span class="n">x</span> <span class="o">::</span> <span class="n">s</span><span class="p">)</span> <span class="n">acc</span><span class="p">)</span>
      <span class="p">[[]]</span>
      <span class="n">list</span><span class="p">;</span></div>
<div class="code-output">val subLists = fn : 'a list -&gt; 'a list list</div>
<div class="code-input">
<span class="kr">fun</span> <span class="nf">subBags</span> <span class="n">bag</span> <span class="p">=</span>
    <span class="nn">Bag</span><span class="p">.</span><span class="n">map</span> <span class="nn">Bag</span><span class="p">.</span><span class="n">fromList</span> <span class="p">(</span><span class="nn">Bag</span><span class="p">.</span><span class="n">fromList</span> <span class="p">(</span><span class="n">subLists</span> <span class="p">(</span><span class="nn">Bag</span><span class="p">.</span><span class="n">toList</span> <span class="n">bag</span><span class="p">)));</span></div>
<div class="code-output">val subBags = fn : 'a bag -&gt; 'a bag bag</div>
</div>

To be at par with PaQL, `subBags` needs an extra parameter, `k` . If
`k` is positive, each package can contain up to `k + 1` occurrences of
each tuple in the source. Modifying `subBags` is left as an exercise
for the reader (or your favorite agent).

Still, we don't recommend actually executing `subBags` . *N* recipes
generate 2^*N* packages (even more if `k` is positive), so the
algorithm quickly becomes intractable. We need a smarter strategy:
make `subBags` a built-in operator, and have rules recognize it in a
query and send that part of the query to a specialized solver.

The feature vector is what gets sent to the solver. The translation
rules turn the `compute` row into an integer linear program: one
integer variable per source tuple (how many times it is chosen), the
aggregates `c` , `cal` , and `fat` into linear functions of those
variables, and the `where` and `order` clauses into constraints and an
objective. Because every quantity in the query is linear in that
choice vector, the translation is mechanical. The moment a constraint
*isn't* linear &mdash; a probabilistic "the loss stays below *X* with
high probability" bound, or a requirement that the chosen tuples form
a contiguous region &mdash; you are outside what this machinery can
lower, and you need a different solver.

One such solver is presented in the paper:
<span style="font-variant: small-caps">SketchRefine</span>, an
approximate divide-and-conquer technique for answering package queries
efficiently on large datasets.

Morel expresses package queries cleanly, but expressing a problem is
not the same as executing it. Package queries are NP-hard in general
&mdash; the paper proves that PaQL is at least as expressive as
integer linear programming, and knapsack, a special case, is
NP-complete &mdash; so no query language is going to make them cheap.
That is exactly why it helps to have them inside a general-purpose
language. The combination of rows that ordinary SQL can't even
describe is, in Morel, just another query: the irreducibly hard part
goes to a solver, and the ordinary relational work around it does not
have to.

Constraint programming and logic programming both run on the same
`from` and `where` constructs, made runnable by predicate inversion.
Coloring a map so that no two neighboring states share a color is a
`from` over color variables whose `where` is a conjunction of
inequalities; a small optimization &mdash; say, how many banana and
chocolate cakes to bake from a limited pantry, an example from
[Basic Modelling in MiniZinc][minizinc] &mdash; is the same, with an
`order` on the objective. And because a predicate can recurse, Morel
runs Datalog; an
[earlier article]({% post_url 2026-03-09-datalog-in-morel %})
walked through transitive closure and the classic Joe's bar rules.
Package queries add combinatorial optimization over packages of rows.

That seam is the point. A single language, type system, and
environment can express a hybrid problem end to end &mdash; pull
records from a database, feed them into a solver for a graph algorithm
or an arithmetic optimization, and consume the result &mdash; with no
marshalling of data sets between half a dozen little languages. And
because the relational structure stays visible to the compiler, Morel
can push down filters and materialize intermediate results before
handing the hard core to a specialized solver.

[paper]: https://www.vldb.org/pvldb/vol9/p576-brucato.pdf
[morel-372]: https://github.com/hydromatic/morel/issues/372
[minizinc]: https://docs.minizinc.dev/en/stable/modelling.html

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
