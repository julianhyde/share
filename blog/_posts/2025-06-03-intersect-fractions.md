---
layout: post
title:  "INTERSECT ALL, EXCEPT ALL, and the arithmetic of fractions"
date:   2025-06-03 12:00:00 -0800
author: Julian Hyde
image:  /assets/img/OldDesignShop_MushroomSpringMorel-240x240.jpg
tweet:  https://x.com/julianhyde/status/1930104410375630901
---

SQL's `INTERSECT ALL` and `EXCEPT ALL` operators rarely get attention,
but they elegantly solve a classic math problem. The problem is
computing the **greatest common divisor (GCD)** and **least common
multiple (LCM)** of two integers, using the prime factors of those
integers.  In this post we show how to do this using `intersect` and
`except`, Morel's equivalent of `INTERSECT ALL` and `EXCEPT ALL`.

SQL's set operators (`UNION`, `INTERSECT`, and `EXCEPT`) have set and
multiset variants.  The multiset variants retain duplicates and use
the `ALL` keyword; the set variants discard duplicates, and you can
use the optional `DISTINCT` keyword if you want to be explicit.

Morel has [just added](https://github.com/hydromatic/morel/issues/253)
`union`, `intersect` and `except` query steps, achieving parity
with both Standard SQL and
[GoogleSQL's pipe syntax](https://cloud.google.com/bigquery/docs/reference/standard-sql/pipe-syntax#union_pipe_operator).
(This post is about multiset mode, which retains duplicate values; to
use the set mode, which discards duplicates and is far more common,
use the `distinct` keyword, for example `intersect distinct`.)

Using these steps, we can compute GCD and LCM. The queries are even more
concise because Morel queries over integer values do not require
column names.

## Adding fractions

Remember how -- probably in middle school -- you learned how to add
two fractions, and to reduce a fraction to its lowest terms?

Suppose you need to add 5/36 and 7/120. First, find the least
common multiple (LCM) of their denominators (36 and 120).

Next, convert each fraction to an equivalent fraction with the LCM
(360) as the denominator.
* For 5/36: Multiply the numerator and denominator by 10
  (since 36 * 10 = 360).
* For 7/120: Multiply the numerator and denominator by 3
  (since 120 * 3 = 360).

Last, add the fractions.

```
  5      7        5 * 10    7 * 3        50      21       71
---- + -----  =  ------- + -------  =  ----- + -----  =  -----
 36     120      36 * 10   120 * 3      360     360       360
```

## Computing GCD and LCM

To compute the GCD of two numbers, you start by finding their prime
factors.  Prime factors can be repeated, so these are multisets, not
sets.  Let's find the GCD of 36 and 120.

* 36 is 2<sup>2</sup> * 3<sup>2</sup>, so has factors [2, 2, 3, 3]
* 120 is 2<sup>3</sup> * 3 * 5, so has factors [2, 2, 2, 3, 5]

Where there are factors in common, we take the lower repetition count.
Taking the minimum count for each common factor -- two 2s, one 3, and
no 5s -- the GCD is therefore 2<sup>2</sup> * 3, which is 12.

The crucial step of the algorithm is to combine two multisets and take
the minimum repetition count; that is exactly what `intersect` does.

The LCM is similar, but takes the higher repetition count.
This can be achieved by taking the union of both factor multisets,
then subtracting their intersection. Here's why: The union gives us
all factors from both numbers, but it adds the counts together. Since
we want the maximum count (not the sum), we subtract the intersection,
which contains the overlapping factors we double-counted.
The LCM is therefore 2<sup>3</sup> * 3<sup>2</sup> * 5, which is 360.

## Using Morel to compute LCM and GCD

To convert this algorithm to code, we will need three things:
 * a `factorize` function splits the numbers into multisets of prime
   factors;
 * the `intersect` step combines the multisets;
 * a `product` function converts the multisets back to a number.

Here are the `factorize` and `product` functions.

{% highlight sml %}
fun factorize n =
  let
    fun factorize' n d =
      if n < d then [] else
      if n mod d = 0 then d :: (factorize' (n div d) d)
      else factorize' n (d + 1)
  in
    factorize' n 2
  end;
(*[> val factorize = fn : int -> int list]*)

fun product [] = 1
  | product (x::xs) = x * (product xs);
(*[> val product = fn : int list -> int]*)
{% endhighlight %}

Here's how they work:
{% highlight sml %}
factorize 120;
(*[> val it = [2, 2, 2, 3, 5] : int list]*)
product (factorize 120);
(*[> val it = 120 : int]*)
{% endhighlight %}

So, we can compute GCD like this:

{% highlight sml %}
fun gcd (m, n) =
  from f in factorize m
    intersect factorize n
    compute product;
(*[> val gcd = fn : int * int -> int]*)
{% endhighlight %}

The last step uses `compute` because `product` fulfills Morel's only
criterion to be an aggregate function: its argument is a collection
of values. (At least one SQL dialect agrees with us, and has a
[PRODUCT](https://duckdb.org/docs/stable/sql/functions/aggregates#productarg)
aggregate function.)

LCM can be computed from GCD:
{% highlight sml %}
fun lcm (m, n) =
  (m * n) div gcd (m, n);
(*[> val lcm = fn : int * int -> int]*)
{% endhighlight %}

But, as we discussed above, it can also be computed directly using
`union`, `except` and `intersect`:

{% highlight sml %}
fun lcm' (m, n) =
  let
    val m_factors = factorize m
    val n_factors = factorize n
  in
    from f in m_factors
      union (n_factors)
      except (from f in m_factors
        intersect n_factors)
    compute product
  end;
(*[> val lcm' = fn : int * int -> int]*)
{% endhighlight %}

Let's test them:

{% highlight sml %}
gcd (36, 120);
(*[> val it = 12 : int]*)
lcm (36, 120);
(*[> val it = 360 : int]*)
lcm' (36, 120);
(*[> val it = 360 : int]*)
{% endhighlight %}

## Conclusion

The `intersect`, `except`, and `union` steps neatly solve the problem
of computing GCD and LCM because they handle repeated factors in
exactly the way we need.

These steps will be available shortly in Morel release 0.7.

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
