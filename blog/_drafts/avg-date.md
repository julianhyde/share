---
layout: post
title:  "Averaging dates"
date:   2112-12-21 12:21:00 -0800
author: Julian Hyde
image: /assets/img/P2019515_j6d_IMG_0383_lb.JPG
tweet: https://twitter.com/julianhyde/status/1229847506403479553
---

No dialect of SQL (todo: footnote 'that I am aware of') allows you
average date values.  For example, on the
[scott](https://gist.github.com/julianhyde/70609f4de92ccabec41b9a9df0689d23)
schema, Postgres

```sql
SELECT avg(hiredate)
FROM emp
```

gives

```
ERROR:  function avg(date) does not exist
LINE 2: SELECT avg(hiredate)
               ^
HINT:  No function matches the given name and argument types. You might need to add explicit type casts.
```

If you doubt whether "average hiredate" of employees is a valid
question, think instead about their tenure -- the number of days since
they were hired.  Clearly tenure is well-defined, because tenure is a
number, and you can add up the values and divide by the count.  But
when you have the average tenure, you can subtract it from today's
date and find the average hiredate.

```sql
SELECT current_date
     + ROUND(AVG(hiredate - current_date))::int AS avg_hiredate
FROM emp;

 avg_hiredate 
--------------
 1982-05-03
(1 row)
```

You don't have to use today's date.  Whichever origin you pick, the
query will give the same answer.

This, by the way, is why you can't easily tell what epoch a system
uses to store dates.  Linux uses January 1st, 1970; Postgres uses
January 1st, 2000; and Excel effectively uses
[December 31st, 1899](https://www.reddit.com/r/AskStatistics/comments/7uk40z).

Why do dates behave this way? There is no zero date, and `hiredate *
2` is meaningless.  What *is* meaningful is the difference of two
dates (a number of days) and a date plus a number of days.  A set with
that structure -- you can subtract two points to get a vector, and add
a vector to a point, but not add two points -- is an
[affine space](https://en.wikipedia.org/wiki/Affine_space).

Temperatures are an affine space: the average of 0&deg;C and 100&deg;C
is 50&deg;C, and in Fahrenheit the average of 32&deg; and 212&deg; is
122&deg;, which is the same temperature -- but adding two temperatures
is nonsense.

Pointers in the C and C++ programming languages work the same way.
Given an array defined by its endpoints, pointers `pStart` and `pEnd`,
can you find the midpoint?  It is not the arithmetic mean:

```c
(pStart + pEnd) / 2
```

That expression does not compile, because you cannot add two pointers.
But you can subtract them:

```c
pStart + (pEnd - pStart) / 2
```

Another sign you are dealing with an affine space is when subtracting
two values yields a different type.  (The difference between two SQL
`DATE` values is an `INTERVAL`; the difference between two C `void *`
values is a `size_t`.)  The difference is a vector, not a point.
Vectors have a zero value, which is the value you get when you
subtract a point from itself.

The geospatial folks have their act together, as usual.  The PostGIS
[`ST_Centroid`](https://postgis.net/docs/ST_Centroid.html) function
lets you compute the center of mass -- "average" generalized to 2D or
3D -- of a collection of points, lines, shapes, or other geometric
objects.  Once again, the representation of geometries requires an
origin, but the choice of origin does not affect the calculated
centroid.

## Conclusion

Dates, temperatures, points, and pointers all have well-defined
averages.

Because affine sets have no origin, you cannot add values, and
therefore the "add them up and divide by n" formula for arithmetic
mean does not work.

You can average them by picking an arbitrary origin and averaging the
distances to that origin.

So, why we can't average DATE, DATETIME and TIMESTAMP values in SQL?

If you have comments, please reply on Twitter:

{% twitter page.tweet maxwidth=400 limit=5 hide_media=true data_cards=hidden %}
