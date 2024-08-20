share/data
==========

A compendium of data sets.

# Getting started

```
$ ./sqlline
$ sqlline version 1.9.0
sqlline> !connect jdbc:hsqldb:res:scott SCOTT TIGER
0: jdbc:hsqldb:res:scott> select * from dept;
+--------+----------------+---------------+
| DEPTNO |     DNAME      |      LOC      |
+--------+----------------+---------------+
| 10     | ACCOUNTING     | NEW YORK      |
| 20     | RESEARCH       | DALLAS        |
| 30     | SALES          | CHICAGO       |
| 40     | OPERATIONS     | BOSTON        |
+--------+----------------+---------------+
4 rows selected (0.002 seconds)
```

# Data sets

Hsqldb:
* [chinook](https://github.com/julianhyde/chinook-data-hsqldb)
* [flight](https://github.com/julianhyde/flight-data-hsqldb)
* [foodmart](https://github.com/julianhyde/foodmart-data-hsqldb)
* [look](https://github.com/hydromatic/look-data-hsqldb)
* [scott](https://github.com/julianhyde/scott-data-hsqldb)
* [steelwheels](https://github.com/julianhyde/steelwheels-data-hsqldb)


```
> !connect jdbc:hsqldb:res:chinook sa ''
> select count(*) from Track;

> !connect jdbc:hsqldb:res:flight FLIGHT TIGER
> select * from "aircraft_types";
> select count(*) from "airports";

> !connect jdbc:hsqldb:res:foodmart sa ''
> select count(*) from "foodmart"."employee";

> !connect jdbc:hsqldb:res:look looker looker
> select count(*) from "look"."users";

> !connect jdbc:hsqldb:res:scott SCOTT TIGER
> select * from emp;

> !connect jdbc:hsqldb:res:steelwheels steelwheels ''
> select * from "steelwheels"."employees";
```

