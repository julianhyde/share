/*
 * Licensed to Julian Hyde under one or more contributor license
 * agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership. Julian Hyde
 * licenses this file to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.hydromatic.scratch;

import org.junit.Test;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

/** Tests for Cover. */
public class CoverTest {
  @Test public void testCoverInitialState() {
    final int[][] rows = {
        {0, 0, 1, 0, 1, 0, 0},
        {1, 0, 0, 1, 0, 0, 1},
        {0, 1, 1, 0, 0, 1, 0},
        {1, 0, 0, 1, 0, 1, 0},
        {0, 1, 0, 0, 0, 0, 1},
        {0, 0, 0, 1, 1, 0, 1},
    };
    final List<String> names =
        Arrays.asList("a", "b", "c", "d", "e", "f", "g");
    final Cover state = new Cover(names, rows.length,
        (i, x) -> rows[x][i] == 1);
    checkCoverInitialState(state);
  }

  @Test public void testCoverInitialState2() {
    final String[] options = {
        "c e", "a d g", "b c f", "a d f", "b g", "d e g"
    };
    checkCoverInitialState(Cover.create(Arrays.asList(options)));
  }

  void checkCoverInitialState(Cover state) {
    final StringWriter sw = new StringWriter();
    final PrintWriter w = new PrintWriter(sw);
    state.dump(w);
    final String expected = "i name left right\n"
        + "0 null    7    1\n"
        + "1    a    0    2\n"
        + "2    b    1    3\n"
        + "3    c    2    4\n"
        + "4    d    3    5\n"
        + "5    e    4    6\n"
        + "6    f    5    7\n"
        + "7    g    6    0\n"
        + "\n"
        + " x top   up down\n"
        + " 0   0    0    0\n"
        + " 1   2   20   12\n"
        + " 2   2   24   16\n"
        + " 3   2   17    9\n"
        + " 4   3   27   13\n"
        + " 5   2   28   10\n"
        + " 6   2   22   18\n"
        + " 7   3   29   14\n"
        + " 8   0    0   10\n"
        + " 9   3    3   17\n"
        + "10   5    5   28\n"
        + "11  -1    0    0\n"
        + "12   1    1   20\n"
        + "13   4    4   21\n"
        + "14   7    7   25\n"
        + "15  -2    0    0\n"
        + "16   2    2   24\n"
        + "17   3    9    3\n"
        + "18   6    6   22\n"
        + "19  -3    0    0\n"
        + "20   1   12    1\n"
        + "21   4   13   27\n"
        + "22   6   18    6\n"
        + "23  -4    0    0\n"
        + "24   2   16    2\n"
        + "25   7   14   29\n"
        + "26  -5    0    0\n"
        + "27   4   21    4\n"
        + "28   5   10    5\n"
        + "29   7   25    7\n"
        + "\n";
    assertThat(sw.toString(), is(expected));
    w.close();
  }

  @Test public void testPrintShapes() {
    final StringWriter sw = new StringWriter();
    final List<Shape> map = new ArrayList<>();
    map.add(Shapes.Pentonimo.V.shape);
    map.add(
        Shapes.Pentonimo.R.shape.transform(Shapes.Transformation.R90, 5, 3));
    final Shapes.Printer printer = new Shapes.Printer(10, 6);
    printer.print(new PrintWriter(sw), map);
    final String expected = ""
        + "+--------+--------------------+\n"
        + "|        |                    |\n"
        + "|  +-----+                    |\n"
        + "|  |                          |\n"
        + "|  |                          |\n"
        + "|  |                          |\n"
        + "+--+        +--+              |\n"
        + "|           |  |              |\n"
        + "|        +--+  +--+           |\n"
        + "|        |        |           |\n"
        + "|        |  +-----+           |\n"
        + "|        |  |                 |\n"
        + "+--------+--+-----------------+\n";
    assertThat(sw.toString(), is(expected));
  }

  @Test public void testSolvePentonimo() {
    PentonimoCoverSolver.solve(new PrintWriter(System.out));
  }
}

// End CoverTest.java
