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

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

/** Solves the Pentonimo problem using the exact-cover solver. */
public final class PentonimoCoverSolver {
  private PentonimoCoverSolver() {
  }

  public static void solve(PrintWriter w) {
    // The items are the 60 squares of the 10x6 board, named "0_0", ... "0_9",
    // ... "5_9". Each option is a placement of a shape, perhaps transformed
    // (rotated, reflected, translated) on the board.
    final int width = 10;
    final int height = 6;
    final List<String> optionList = new ArrayList<>();
    final Set<String> options = new HashSet<>();
    final Shapes.Pentonimo[] pentonimos = Shapes.Pentonimo.values();
    final Shapes.Transformation[] transformations =
        Shapes.Transformation.values();
    final StringBuilder buf = new StringBuilder();
    for (Shapes.Pentonimo pentonimo : pentonimos) {
      for (Shapes.Transformation transformation : transformations) {
        for (int y = 0; y < height; y++) {
          for (int x = 0; x < width; x++) {
            final Shape shape = pentonimo.shape.transform(transformation, x, y);
            if (shape.within(0, 0, width, height)) {
              buf.setLength(0);
              shape.print((x1, y1) ->
                  buf.append(x1).append('_').append(y1).append(' '));
              // Add the name of the pentomino as a "ghost square", so that
              // each solution uses each pentomino exactly once.
              buf.append(pentonimo.name().toLowerCase(Locale.ROOT));
              final String option = buf.toString();
              if (options.add(option)) {
                optionList.add(option);
              }
            }
          }
        }
      }
    }
    w.println(optionList);
    final Cover cover = Cover.create(optionList);
    cover.dump(w);
    final Shapes.Printer printer = new Shapes.Printer(width, height);
    final List<Shape> shapes = new ArrayList<>();
    final int[] coords = new int[10];
    cover.solve(solution -> {
      for (int optionOrdinal : solution.options()) {
        final String option = optionList.get(optionOrdinal);
        int c = 0;
        // Split an option into coordinates;
        // e.g. "0_1 2_3 r" becomes [0, 1, 2, 3].
        for (String itemName : option.split(" ")) {
          if (itemName.contains("_")) {
            coords[c++] = Integer.parseInt(itemName.substring(0, 1));
            coords[c++] = Integer.parseInt(itemName.substring(2, 3));
          }
          shapes.add(new Shape(coords));
        }
      }
      printer.print(w, shapes);
      w.println();
      shapes.clear();
    });
  }
}

// End PentonimoCoverSolver.java
