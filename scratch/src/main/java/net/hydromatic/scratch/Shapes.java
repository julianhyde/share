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
import java.util.Arrays;
import java.util.List;

import static net.hydromatic.scratch.Utilities.cons;

/** Utilities for shapes. */
public class Shapes {
  /** Rotations and reflections. */
  enum Transformation {
    IDENTITY(1, 0, 0, 1),
    R90(0, 1, -1, 0),
    R180(-1, 0, 0, -1),
    R270(0, -1, 1, 0),
    M(-1, 0, 0, 1),
    MR90(0, -1, -1, 0),
    MR180(1, 0, 0, -1),
    MR270(0, 1, 1, 0);

    final int x0;
    final int x1;
    final int y0;
    final int y1;

    Transformation(int x0, int x1, int y0, int y1) {
      this.x0 = x0;
      this.x1 = x1;
      this.y0 = y0;
      this.y1 = y1;
    }
  }

  /** Function that accepts (x, y) coordinates. */
  @FunctionalInterface
  interface IntIntConsumer {
    void accept(int x, int y);
  }

  /**
   * Shape consisting of five squares.
   *
   * <pre>
   * []
   * []        [][]
   * []  [][]    []    [][]            [][][]
   * []  [][]    []  [][]        [][]    []
   * []  []      []    []    [][][]      []
   *
   * O   P     Q     R       S         T
   *
   *                                   []
   *         []      []        []    [][]  [][]
   * []  []  []      [][]    [][][]    []    []
   * [][][]  [][][]    [][]    []      []    [][]
   *
   * U       V       W       X       Y     Z
   * </pre>
   */
  public enum Pentonimo {
    O(0, 0, 0, 1, 0, 2, 0, 3, 0, 4),
    P(0, 0, 0, 1, 1, 1, 0, 2, 1, 2),
    Q(1, 0, 1, 1, 2, 1, 3, 0, 3, 1),
    R(1, 0, 0, 1, 1, 1, 1, 2, 2, 2),
    S(0, 0, 1, 0, 2, 0, 2, 1, 3, 1),
    T(1, 0, 1, 1, 0, 2, 1, 2, 2, 2),
    U(0, 0, 1, 0, 2, 0, 1, 0, 1, 2),
    V(0, 0, 1, 0, 2, 0, 0, 1, 0, 2),
    W(1, 0, 2, 0, 0, 1, 1, 1, 0, 2),
    X(1, 0, 0, 1, 1, 1, 2, 1, 1, 2),
    Y(1, 0, 1, 1, 0, 2, 1, 2, 1, 3),
    Z(1, 0, 2, 0, 1, 1, 0, 2, 1, 2);

    final Shape shape;

    Pentonimo(int... coords) {
      this.shape = new Shape(coords);
      assert coords.length == 10;
    }
  }

  /** Can format a collection of shapes to a fixed-size rectangular board. */
  static class Printer {
    private final int width;
    private final int height;
    private final int[] regions;
    private final Shape board;

    /** Creates a Printer. */
    Printer(int width, int height) {
      this.width = width;
      this.height = height;
      this.regions = new int[(width + 1) * (height + 2) + 1];
      this.board = Shape.rectangle(width, height);
    }

    /** Prints a collection of shapes in a rectangle. */
    void print(PrintWriter w, List<Shape> shapes) {
      Arrays.fill(regions, -2); // outside
      int region = 0;
      for (Shape entry : cons(board, shapes)) {
        final int r = region++;
        final IntIntConsumer consumer = (x, y) -> {
          regions[(y + 1) * (width + 1) + (x + 1)] = r;
        };
        entry.print(consumer);
      }
      //
      // Consider width = 2, height = 2
      //
      // column  0 2
      //       -1 1 3
      // row -1 +-+-+
      //      0 | | |
      //      1 +-+-+
      //      2 | | |
      //      3 +-+-+
      //
      // x = column / 2, y = row / 2
      //
      // For example, the top-left cell has row = 0, column = 0,
      // x = 0, y = 0, c = 4. The cell to its right has c = 5.
      // The cell below it has c = 7.
      //
      for (int row = -1; row < height * 2; row++) {
        for (int column = -1; column < width * 2; column++) {
          w.append(text(row, column));
        }
        w.println();
      }
    }

    private String text(int row, int column) {
      final int x = (column + 3) / 2;
      final int y = (row + 3) / 2;
      final int c = x + (width + 1) * y;
      final int current;
      final int left;
      final int above;
      final int aboveLeft;
      switch ((row & 1) << 1 | (column & 1)) {
      case 3: // corner (odd row, odd column)
        current = regions[c];
        left = regions[c - 1];
        above = regions[c - (width + 1)];
        aboveLeft = regions[c - (width + 1) - 1];
        if (current == left && above == aboveLeft) {
          if (current == above) {
            return " "; // all 4 cells are the same
          } else {
            return "-";
          }
        }
        if (current == above && left == aboveLeft) {
          return "|";
        }
        return "+";
      case 1: // vertical wall (even row, odd column)
        current = regions[c];
        left = regions[c - 1];
        return current == left ? " " : "|";
      case 2: // horizontal wall (odd row, even column)
        current = regions[c];
        above = regions[c - (width + 1)];
        return current == above ? "  " : "--";
      default: // middle of a room
        return "  ";
      }
    }
  }
}

// End Shapes.java
