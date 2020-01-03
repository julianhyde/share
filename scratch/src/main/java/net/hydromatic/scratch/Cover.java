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
import java.util.List;

import static net.hydromatic.scratch.Utilities.cons;

/**
 * Exact cover solver.
 *
 * <p>Based on Knuth's DLX (Algorithm X implemented using Dancing Links).
 */
public class Cover {
  // The following arrays have one entry per item.
  final String[] names;
  final int[] leftLinks;
  final int[] rightLinks;

  // The following arrays have one entry per cell
  // (header row, followed by items)
  final int[] tops;
  final int[] upLinks;
  final int[] downLinks;

  /** Creates a Cover. */
  Cover(List<String> nameList, int rowCount, Element element) {
    final int columnCount = nameList.size();
    names = cons(null, nameList).toArray(new String[0]);
    leftLinks = new int[names.length];
    rightLinks = new int[names.length];
    for (int i = 0; i < names.length; i++) {
      leftLinks[i] = (i - 1 + names.length) % names.length;
      rightLinks[i] = (i + 1 + names.length) % names.length;
    }
    int elementCount = 0;
    for (int i = 0; i < columnCount; i++) {
      for (int x = 0; x < rowCount; x++) {
        if (element.test(i, x)) {
          ++elementCount;
        }
      }
    }
    tops = new int[1 + columnCount + elementCount + rowCount];
    upLinks = new int[tops.length];
    downLinks = new int[tops.length];
    for (int i = 0; i < columnCount; i++) {
      upLinks[i + 1] = i + 1;
      downLinks[i + 1] = i + 1;
    }
    int c = 1 + columnCount;
    for (int x = 0; x < rowCount; x++) {
      // spacer cell
      tops[c] = -x;
      ++c;
      for (int i = 0; i < columnCount; i++) {
        if (element.test(i, x)) {
          insert(i + 1, c);
          ++c;
        }
      }
    }
    // For each spacer cell, set 'up' to the first cell of the previous item,
    // and 'down' to the last cell of the next item.
    int first = 0;
    for (c = columnCount; c < tops.length; c++) {
      if (tops[c] == 0) {
        upLinks[c] = first;
      }
      if (tops[c - 1] <= 0) {
        first = c;
      }
    }
    int last = 0;
    for (c = tops.length - 2; c >= columnCount; c--) {
      if (tops[c] == 0) {
        downLinks[c] = last;
      }
      if (tops[c + 1] <= 0) {
        last = c;
      }
    }
  }

  void insert(int i, int c) {
    final int last = upLinks[i];
    upLinks[c] = last;
    downLinks[c] = i;
    downLinks[last] = c;
    upLinks[i] = c;
    tops[c] = i;
    ++tops[i];
  }

  void dump(PrintWriter w) {
    w.println("i name left right");
    for (int i = 0; i < names.length; i++) {
      w.format("%d %4s %4d %4d\n", i, names[i], leftLinks[i], rightLinks[i]);
    }
    w.println();
    w.format("%2s %3s %4s %4s\n", "x", "top", "up", "down");
    for (int x = 0; x < tops.length; x++) {
      w.format("%2d %3d %4d %4d\n", x, tops[x], upLinks[x], downLinks[x]);
    }
    w.println();
  }

  @FunctionalInterface
  interface Element {
    boolean test(int i, int x);
  }
}

// End Cover.java
