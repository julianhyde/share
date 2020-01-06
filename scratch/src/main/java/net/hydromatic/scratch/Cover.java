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
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Consumer;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

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

  /** The original list of options. */
  private final List<List<Integer>> options;

  /** Creates a Cover from a list of strings.
   *
   * <p>For example,
   *
   * <pre>
   *   create(Arrays.asList("a b", "b c"))
   * </pre>
   *
   * <p>creates a Cover with 3 items named "a", "b", "c"
   * and two options (0, 1) and (1, 2).
   */
  static Cover create(List<String> optionList) {
    return create(optionList, nameList ->
      new ArrayList<>(new TreeSet<>(nameList)));
  }

  static Cover create(List<String> optionList,
      UnaryOperator<List<String>> namesTransform) {
    final Set<String> nameSet = new LinkedHashSet<>();
    for (String option : optionList) {
      nameSet.addAll(Arrays.asList(option.split(" ")));
    }
    final List<String> nameList =
        namesTransform.apply(new ArrayList<>(nameSet));
    return new Cover(nameList, optionList.size(),
        (item, option) -> Arrays.asList(optionList.get(option).split(" "))
            .contains(nameList.get(item)));
  }

  /** Creates a Cover. */
  Cover(List<String> nameList, int rowCount, Element element) {
    this.options = new AbstractList<List<Integer>>() {
      @Override public int size() {
        return rowCount;
      }

      @Override public List<Integer> get(int index) {
        final List<Integer> items = new ArrayList<>();
        for (int i = 0; i < names.length; i++) {
          if (element.test(i, index)) {
            items.add(i);
          }
        }
        return items;
      }
    };

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

  /** Returns the options. */
  List<List<Integer>> getOptions() {
    return options;
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

  public void solve(Consumer<Solution> solutionConsumer) {
    // 1. Initialize
    int n = names.length - 1; // number of items
    int z = tops.length - 1; // last spacer address
    int level = 0;
    final int[] cellStack = new int[names.length];

    while (true) {
      // 2. Enter level "l"
      if (rightLinks[0] == 0) {
        // All items have been covered. We have a solution.
        final List<Integer> optionOrdinals = new ArrayList<>();
        for (int i = 0; i < level; i++) {
          final int cell = cellStack[i];
          optionOrdinals.add(cellOption(cell));
        }
        solutionConsumer.accept(() -> optionOrdinals);
      }

      // 8. Leave level "l"
      if (level == 0) {
        return;
      }
      --level;

      // 6. Try again.

      // 5. Try x[level].
      if (false) {
        // 7. Backtrack

        // goto 8
      }
    }
  }

  /** Returns the ordinal of the option that a given cell belongs to. */
  private int cellOption(int cell) {
    while (tops[cell] > 0) {
      --cell;
    }
    return -tops[cell];
  }

  public String optionToString(int optionOrdinal) {
    return options.get(optionOrdinal)
        .stream()
        .map(item -> names[item])
        .collect(Collectors.joining(" "));
  }

  /** Returns whether a given option contains a particular item. */
  @FunctionalInterface
  interface Element {
    boolean test(int item, int option);
  }

  /** A solution. */
  interface Solution {
    /** Returns the ordinals of the options that formed a solutio, i.e. an
     * exact cover of the set. */
    List<Integer> options();
  }
}

// End Cover.java
