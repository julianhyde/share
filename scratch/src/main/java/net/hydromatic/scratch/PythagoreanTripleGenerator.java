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

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import net.hydromatic.scratch.PythagoreanTripleGenerator.Triple;

/**
 * Generates triples of the form (a, b, c)
 * where a, b, and c are positive integers,
 * and a<sup>2</sup> + b<sup>2</sup> = c<sup>2</sup>.
 */
public class PythagoreanTripleGenerator
    implements Iterable<Triple>, AutoCloseable {
  private final int workerCount;
  private final ExecutorService executorService;
  private final int max;

  /** Creates a PythagoreanTripleGenerator. */
  PythagoreanTripleGenerator(int workerCount, int max) {
    this.workerCount = workerCount;
    this.executorService =
        new ThreadPoolExecutor(workerCount, workerCount * 2, 1,
            TimeUnit.SECONDS,
            new ArrayBlockingQueue<>(workerCount * workerCount));
    this.max = max;
  }

  public void close() {
    executorService.shutdown();
  }

  public Iterator<Triple> iterator() {
    return new PythagoreanTripleIterator();
  }

  private class PythagoreanTripleIterator implements Iterator<Triple> {
    final ArrayDeque<Triple> triples = new ArrayDeque<>();
    final ArrayDeque<Future<List<Triple>>> futures = new ArrayDeque<>();
    private int i;

    PythagoreanTripleIterator() {
      i = 5;
      for (int j = 0; j < workerCount; j++) {
        addTask();
      }
    }

    public boolean hasNext() {
      while (true) {
        final Triple triple = triples.peek();
        if (triple != null) {
          return true;
        }
        final Future<List<Triple>> future;
        try {
          future = futures.pop();
        } catch (NoSuchElementException e) {
          return false;
        }
        addTask();
        try {
          final List<Triple> list = future.get();
          triples.addAll(list);
        } catch (InterruptedException e) {
          throw new RuntimeException(e);
        } catch (ExecutionException e) {
          throw new RuntimeException(e.getCause());
        }
      }
    }

    public Triple next() {
      return triples.pop();
    }

    private boolean addTask() {
      if (i < max) {
        final int prevI = i;
        i += 2;
        return futures.add(
            executorService.submit(() -> generateTriples(prevI)));
      }
      return false;
    }
  }

  private static List<Triple> generateTriples(int c) {
    final List<Triple> list = new ArrayList<>();
    for (int b = 4; b < c; b++) {
      for (int a = 3 + (b % 2); a < b; a++) {
        final int a2b2 = a * a + b * b;
        final int c2 = c * c;
        if (a2b2 >= c2) {
          if (a2b2 == c2 && gcd(a, b) == 1) {
            list.add(new Triple(a, b, c));
          }
          break;
        }
      }
    }
    return list;
  }

  /** Command line. */
  public static void main(String[] args) {
    final int workerCount = 160;
    final int max = 20_000;
    try (PythagoreanTripleGenerator iterable = iterable(workerCount, max)) {
      for (Triple triple : iterable) {
        System.out.println(triple);
      }
    }
  }

  /** Returns an iterable over the prime numbers.
   *
   * @param workerCount Number of worker threads
   * @param max Maximum hypotenuse
   */
  public static PythagoreanTripleGenerator iterable(int workerCount, int max) {
    return new PythagoreanTripleGenerator(workerCount, max);
  }

  /** Returns the greatest common divisor of {@code e} and {@code f};
   * 1 means they are co-prime. */
  static int gcd(int e, int f) {
    if (e == 0) {
      if (f == 0) {
        return 1; // special case: gcd(0, 0) is 1
      } else {
        return f; // special case: gcd(0, x) is x
      }
    } else if (e < 0) {
      return gcd(-e, f);
    } else if (f < 0) {
      return gcd(e, -f);
    } else if (e == f) {
      return e;
    } else if (e < f) {
      return gcd(e, f - e);
    } else {
      return gcd(e - f, f);
    }
  }

  /** Three integers. */
  static class Triple {
    final int a;
    final int b;
    final int c;

    Triple(final int a, final int b, final int c) {
      this.a = a;
      this.b = b;
      this.c = c;
    }

    @Override public String toString() {
      return a + " " + b + " " + c;
    }
  }
}

// End PythagoreanTripleGenerator.java
