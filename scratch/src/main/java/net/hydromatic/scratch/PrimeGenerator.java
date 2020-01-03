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

import java.util.Iterator;
import java.util.PriorityQueue;

/**
 * Prime number generator.
 */
public class PrimeGenerator implements Iterator<Integer> {
  final PriorityQueue<Seq> sequences = new PriorityQueue<>();
  int candidate = 2;

  /** Creates a PrimeGenerator. */
  public PrimeGenerator() {
    sequences.add(new SquareSeq(2));
  }

  @Override public boolean hasNext() {
    return true;
  }

  /** {@inheritDoc}
   *
   * <p>Returns the next prime number. */
  public Integer next() {
    while (true) {
      Seq seq = sequences.poll();
      int c = seq.current;
      if (c <= candidate) {
        seq.next();
      }
      sequences.add(seq);
      if (c > candidate) {
        return candidate++;
      } else if (c >= candidate) {
        ++candidate;
      }
    }
  }

  /** Command line. */
  public static void main(String[] args) {
    for (int i : iterable()) {
      System.out.println(i);
    }
  }

  /** Returns an iterable over the prime numbers. */
  public static Iterable<Integer> iterable() {
    return PrimeGenerator::new;
  }

  /** Sequence that generates squares of prime numbers, and adds a
   * {@link PrimeSeq} to the priority queue each time it hits one. */
  class SquareSeq extends Seq {
    int prime;

    SquareSeq(int prime) {
      super(prime * prime);
      this.prime = prime;
    }

    @Override public String toString() {
      return "square: prime = " + prime + ", current = " + current;
    }

    @Override void next() {
      sequences.add(new PrimeSeq(prime));
      current += 2 * prime++ + 1;
    }
  }

  abstract static class Seq implements Comparable<Seq> {
    int current;

    Seq(int current) {
      this.current = current;
    }

    abstract void next();

    public int compareTo(Seq o) {
      return Integer.compare(current, o.current);
    }
  }

  /** Sequence that generates multiples of a given prime. */
  static class PrimeSeq extends Seq {
    final int prime;

    PrimeSeq(int prime) {
      super(prime * (prime + 1));
      this.prime = prime;
    }

    @Override public String toString() {
      return "prime: prime = " + prime + ", current = " + current;
    }

    void next() {
      current += prime;
    }
  }
}

// End PrimeGenerator.java
