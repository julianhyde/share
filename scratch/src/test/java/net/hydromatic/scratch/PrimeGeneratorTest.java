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

import org.junit.jupiter.api.Test;

import java.util.Iterator;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * Tests for prime number generator.
 */
public class PrimeGeneratorTest {
  @Test void testPrimeGenerator() {
    final PrimeGenerator g = new PrimeGenerator();
    assertThat(g.next(), is(2));
    assertThat(g.next(), is(3));
    assertThat(g.next(), is(5));
    assertThat(g.next(), is(7));
    assertThat(g.next(), is(11));
    assertThat(g.next(), is(13));
    assertThat(g.next(), is(17));
    for (int i  = 0; i < 10_000; i++) {
      g.next();
    }
    assertThat(g.next(), is(104_803));
  }

  @Test void testPythagoreanTupleGenerator() {
    try (PythagoreanTripleGenerator g =
             new PythagoreanTripleGenerator(1, 15)) {
      final Iterator<PythagoreanTripleGenerator.Triple> iterator =
          g.iterator();
      assertThat(iterator.hasNext(), is(true));
      assertThat(iterator.next().toString(), is("3 4 5"));
      assertThat(iterator.hasNext(), is(true));
      assertThat(iterator.next().toString(), is("5 12 13"));
      assertThat(iterator.hasNext(), is(false));
    }
  }

  @Test void testPythagoreanTupleGenerator2() {
    try (PythagoreanTripleGenerator g =
             new PythagoreanTripleGenerator(10, Integer.MAX_VALUE)) {
      final Iterator<PythagoreanTripleGenerator.Triple> iterator =
          g.iterator();
      assertThat(iterator.hasNext(), is(true));
      assertThat(iterator.next().toString(), is("3 4 5"));
      assertThat(iterator.hasNext(), is(true));
      assertThat(iterator.next().toString(), is("5 12 13"));
      assertThat(iterator.hasNext(), is(true));
    }
  }
}

// End PrimeGeneratorTest.java
