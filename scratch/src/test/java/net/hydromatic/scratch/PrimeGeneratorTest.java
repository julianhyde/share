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

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

/**
 * Tests for prime number generator.
 */
public class PrimeGeneratorTest {
  @Test public void testPrimeGenerator() {
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
}

// End PrimeGeneratorTest.java
