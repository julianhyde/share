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

import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;

/** Tests for Play. */
public class PlayTest {
  private void check(String in, String out) {
    final List<String> args = Arrays.asList("--delay", "1");
    final StringWriter w = new StringWriter();
    try {
      new Play(new StringReader(in), w, args).run();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    assertThat(w.toString(), is(out));
  }

  @Test void testEmpty() {
    check("", "");
  }

  @Test void testNewline() {
    check("\n", "\n");
  }

  @Test void testSimple() {
    check("abc\ndef\n", "abc\ndef\n");
  }

  /** Tests that a comment does not cause a newline in the output. */
  @Test void testComment() {
    String in = "abc\n"
        + "# a comment\n"
        + "def\n";
    String out = "abcdef\n";
    check(in, out);
  }

  @Test void testComment2() {
    String in = "abc\n"
        + "\n"
        + "# a comment\n"
        + "def\n";
    String out = "abc\n"
        + "def\n";
    check(in, out);
  }

  @Test void testComment3() {
    String in = "abc\n"
        + "# a comment\n"
        + "\n"
        + "def\n";
    String out = "abc\n"
        + "def\n";
    check(in, out);
  }

  @Test void testCommentAtStart() {
    String in = "#delay 2\n"
        + "a\n"
        + "# a comment\n"
        + "b\n";
    String out = "ab\n";
    check(in, out);
  }

  @Test void testNoTrailingNewline() {
    String in = "#delay 2\n"
        + "a\n"
        + "# a comment\n"
        + "b";
    String out = "ab";
    check(in, out);
  }

}

// End PlayTest.java
