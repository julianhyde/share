/*
// Licensed to Julian Hyde under one or more contributor license
// agreements. See the NOTICE file distributed with this work for
// additional information regarding copyright ownership.
//
// Julian Hyde licenses this file to you under the Apache License,
// Version 2.0 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at:
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
*/
package net.hydromatic.game2k;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;

/**
 * Test for {@link Main}.
 */
public class MainTest {
  @Test public void testBoardToString() {
    assertThat(new Main.Board(0).toString(), equalTo(
        "    .    .    .    .\n"
        + "    .    .    .    .\n"
        + "    .    .    .    .\n"
        + "    .    .    .    .\n"));
  }

  @Test public void testPlace() {
    final Main.Board board = new Main.Board(0);
    assertThat(board.isEmpty(0), is(true));
    assertThat(board.isEmpty(2), is(true));
    Main.Board board2 = board.place(2, 1);
    assertThat(board2.toString(), equalTo(
        "    .    .    2    .\n"
            + "    .    .    .    .\n"
            + "    .    .    .    .\n"
            + "    .    .    .    .\n"));
    assertThat(board2.shortString(), equalTo(
        "[. . 1 . . . . . . . . . . . . .]"));
    assertThat(board2.isEmpty(0), is(true));
    assertThat(board2.isEmpty(2), is(false));
  }

  @Test public void testDown() {
    final Main.Board board = new Main.Board(0).place(2, 1).place(3, 1).down();

  }
}
