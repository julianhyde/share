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

/**
 * Simulation of 2048 game.
 */
public class Main {
  /** Creates a Main. */
  private Main() {}

  /** Main method. */
  public static void main(String[] args) {
  }

  /** Board is 16 positions, each with 12 states (4 bits). */
  static class Board {
    final long positions;

    static final String[] LONGS = {
      "    .",
      "    2",
      "    4",
      "    8",
      "   16",
      "   32",
      "   64",
      "  128",
      "  256",
      "  512",
      " 1024",
      " 2048",
    };

    static final String[] HEXES = {
      ".",
      "1", // 1
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "A",
      "B", // 2048
    };

    Board(long state) {
      this.positions = state;
    }

    /** Returns a value between 0 and 15. 0 means unoccupied; 1 means 2;
     * 2 means 4; 3 means 8; 11 means 2048. */
    private int pos(int i) {
      return (int) ((positions >> (i * 4)) & 0xF);
    }

    @Override public String toString() {
      return LONGS[pos(0)]
          + LONGS[pos(1)]
          + LONGS[pos(2)]
          + LONGS[pos(3)]
          + "\n"
          + LONGS[pos(4)]
          + LONGS[pos(5)]
          + LONGS[pos(6)]
          + LONGS[pos(7)]
          + "\n"
          + LONGS[pos(8)]
          + LONGS[pos(9)]
          + LONGS[pos(10)]
          + LONGS[pos(11)]
          + "\n"
          + LONGS[pos(12)]
          + LONGS[pos(13)]
          + LONGS[pos(14)]
          + LONGS[pos(15)]
          + "\n";
    }

    /** Places {@code piece} on board at {@code pos}. Does not check whether
     * the position is empty. */
    public Board place(int pos, int piece) {
      assert piece > 0;
      assert piece < 13;
      assert isEmpty(pos);
      return new Board(positions | (piece << (pos * 4)));
    }

    /** Returns whether position {@code pos} is empty on the board. */
    public boolean isEmpty(int pos) {
      return pos(pos) == 0;
    }

    public String shortString() {
      final StringBuilder buf = new StringBuilder("[");
      for (int i = 0; i < 16; i++) {
        if (i > 0) {
          buf.append(' ');
        }
        buf.append(HEXES[pos(i)]);
      }
      return buf.toString();
    }

    public Board down() {
      long board = 0;
      return new Board(board);
    }
  }

}

// End Main.java
