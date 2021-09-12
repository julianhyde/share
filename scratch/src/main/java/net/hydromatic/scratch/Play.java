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

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.Arrays;
import java.util.List;

import static java.lang.Long.parseLong;

/** Plays keystrokes. */
public class Play {
  private final Reader r;
  private final Writer w;
  private final List<String> args;
  /** System time at which the previous character was emitted. */
  private long last;
  /** The number of times that {@link #read()} has been called. */
  private int readCount = 0;
  /** The number of times that {@link #write(int, long)} has been called. */
  private int writeCount = 0;
  /** The value of {@link #readCount} the last time that we read a newline.
   * Initialized to 0 so that we start in a state as if we've just read a
   * newline. */
  private int previousNewlineAt = 0;

  Play(Reader r, Writer w, List<String> args) {
    this.r = r;
    this.w = w;
    this.args = args;
  }

  /** Command-line entry point. */
  public static void main(String[] args) {
    try (Reader r = new InputStreamReader(System.in);
         Writer w = new OutputStreamWriter(System.out)) {
      new Play(r, w, Arrays.asList(args)).run();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  void run() throws IOException {
    // Example file:
    //
    //   # a comment
    //   #delay 250
    //   abc
    //   ##
    //   de gh
    //   #delay 2000
    //   ijk lmnop

    final StringBuilder b = new StringBuilder();
    long delay = 1000; // default 1 character per second
    if (args.size() >= 2
        && args.get(0).equals("--delay")) {
      delay = parseLong(args.get(1));
    }
    last = System.currentTimeMillis();
    while (true) {
      int c = read();
      while (c == 10) {
        // We've read a newline. We can print it if what is coming next
        // is not a '#'.
        c = read();
        if (c != '#') {
          write(10, delay);
        }
      }
      if (c < 0) {
        break;
      }
      if (c == '#' && previousNewlineAt == readCount - 1) {
        // A line starting with '#'. Read the rest of the line.
        b.appendCodePoint(c);
        while (true) {
          c = read();
          if (c < 0) {
            c = 10; // end of input; pretend we saw a newline
          }
          if (c == 10) {
            break; // newline
          }
          b.appendCodePoint(c);
        }
        final String line = b.toString();
        b.setLength(0);
        if (line.startsWith("# ")) {
          // a comment; ignore the line
        } else if (line.equals("##")) {
          // output a '#' character
          write('#', delay);
        } else if (line.matches("#pause [0-9]+")) {
          final long pause = parseLong(line.substring("#pause ".length()));
          last += pause;
        } else if (line.matches("#delay [0-9]+")) {
          delay = parseLong(line.substring("#delay ".length()));
        } else {
          throw new RuntimeException("unknown line: " + line);
        }
      } else {
        write(c, delay);
      }
    }
  }

  private int read() throws IOException {
    ++readCount;
    final int c = r.read();
    if (c == 10) {
      previousNewlineAt = readCount;
    }
    return c;
  }

  /** Writes a character at a given delay after the previous character. */
  private void write(int c, long delay) throws IOException {
    final long next = last + delay;
    final long diff = next - System.currentTimeMillis();
    if (diff > 0) {
      try {
        Thread.sleep(diff);
      } catch (InterruptedException e) {
        throw new RuntimeException(e);
      }
    }
    last = next;
    writeCount++;
    w.write(c);
    w.flush();
  }
}
