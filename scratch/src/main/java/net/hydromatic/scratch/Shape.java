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

import java.util.Objects;

/** Shape consisting of a connected set of tiles with (x, y) coordinates. */
class Shape {
  private final int[] coords;

  Shape(int... coords) {
    this.coords = Objects.requireNonNull(coords).clone();
    assert coords.length % 2 == 0;
  }

  static Shape rectangle(int width, int height) {
    int[] coords = new int[2 * width * height];
    int b = 0;
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
        coords[b++] = x;
        coords[b++] = y;
      }
    }
    return new Shape(coords);
  }

  Shape transform(Shapes.Transformation transformation, int offsetX,
      int offsetY) {
    int[] newCoords = new int[coords.length];
    for (int i = 0; i < newCoords.length; i += 2) {
      int x = coords[i];
      int y = coords[i + 1];
      int xNew = x * transformation.x0 + y * transformation.y0 + offsetX;
      int yNew = x * transformation.x1 + y * transformation.y1 + offsetY;
      newCoords[i] = xNew;
      newCoords[i + 1] = yNew;
    }
    return new Shape(newCoords);
  }

  /** Returns whether this shape lies entirely within a given rectangle. */
  boolean within(int x0, int y0, int x1, int y1) {
    for (int i = 0; i < coords.length; i += 2) {
      final int x = coords[i];
      final int y = coords[i + 1];
      if (x < x0 || x >= x1 || y < y0 || y >= y1) {
        return false;
      }
    }
    return true;
  }

  void print(Shapes.IntIntConsumer consumer) {
    for (int i = 0; i < coords.length; i += 2) {
      consumer.accept(coords[i], coords[i + 1]);
    }
  }

}

// End Shape.java
