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

import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Random;

import static java.lang.System.out;

/**
 * Ecology simulator.
 */
public class Ecology {
  private final PrintWriter fileWriter;

  public static void main(String[] args) throws IOException {
    new Ecology().run();
  }

  public Ecology() throws IOException {
    fileWriter =
        new PrintWriter(
            new FileWriter("/home/jhyde/open1/share/scratch/my.dat"));
  }

  void print(String s) {
    fileWriter.println(s);
    out.println(s);
  }

  void run() {
    final Random random = new Random(1);
    double rabbits = 100;
    double foxes = 100;
    final int food = 1000;
    final double rabbitBirthRate = 0.05;
    final double rabbitCaughtRate = 0.001;
    final double foxCatchRate = 0.00025;
    final double foxDeathRate = 0.02;
    final int iterationCount = 800;
    for (int t = 0; t < iterationCount; t++) {
      print(t + " " + (int) rabbits + " " + (int) foxes);
      rabbits += rabbits * rabbitBirthRate;
      rabbits -= rabbits * foxes * rabbitCaughtRate;
      foxes += rabbits * foxes * foxCatchRate;
      foxes -= foxes * foxDeathRate;
      /*
      for (int i = 0, n = rabbits; i < n; i++) {
        if (rabbits < food && random.nextInt(5) == 0) {
          ++rabbits; // rabbit has a baby rabbit
        }
      }
      for (int i = 0, n = foxes; i < n; i++) {
        if (random.nextInt(rabbits) < 3) {
          if (foxes > 1 && random.nextInt(3) == 0) {
            --foxes; // fox cannot catch rabbit, so dies
          }
        } else {
          if (rabbits > 1 && random.nextInt(3) == 0) {
            --rabbits;
          }
        }
        if (random.nextInt(3) == 0) {
          ++foxes; // fox has a baby fox
        }
      }
      */
    }
    fileWriter.close();
  }

  private double rate(int rabbitBirth) {
    return 1d + 1d / rabbitBirth;
  }
}


