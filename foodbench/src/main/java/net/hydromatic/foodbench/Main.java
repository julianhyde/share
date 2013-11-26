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
package net.hydromatic.foodbench;

import mondrian.test.data.FoodMartQuery;

import com.fasterxml.jackson.databind.ObjectMapper;

import com.google.common.collect.Range;
import com.google.common.collect.RangeSet;
import com.google.common.collect.TreeRangeSet;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.sql.*;
import java.util.List;
import java.util.Map;

/**
 * Command-line app that runs FoodBench.
 */
public class Main {
  /** Optiq model. */
  public static final String OPTIQ_MODEL =
      "{~\n"
      + "  version: '1.0',\n"
      + "  defaultSchema: 'FOODMART_CLONE',\n"
      + "  schemas: [\n"
      + "    {\n"
      + "      name: 'FOODMART_CLONE',\n"
      + "      type: 'custom',\n"
      + "      factory: 'net.hydromatic.optiq.impl.clone.CloneSchema$Factory',\n"
      + "      operand: {\n"
      + "        jdbcDriver: 'com.mysql.jdbc.Driver',\n"
      + "        jdbcUrl: 'jdbc:mysql://localhost/foodmart',\n"
      + "        jdbcUser: 'foodmart',\n"
      + "        jdbcPassword: 'foodmart'\n"
      + "      }\n"
      + "    }\n"
      + "  ]\n"
      + "}\n";

    /** Main method. */
  public static void main(String[] args) {
    try {
      Main main = new Main();

      // E.g. Main -Dtest.ids=10,20-30,-23
      RangeSet<Integer> idSet = parseInts(System.getProperty("test.ids"));

      switch (1) {
      case 0:
        main.run(
            idSet,
            "jdbc:mysql://localhost?user=foodmart&password=foodmart",
            "foodmart",
            "com.mysql.jdbc.Driver");
        break;
      case 1:
        main.run(
            idSet,
            "jdbc:optiq:model=inline:" + OPTIQ_MODEL,
            "FOODMART_CLONE",
            null);
        break;
      }
    } catch (Throwable e) {
      e.printStackTrace();
    }
  }

  private void run(RangeSet<Integer> idSet, String jdbcUrl, String catalog,
      String driverClassName) throws IOException, SQLException,
     ClassNotFoundException {
    URL url = FoodMartQuery.class.getResource("/queries.json");
    InputStream inputStream = url.openStream();
    ObjectMapper mapper = new ObjectMapper();
    Map values = mapper.readValue(inputStream, Map.class);
    //noinspection unchecked
    List<Map<String, Object>> tests = (List) values.get("queries");
    if (driverClassName != null) {
      Class.forName(driverClassName);
    }
    Connection connection = DriverManager.getConnection(jdbcUrl);
    if (catalog != null) {
      connection.setCatalog(catalog);
    }
    Statement statement = connection.createStatement();
    for (Map<String, Object> test : tests) {
      int id = (Integer) test.get("id");
      if (!idSet.contains(id)) {
        continue;
      }
      String sql = (String) test.get("sql");
      if (jdbcUrl.startsWith("jdbc:mysql:")) {
        sql = sql.replace("\"", "`");
        sql = sql.replace(" NULLS FIRST", "");
        sql = sql.replace(" NULLS LAST", "");
        if (sql.contains("VALUES ")) {
          System.out.println("query id: " + id + " sql: " + sql + " skipped");
          continue;
        }
      }
      try {
        final long t0 = System.nanoTime();
        ResultSet resultSet = statement.executeQuery(sql);
        int n = 0;
        while (resultSet.next()) {
          ++n;
        }
        resultSet.close();
        final long nanos = System.nanoTime() - t0;
        System.out.println("query id: " + id + " rows: "
            + n + " nanos: " + nanos);
      } catch (SQLException e) {
        System.out.println("query id: " + id + " sql: " + sql
            + " error: " + e.getMessage());
      }
    }
    statement.close();
    connection.close();
  }

  private static RangeSet<Integer> parseInts(String idsProperty) {
    RangeSet<Integer> idSet = TreeRangeSet.create();
    if (idsProperty == null || idsProperty.isEmpty()) {
      return idSet;
    }
    for (String id : idsProperty.split(",")) {
      String[] split2 = id.split("-");
      if (split2.length != 2) {
        idSet.add(Range.singleton(Integer.parseInt(id)));
      } else if (split2[0].equals("")) {
        // -10 means "not 10"
        idSet.remove(Range.singleton(Integer.parseInt(split2[1])));
      } else if (split2[1].equals("")) {
        // 10- means "10 onwards"
        idSet.add(Range.atLeast(Integer.parseInt(split2[0])));
      } else {
        int min = Integer.parseInt(split2[0]);
        int max = Integer.parseInt(split2[1]);
        idSet.add(Range.closed(min, max));
      }
    }
    return idSet;
  }
}

// End Main.java
