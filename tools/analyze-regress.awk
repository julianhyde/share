#!/bin/gawk
# Analyzes the output of a 'mvn clean test' run

/JAVA_HOME .*jdk1.7/ {jdk7=1}
/^Tests run:/ {f+=$5;e+=$7}
/SIGSEGV/ {++c}
/Tag @link: reference not found/ {++j}
/Parameter ".*" is documented more than once/ {++j}
/ has been deprecated/ && !/org.apache.calcite.avatica.proto.Responses/ {++d}
END {
  # Evidently @SuppressWarnings("deprecation") doesn't work until jdk1.8
  if (jdk7) d=0;
  if (f + e + c + j + d > 0) {
    printf "fecjd: %d:%d:%d:%d:%d\n", f, e, c, j, d;
  }
}

# End analyze-regress.awk

