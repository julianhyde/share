#!/bin/gawk
# Analyzes the output of a 'mvn clean test' run
#
# Invoke with "-v verbose=1" to get more output

function p() {
  if (verbose) {
    print
  }
}
/JAVA_HOME .*jdk1.7/ {jdk7=1}
/JAVA_HOME .*jdk1.8/ {jdk8=1}
/^(\[(ERROR|INFO|WARNING)\] )?Tests run: / {
  gsub(/^(\[(ERROR|INFO|WARNING)\] )?Tests run:/, "")
  f+=$3; e+=$5;
  if ($3+0 > 0 || $5+0 > 0) p()
}
/SIGSEGV/ {++c; p()}
/Tag @link: reference not found/ {++j; p()}
/Parameter ".*" is documented more than once/ {++j; p()}
/ has been deprecated/ && !/org.apache.calcite.avatica.proto.Responses/ {
  # Evidently @SuppressWarnings("deprecation") doesn't work until jdk1.9
  if (!jdk7 && !jdk8) {
    ++d;
    p()
  }
}
END {
  if (f + e + c + j + d > 0) {
    printf "fecjd: %d:%d:%d:%d:%d\n", f, e, c, j, d;
  }
}

# End analyze-regress.awk

