#!/bin/gawk
# Extra code style checking.
# Invoked from extra.sh.

function isProto(filename) {
  return filename ~ /\/proto\//;
}
function afterFile() {
  if (prevFileName ~ /.java$/) {
    b = prevFileName;
    gsub(/.*\//, "", b);
    if (prevLine != "// End " b && !isProto(prevFileName)) {
      err(prevFileName, prevFnr, sprintf("Last line should be '%s'", "// End " b));
    }
  }
}
function err(f,n,m) {
  printf "%s: %d: %s\n", f, n, m;
}
FNR == 1 {
  afterFile();
  off = 0;
  prev = "";
  prevFileName = FILENAME;
  endCount = 0;
  maxLineLength = 80;
  if (FILENAME ~ /calcite/) {
    maxLineLength = 100;
  }
}
END {
  afterFile();
}
/\t/ {
  err(FILENAME, FNR, "Tab");
}
/CHECKSTYLE: ON/ {
  off = 0;
}
/CHECKSTYLE: OFF/ {
  off = 1;
}
off {
  next
}
/^\/\/ End / {
  if (endCount++ > 0) {
    err(FILENAME, FNR, "End seen more than once");
  }
}
/\\n" \+ "/ {
  err(FILENAME, FNR, "Newline in string should be at end of line");
}
/{@link/ {
  if ($0 !~ /}/) {
    err(FILENAME, FNR, "Split @link");
  }
}
/@Override$/ {
    err(FILENAME, FNR, "@Override should not be on its own line");
}
/<p>$/ {
    err(FILENAME, FNR, "Orphan <p>. Make it the first line of a paragraph");
}
/@/ && !/@see/ && length($0) > maxLineLength {
  s = $0;
  gsub(/^ *\* */, "", s);
  gsub(/ \*\/$/, "", s);
  gsub(/[;.,]$/, "", s);
  gsub(/<li>/, "", s);
  if (s ~ /^{@link .*}$/) {}
  else if (FILENAME ~ /CalciteResource.java/) {}
  else {
    err(FILENAME, FNR, "Javadoc line too long (" + length($0) + " chars)");
  }
}
/@param +[^ ]+ *$/ {
  err(FILENAME, FNR, "Parameter with no description");
}
/href=.*CALCITE-/ {
  if ($0 !~ /<a href="https:\/\/issues.apache.org\/jira\/browse\/CALCITE-[0-9]+">\[CALCITE-[0-9]+\]/) {
    err(FILENAME, FNR, "Bad JIRA reference");
  }
}
FILENAME ~ /\.java/ && (/\(/ || /\)/) {
  s = $0;
  if ($0 ~ /"/) {
    gsub(/"([^"]|\\\")*"/, "string", s);
  }
  o = 0;
  for (i = 1; i <= length(s); i++) {
    c = substr(s, i, 1);
    if (c == "(" && i > 1 && substr(s, i - 1, 1) ~ /[A-Za-z0-9_]/) {
      ++o;
    } else if (c == ")") {
      --o;
    }
  }
  if (o > 1 && FILENAME !~ /proto\/Common.java/) {
    err(FILENAME, FNR, "Open parentheses exceed closes by 2 or more");
  }
}
{
  prevFnr = FNR;
  prevLine = $0;
}

# End extra.awk
