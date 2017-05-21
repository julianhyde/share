#!/bin/gawk
# Extra code style checking.
# Invoked from extra.sh.

function isProto(filename) {
  return filename ~ /\/proto\//;
}
function isJava(filename) {
  return filename ~ /.java$/;
}
function prevLine(i) {
  return prevLines[(k - i) % 5];
}
function afterFile() {
  if (isJava(prevFileName) && !isProto(prevFileName)) {
    b = prevFileName;
    gsub(/.*\//, "", b);
    if (prevLine(1) != "// End " b || prevLine(2) != "") {
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
  deprecated = -1;
  endCount = 0;
  maxLineLength = 80;
  if (FILENAME ~ /calcite/) {
    maxLineLength = 100;
  }
  startJavadoc = endJavadoc = blankJavadoc = 0;
  tableOpens = tableCloses = 0;
}
END {
  afterFile();
}
/\t/ && FILENAME !~ /data.*.txt/ {
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
/ $/ {
  if (!isProto(FILENAME)) {
    err(FILENAME, FNR, "Trailing spaces");
  }
}
/ );/ {
  err(FILENAME, FNR, "Spaces before )");
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
/@Override$/ && FILENAME !~ /\.md/ {
    err(FILENAME, FNR, "@Override should not be on its own line");
}
/<p>$/ {
    err(FILENAME, FNR, "Orphan <p>. Make it the first line of a paragraph");
}
/@/ && !/@see/ && length($0) > maxLineLength && isJava(FILENAME) {
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
/href=.*CALCITE-/ && FILENAME !~ /\.md/ {
  if ($0 !~ /<a href="https:\/\/issues.apache.org\/jira\/browse\/CALCITE-[0-9]+">\[CALCITE-[0-9]+\]/) {
    err(FILENAME, FNR, "Bad JIRA reference");
  }
}
/^package / {
  if (prevLine(1) == "" && isJava(FILENAME) && !isProto(FILENAME)) {
    err(FILENAME, FNR, "Blank line before 'package'");
  }
}
/\<(switch|if|for|while)\(/ {
  err(FILENAME, FNR, "Missing space between keyword and '('");
}
/subquer|SUBQUER|Subquer/ \
 && ! /subQuer|SUB_QUER|SubQuer|sub-quer|supportsSubqueries|supportsCorrelatedSubqueries/ \
 && FNR != deprecated + 1 {
  err(FILENAME, FNR, "Subquery, should be sub-query");
}
/@deprecated/ || /@Deprecated/ {
  deprecated = FNR;
}
/\/\*\*/ {
  startJavadoc = FNR;
}
/\*\// {
  endJavadoc = FNR;
}
endJavadoc < startJavadoc \
    && /^ *\*$/ {
  blankJavadoc = FNR;
}
endJavadoc < startJavadoc \
    && /<table/ {
  ++tableOpens;
}
endJavadoc < startJavadoc \
    && /<\/table>/ {
  ++tableCloses;
}
endJavadoc < startJavadoc \
    && FNR == blankJavadoc + 1 \
    && !/<\/?(p|ul|ol|dl|li|dd|dt|h[1234]|blockquote|table)\/?>/ \
    && tableCloses >= tableOpens \
    && !/ @/ {
  err(FILENAME, FNR, "Missing <p> in javadoc");
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
FILENAME ~ /\.(java|xml|sh)/ && /[^\x00-\xFF]/ {
  err(FILENAME, FNR, "Non-ASCII character");
}
{
  prevFnr = FNR;
  prevLines[k++ % 5] = $0;
}

# End extra.awk
