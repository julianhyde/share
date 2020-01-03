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
  if (isJava(prevFileName) && !isProto(prevFileName) && needEnd) {
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
  needEnd = true;
  if (FILENAME ~ /calcite/) {
    maxLineLength = 100;
    needEnd = false;
  }
  startJavadoc = endJavadoc = blankJavadoc = nonBlank = import = package = 0;
  tableOpens = tableCloses = 0;
  startPre = endPre = 0;
}
END {
  afterFile();
}
/\t/ && FILENAME !~ /data.*.txt/ && FILENAME !~ /.(xsl|js|css|zip)$/ {
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
  if (!isProto(FILENAME) && FILENAME !~ /.(css|html|js|xsl|zip)$/) {
    err(FILENAME, FNR, "Trailing spaces");
  }
}
/^\/\/ End / {
  if (endCount++ > 0) {
    err(FILENAME, FNR, "End seen more than once");
  }
}
/\\n" \+ "/ {
  err(FILENAME, FNR, "Newline in string should be at end of line");
}
/^ +(=|->) / {
  err(FILENAME, FNR, "'" $1 "' must not be at start of line");
}
FILENAME ~ /\.java/ && / [:?]$/ {
  err(FILENAME, FNR, "'" $NF "' must not be at end of line");
}
/{@link/ {
  if ($0 !~ /}/) {
    err(FILENAME, FNR, "Split @link");
  }
}
/@return$/ {
  err(FILENAME, FNR, "Empty javadoc tag");
}
/@throws( *[^ ]*)?$/ {
  err(FILENAME, FNR, "Empty javadoc tag");
}
/\*   *@/ {
  err(FILENAME, FNR, "Too many spaces before @");
}
/@Override$/ && FILENAME !~ /\.md/ {
  err(FILENAME, FNR, "@Override should not be on its own line");
}
/<p>$/ && FILENAME !~ /\.html/ {
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
  if (FNR > nonBlank + 1 && isJava(FILENAME) && !isProto(FILENAME)) {
    err(FILENAME, FNR, "1 or more blank lines before 'package'");
  }
}
/^import / {
  import = FNR;
  if (FNR > nonBlank + 2) {
    err(FILENAME, FNR, "2 or more blank lines before 'import'");
  }
}
/\<(switch|if|for|while)\(/ && FILENAME !~ /.(html|js)$/ {
  err(FILENAME, FNR, "Missing space between keyword and '('");
}
s ~ /\<(case .*|default) :/ {
  err(FILENAME, FNR, "Space before ':' following case/default");
}
/subquer|SUBQUER|Subquer/ \
 && ! /subQuer|SUB_QUER|SubQuer|sub-quer|supportsSubqueries|supportsCorrelatedSubqueries/ \
 && FNR != deprecated + 1 {
  err(FILENAME, FNR, "Subquery, should be sub-query");
}
/ sql / \
 && ! /(assert|String|StringBuilder|+|=|highlight|@param *) sql/ \
 && ! /sql (=|+)/ {
  # err(FILENAME, FNR, "sql, should be SQL");
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
  && /<pre>/ {
    startPre = FNR;
}
endJavadoc < startJavadoc \
  && /<\/pre>/ {
    endPre = FNR;
}
endJavadoc < startJavadoc \
    && FNR == blankJavadoc + 1 \
    && !/<\/?(p|ul|ol|dl|li|dd|dt|h[1234]|blockquote|table)\/?>/ \
    && tableCloses >= tableOpens \
    && endPre >= startPre \
    && !/ @/ \
    && FILENAME !~ /.js$/ {
  err(FILENAME, FNR, "Missing <p> in javadoc");
}
/<tt>/ || /<tt\/>/  || /<\/tt>/ {
  if (FILENAME ~ /.md$/ && $0 ~ /`/) {
    # ignore
  } else {
    err(FILENAME, FNR, "In HTML5, <tt> is deprecated; use <code>")
  }
}
FILENAME ~ /\.java/ && (/\(/ || /\)/) {
  s = $0;
  if ($0 ~ /"/) {
    gsub(/"([^"]|\\")*"/, "string", s);
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
s ~ /\.$/ \
    && endJavadoc >= startJavadoc \
    && endJavadoc < FNR \
    && s !~ /\/\// {
  err(FILENAME, FNR, "'.' must not be at end of line");
}
(s ~ / );/ || s ~ / ))/ || s ~ / ),/ || s ~ / ) {/ || s ~ / )$/) \
    && endJavadoc >= startJavadoc \
    && endJavadoc < FNR \
    && s !~ /\/\// {
    print startJavadoc
    print endJavadoc
  err(FILENAME, FNR, "Spaces before )");
}
FILENAME ~ /\.(java|xml|sh)/ && /[^\x00-\xFF]/ {
  err(FILENAME, FNR, "Non-ASCII character");
}
/^./ {
  if (nonBlank == import && FNR > nonBlank + 2) {
    err(FILENAME, FNR, "2 or more blank lines after import");
  }
  if (nonBlank == package && FNR > nonBlank + 2) {
    err(FILENAME, FNR, "2 or more blank lines after package");
  }
  nonBlank = FNR;
}
{
  prevFnr = FNR;
  prevLines[k++ % 5] = $0;
  s = "";
}

# End extra.awk
