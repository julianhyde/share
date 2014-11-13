#!/bin/gawk
# Extra code style checking.
# Invoked from extra.sh.

function afterFile() {
  if (prevFileName ~ /.java$/) {
    b = prevFileName;
    gsub(/.*\//, "", b);
    if (prevLine != "// End " b) {
      err(prevFileName, prevFnr, sprintf("Last line should be '%s'\n", "// End " b));
    }
  }
}
function err(f,n,m) {
  printf "%s: %d: %s\n", f, n, m;
}
FNR == 1 {
  afterFile();
  prev = "";
  prevFileName = FILENAME;
  endCount = 0;
}
END {
  afterFile();
}
/^\/\/ End / {
  if (endCount++ > 0) {
    err(FILENAME, FNR, "End seen more than once");
  }
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
/@/ && !/@see/ && length($0) > 80 {
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
{
  prevFnr = FNR;
  prevLine = $0;
}

# End extra.awk
