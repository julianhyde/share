# Run this script after 'docker compose run build-site' to highlight
# keywords that are in Morel but not in Standard ML.
#
# Morel-specific keywords such as 'from' are highlighted.
#
# We recommend that you enclose output lines with '(*[' ... ']*)'
# comments. The comment markers will be removed but the lines will
# remain grayed out.
#
for f in _site/*/*/*/*.html; do
  perl -p -i -e '
s!<span class="n">(commit|from|join|on|not|union|intersect|except|yield|distinct|group|compute|order|desc|forall|exists|require|skip|take|andalso|orelse|mod|div|implies|elem)</span>!<span class="kr">\1</span>!g;
s!\(\*</span><span class="cm">\[!!g;
s!\]\*\)!!g;
' $f

# Special keywords for morel-dml only
  case "$f" in
  (*/dml-in-morel.html)
    perl -p -i -e '
s!<span class="n">(assign|delete|insert|update)</span>!<span class="kr">\1</span>!g;
' $f;;
  esac
done

# End after.sh

