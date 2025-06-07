# Run this script after 'docker compose run build-site' to highlight
# keywords that are in Morel but not in Standard ML.
#
# Morel-specific keywords such as 'from' are highlighted.
#
# We recommend that you enclose output lines with '(*[' ... ']*)'
# comments. The comment markers will be removed but the lines will
# remain grayed out.
#

function main
{
  for f in _site/*/*/*/*.html; do
    perl -p -i -e '
  s!<span class="n">(commit|from|join|on|not|union|intersect|except|yield|distinct|group|compute|order|unorder|desc|DESC|forall|exists|require|skip|take|andalso|orelse|mod|div|implies|elem|ordinal|current)</span>!<span class="kr">\1</span>!g;
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
}

if [ "$1" = "-nt" ]; then
  if [ "$2" -nt /tmp/after.txt ]; then
    echo Regenerating at $(date)...
    main
    touch /tmp/after.txt
  fi
else
  main
fi

# End after.sh
