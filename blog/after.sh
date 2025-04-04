# Run this script after 'docker compose run build-site' to highlight
# keywords that are in Morel but not in Standard ML.
#
for f in _site/*/*/*/*.html; do
  perl -p -i -e '
s!<span class="n">(commit|from|not|union|yield)</span>!<span class="kr">\1</span>!g;
' $f

# Special keywords for morel-dml only
  case "$f" in
  (*/morel-dml.html)
    perl -p -i -e '
s!<span class="n">(assign|delete|insert|update)</span>!<span class="kr">\1</span>!g;
' $f;;
  esac
done

# End after.sh

