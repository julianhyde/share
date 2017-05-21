# Extra code style checking.
case $(pwd) in
(*/hive/*) exit;;
esac
if [ "$1" ]; then git diff $1; else git diff HEAD; fi |
awk '
/^diff / {f=substr($3,3)}
/^@@ / {s=$0; gsub(/^[^+]*\+/,"",s); gsub(/,.*$/,"",s); line=s-1}
/^ / {++line}
/^\+/ {++line}
/^\+/ && !/^\+\+\+/ && f ~ /.java/ && length($0) > 81 {printf "%s:%s:%s\n", f, line, "line too long"; print}
/^\+/ && !/^\+\+\+/ && f ~ /.java/ && /for \(.*[^ ]:/ {printf "%s:%s:%s\n", f, line, "need space before colon"; print}
/^\+/ && !/^\+\+\+/ && f ~ /.java/ && /TODO/ {printf "%s:%s:%s\n", f, line, "TODO"; print}
    '
git ls-files | grep -v /fonts/ | egrep -v '\.(png|jpg|min.js|ico|scss)$' | xargs awk -f $(dirname $0)/extra.awk
for f in $(git ls-files | grep -v /fonts/ | egrep -v '\.(png|jpg|min.js|ico|scss)$'); do
  test $(tail -c 1 "$f") && echo "$f: 1: no newline at end of file"
done

# End extra.sh
