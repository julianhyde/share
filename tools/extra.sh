# Extra code style checking.
case $(pwd) in
(*/hive/*) exit;;
esac
git diff HEAD^ |
awk '
/^diff / {f=substr($3,3)}
/^@@ / {s=$0; gsub(/^[^+]*\+/,"",s); gsub(/,.*$/,"",s); line=s-1}
/^ / {++line}
/^\+/ {++line}
/^\+/ && !/^\+\+\+/ && f ~ /.java/ && length($0) > 81 {printf "%s:%s:%s\n", f, line, "line too long"; print}
/^\+/ && !/^\+\+\+/ && f ~ /.java/ && /for \(.*[^ ]:/ {printf "%s:%s:%s\n", f, line, "need space before colon"; print}
    '
exec find $(find . -name src -type d) -type f ! -name \*~ | xargs awk -f $(dirname $0)/extra.awk
