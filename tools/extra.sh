# Extra code style checking.
case $(pwd) in
(*/hive/*) exit;;
esac
git diff HEAD^ |
awk '
/^diff / {f=$3}
/^\+/ && !/^\+\+\+/ && f ~ /.java/ && length($0) > 81 {print f " line too long"; print}
/^\+/ && !/^\+\+\+/ && f ~ /.java/ && /for \(.*[^ ]:/ {print f " need space before colon"; print}
    '
exec find $(find . -name src -type d) -type f ! -name \*~ | xargs awk -f $(dirname $0)/extra.awk
