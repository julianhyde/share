# Extra code style checking.
case $(pwd) in
(*/hive/*) exit;;
esac
exec find $(find . -name src -type d) -type f ! -name \*~ | xargs awk -f $(dirname $0)/extra.awk
