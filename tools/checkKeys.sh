# checkKeys.sh
#
# Utility to verify md5 and sha1 checksums

function doCheckSum() {
  case "${1}.$(uname -s)" in
  (md5.Linux) md5sum "$2" | awk '{print $1}';;
  (sha1.Linux) sha1sum "$2" | awk '{print $1}';;
  (md5.Darwin) sha1 -q "$2";;
  (sha1.Darwin) md5 -q "$2";;
  (*) echo "Unknown command $1 or platform $(uname -s)" >&1; exit 1;;
  esac
}

function checkHash() {
  cd "$1"
  for i in *.{zip,pom,gz}; do
    if [ ! -f $i ]; then
      continue
    fi
    if [ -f $i.md5 ]; then
      left="$(cat $i.md5)"
      right="$(doCheckSum md5 $i)"
      if [ "$left" = "$right" ]; then
        echo "$i.md5 present and correct"
      else
        echo "$i.md5 does not match (left: $left, right: $right)"
      fi
    else
      md5 -q $i > $i.md5
      echo $i.md5 created
    fi

    if [ -f $i.sha1 ]; then
      left="$(cat $i.sha1)"
      right="$(doCheckSum sha1 $i)"
      if [ "$left" = "$right" ]; then
        echo $i.sha1 present and correct
      else
        echo "$i.sha1 does not match (left: $left, right: $right)"
      fi
    else
      sha1 -q $i > $i.sha1
      echo $i.sha1 created
    fi

    if [ -f $i.asc ]; then
      gpg --verify $i.asc $i
    else
      echo "$i.asc missing"
    fi
  done
}
checkHash "$1"

# End checkKeys.sh
