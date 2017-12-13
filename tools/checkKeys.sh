# checkKeys.sh
#
# Utility to verify md5 and sha1 checksums

function doCheckSum() {
  case "${1}.$(uname -s)" in
  (md5.Linux) md5sum "$2" | awk '{print $1}';;
  (sha*.*) shasum -a $(echo $1 | sed -e s/sha//) "$2" | awk '{print $1}';;
  (md5.Darwin) md5 -q "$2";;
  (*) echo "Unknown command $1 or platform $(uname -s)" >&1; exit 1;;
  esac
}

function checkHash() {
  cd "$1"
  for i in *.{zip,pom,gz,jar}; do
    if [ ! -f $i ]; then
      continue
    fi

    if [ -f $i.mds ]; then
      gpg --print-mds $i > /tmp/x.mds
      if diff $i.mds /tmp/x.mds; then
        echo "$i.mds is present and correct"
      else
        echo "$.mds does not match"
      fi
    fi

    if [ -f $i.md5 ]; then
      left="$(awk '{s = $0; if (s ~ /\./) s = $1; print s}' $i.md5)"
      right="$(doCheckSum md5 $i)"
      if [ "$left" = "$right" ]; then
        echo "$i.md5 present and correct"
      else
        echo "$i.md5 does not match (left: $left, right: $right)"
      fi
    else
      doCheckSum md5 $i > $i.md5
      echo $i.md5 created
    fi

    if [ -f $i.sha1 ]; then
      left="$(awk '{s = $0; if (s ~ /\./) s = $1; print s}' $i.sha1)"
      right="$(doCheckSum sha1 $i)"
      if [ "$left" = "$right" ]; then
        echo $i.sha1 present and correct
      else
        echo "$i.sha1 does not match (left: $left, right: $right)"
      fi
    else
      doCheckSum sha1 $i > $i.sha1
      echo $i.sha1 created
    fi

    if [ -f $i.sha256 ]; then
      left="$(awk '{s = $0; if (s ~ /\./) s = $1; print s}' $i.sha256)"
      right="$(doCheckSum sha256 $i)"
      if [ "$left" = "$right" ]; then
        echo $i.sha256 present and correct
      else
        echo "$i.sha256 does not match (left: $left, right: $right)"
      fi
    else
      doCheckSum sha256 $i > $i.sha256
      echo $i.sha256 created
    fi

    if [ -f $i.asc ]; then
      # If verify fails with the message
      #   gpg: Signature made Fri 09 Oct 2015 03:06:51 AM PDT using RSA key ID <key hash>
      #   gpg: Can't check signature: public key not found
      # you need to get the key, as follows:
      #   curl -O https://people.apache.org/keys/committer/committer-name.asc
      #   head committer-name.asc
      #   gpg --recv-keys <key hash>
      # Then re-try verification.
      gpg --verify $i.asc $i
    else
      echo "$i.asc missing"
    fi
  done
}
checkHash "$1"

# End checkKeys.sh
