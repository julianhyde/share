# checkKeys.sh
#
# Utility to verify md5 and sha1 checksums

function doCheckSum() {
  case "${1}.$(uname -s)" in
  (md5.Linux) md5sum "$2" | awk '{print $1}';;
  (sha1.Linux) sha1sum "$2" | awk '{print $1}';;
  (md5.Darwin) md5 -q "$2";;
  (sha1.Darwin) shasum -a 1 "$2" | awk '{print $1}';;
  (*) echo "Unknown command $1 or platform $(uname -s)" >&1; exit 1;;
  esac
}

function checkHash() {
  cd "$1"
  for i in *.{zip,pom,gz,jar}; do
    if [ ! -f $i ]; then
      continue
    fi
    if [ -f $i.md5 ]; then
      left="$(cat $i.md5 | awk '{print $NF}')"
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
      left="$(cat $i.sha1 | awk '{print $NF}')"
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
