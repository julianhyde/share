#!/bin/bash
#
# Fixes images.  For each image, creates a symbolic link whose name
# includes the date of the image, and its rotation. It is idempotent:
# once a link exists, does not try to create it again.
#
# Example:
# $ fiximg --prefix ji ~/web/pix/staging/ji

#linkDir=/home/jhyde/web/pix/pix2013/raw2
link=true

getLabel() {
    # Date string like '2008C25'
    exiv2 "$1" |
    awk '
/^Image timestamp : / {
    y = substr($4, 1,4);
    m = substr($4, 6, 2) + 0;
    m2 = substr("123456789ABC", m, 1);
    d = substr($4, 9, 2);
    print y m2 d;
}'
}

getSuffix() {
    exiv2 -p a "$1" |
    awk '
/Image.Orien.*right, top/ { print "_rt"; exit }
/Image.Orien.*left, bottom/ { print "_lb"; exit }
/Image.Orien.*bottom, right/ { print "_br"; exit }
       '
}

fiximg() {
    i="$1"
    if [ -d "$1" ]; then
       ( cd "$1" ; for j in *; do fiximg "$j"; done )
       return
    fi
    thm=
    suffix=
    case "$i" in
    (*.JPG|*.JPEG)
        label=$(getLabel "$i")
        suffix=$(getSuffix "$i")
        ;;
    (*.AVI)
        thm=$(echo "$i" | sed -e 's/\.AVI$/.THM/');
        if [ -f "$thm" ]; then
            label=$(getLabel "$thm")
        fi
        ;;
    esac
    if [ "${label}" = "" ]; then
       echo "echo Cannot derive label for $i"
       return
    fi
    case $(basename "$i") in
    (P${label}*)
        ;;
    (*)
        b="$(basename ${i})"
        if [ "$suffix" ]; then
            b=$(echo $b | sed -e "s/\./${suffix}./")
        fi
        if [ "$link" ]; then
            case "${label}" in
            (2009*) linkDir=/home/jhyde/web/pix/pix2009/raw2;;
            (2010*) linkDir=/home/jhyde/web/pix/pix2010/raw2;;
            (2011*) linkDir=/home/jhyde/web/pix/pix2011/raw2;;
            (2012*) linkDir=/home/jhyde/web/pix/pix2012/raw2;;
            (2013*) linkDir=/home/jhyde/web/pix/pix2013/raw2;;
            (2014*) linkDir=/home/jhyde/web/pix/pix2014/raw;;
            (2015*) linkDir=/home/jhyde/web/pix/pix2015/raw;;
            (2016*) linkDir=/home/jhyde/web/pix/pix2016/raw;;
            (2017*) linkDir=/home/jhyde/web/pix/pix2017/raw;;
            (2018*) linkDir=/home/jhyde/web/pix/pix2018/raw;;
            (2019*) linkDir=/home/jhyde/web/pix/pix2019/raw;;
            (2020*) linkDir=/home/jhyde/web/pix/pix2020/raw;;
            (*)  linkDir=/home/jhyde/web/pix/pix2020/raw;;
            esac
            mkdir -p "$linkDir"
            if [ -f "${linkDir}/P${label}_${prefix}_${b}" ]; then
                echo "# ${linkDir}/P${label}_${prefix}_${b} exists"
            else
                echo "ln -s '$(cd $(dirname $i); pwd)/$i' ${linkDir}/P${label}_${prefix}_${b}"
            fi
        else
            echo "mv '$i' $(dirname $i)/P${label}_${prefix}_${b}"
        fi
        if [ -f "$thm" ]; then
            echo "mv '$thm' $(dirname $thm)/P${label}_${prefix}_$(basename ${thm})"
        fi;;
    esac
}

prefix=
if [ "$1" = "--prefix" ]; then
    shift
    prefix="$1"
    shift
fi

case "$prefix" in
(ji|pi|ipad|ixus|j6d|eos10d|berlie|gokhan|does|joy|wayne|malec|todd) ;;
(*) echo "Missing or invalid prefix $prefix" ; exit 1 ;;
esac

for i in "$@"; do
    fiximg "$i"
done

# Fix the damage done by rsync
chmod 755  ~/web/pix/staging/j6d/DCIM/100CANON/
chmod 755  ~/web/pix/staging/j6d/DCIM/
chmod 755  ~/web/pix/staging/j6d/

# End fiximg.sh
