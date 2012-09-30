#/bin/bash
#
# Generate man pages
#
# This script reads a source file with embedded man page comments and
# creates the man pages from them. Each comment has to start with a special
# tag. Each man page is translated using "ronn"
# <https://github.com/rtomayko/ronn> from the Markdown syntax to troff files
# as used by man.
#
# This script requires "ronn", which is available with the Debian package
# "ruby-ronn". Note that this installs the "ruby" interpreter and some other
# dependencies, but these are only 2 MB.
#

# Configuration ############################################################

if [ $# -ne 1 ] ; then
  echo "Usage: $0 filename"
  exit 1
fi

# input file
INFILE="$1"
# output directory
OUTDIR=./man

START_TAG="(*ronn"
END_TAG="*)"

# Internals ################################################################

if [ ! -r ${INFILE} ] ; then
  echo "ERROR: Can't read ${INFILE}"
  exit 1
fi

# temporary file to store the "ronn" sources
TEMP=$(mktemp --tmpdir genmanXXXXXX)

# inside of a man page comment
INCOMMENT=false
# set to true when the first non-empty line is found, which contains the man
# page title and section
HAVETITLE=false

while read ; do
  if $INCOMMENT ; then
    # inside of a man page comment
    if [ "$REPLY" = "$END_TAG" ] ; then
      # found the end tag -> create man page
      INCOMMENT=false
      # create directory if necessary
      mkdir -p $OUTDIR/man$CHAP
      # write to file
      mv ${TEMP} $OUTDIR/man$CHAP/$NAME.ronn
      # convert to roff format
      ronn --roff $OUTDIR/man$CHAP/$NAME.ronn
      # remove .ronn file
      rm $OUTDIR/man$CHAP/$NAME.ronn
    else
      # man page comment lines
      if $HAVETITLE ; then
        # normal line -> directly write to file
        echo "$REPLY" >> $TEMP
      elif [ -n "$REPLY" ] ; then
        # first non-empty line, which contains the man page title and section
        echo "$REPLY" >> $TEMP
        HAVETITLE=true
        PAGE="${REPLY%% *}"   # "connect(1ez)"
        NAME="${PAGE%%(*}"    # "connect"
        SECT="${PAGE##*(}"    # "1ez)"
        SECT="${SECT%%)}"     # "1ez"
        CHAP="${SECT:0:1}"    # "1"
        echo "Found page $PAGE $NAME $SECT"
      fi
    fi
  else
    # somewhere outside of a man page comment
    if [ "$REPLY" = "$START_TAG" ] ; then
      # found start tag
      INCOMMENT=true
      HAVETITLE=false
    fi
  fi
done < ${INFILE}

# generate man page index
mandb $OUTDIR
