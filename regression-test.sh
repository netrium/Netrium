#!/bin/bash

CONTRACTS="$@"

if test "${CONTRACTS}" = ""
then
  echo "Usage: ./regression-test [contract...]"
  echo "One or more .contract files, for example:"
  echo "$ ./regression-test examples/*.contract"
  exit
fi

for CONTRACT in ${CONTRACTS}; do
  echo -n "${CONTRACT}: "
  
  CONTRACT_DIR="$(dirname ${CONTRACT})"
  CONTRACT_BASE=${CONTRACT%%.contract}
  
  if test -f ${CONTRACT_BASE}.obsdb.xml
  then
    OBSDB="--obs-db=${CONTRACT_BASE}.obsdb.xml"
  else
    OBSDB=;
  fi
  if test -f "${CONTRACT_DIR}/Units.db.xml"
  then
    UNITSDB="--units-db=${CONTRACT_DIR}/Units.db.xml"
  else
    UNITSDB=
  fi
  
  if normalise ${UNITSDB} ${OBSDB} ${CONTRACT} ${CONTRACT}.xml --fast \
      > normalise-out.log 2>&1
  then
    echo -n "compiled OK"
    if simulate ${CONTRACT}.xml ${CONTRACT_BASE}.timeseries.xml ${CONTRACT_BASE}.out \
         > simulate-out.log 2>&1
    then
      echo ", simulate OK"
      cat normalise-out.log
      cat simulate-out.log
    else
      echo ", simulate FAILED:"
      cat simulate-out.log
    fi
  else
    echo "compile FAILED:"
    cat normalise-out.log
  fi
done
rm -f normalise-out.log simulate-out.log
