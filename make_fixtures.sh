#!/usr/bin/env bash

FIXTURE_FILE=Test/Fixtures.elm

add_json() {
    JSON_FILE="$1"
    VAR_NAME=$(basename $JSON_FILE | sed 's/\./_/')
    echo -n $VAR_NAME >> $FIXTURE_FILE
    echo ' = """' >> $FIXTURE_FILE
    cat $JSON_FILE >> $FIXTURE_FILE
    echo '"""' >> $FIXTURE_FILE
    echo >> $FIXTURE_FILE
}

rm -f $FIXTURE_FILE

echo "module Test.Fixtures where" > $FIXTURE_FILE
echo >> $FIXTURE_FILE

for i in data/*.json; do
    add_json $i
done
