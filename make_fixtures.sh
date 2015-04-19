#!/usr/bin/env bash

FIXTURE_FILE=Test/Fixtures.elm

add_json() {
    JSON_FILE="$1"
    VAR_NAME=$(basename $JSON_FILE | sed 's/\./_/')
    echo -n $VAR_NAME >> $FIXTURE_FILE
    echo ' = """' >> $FIXTURE_FILE
    if [ $JSON_FILE = "data/doc_metadata.json" ]; then
        cat $JSON_FILE | jq 'with_entries(.value |= {id, title: .title | gsub("(\\\"|\\\\)";"") , user: {username: .user.username}})' >> $FIXTURE_FILE
    else
        cat $JSON_FILE >> $FIXTURE_FILE
    fi
    echo '"""' >> $FIXTURE_FILE
    echo >> $FIXTURE_FILE
}



rm -f $FIXTURE_FILE

echo "module Test.Fixtures where" > $FIXTURE_FILE
echo >> $FIXTURE_FILE

for i in data/*.json; do
    add_json $i
done
