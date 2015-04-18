#!/bin/sh

D3_DIR=$HOME/Development/elm-d3
ALIAS_D3=elm-stuff/packages/seliopou/elm-d3/1.0.0
ALIAS_D3_BUILD=elm-stuff/build-artifacts/seliopou

D3_FILTER='.dependencies | has("seliopou/elm-d3")'
ADD_D3='.dependencies = .dependencies + {"seliopou/elm-d3": "1.0.0 <= v < 2.0.0"}'

HAS_EXACT='has("seliopou/elm-d3")'
ADD_EXACT='. + { "seliopou/elm-d3": "1.0.0" }'

link_if_not_exists() {
    if [ ! -d "$1" ]; then
        mkdir -p $(dirname "$1")
        ln -s "$2" "$1"
    fi
}

filter_and_replace() {
    FILENAME="$1"
    COND=$2
    FILT="$3"
    if [ $(cat $FILENAME | jq "$COND") = "true" ]; then
        echo "$COND already satisfied."
    else
        echo "Adding $FILT"
        ( cat $FILENAME | jq "$FILT" > $FILENAME.new ) && mv $FILENAME.new $FILENAME
    fi
}

filter_and_replace elm-package.json "$D3_FILTER" "$ADD_D3"
filter_and_replace elm-stuff/exact-dependencies.json "$HAS_EXACT" "$ADD_EXACT"

link_if_not_exists $ALIAS_D3 $D3_DIR
link_if_not_exists $ALIAS_D3_BUILD $D3_DIR/$ALIAS_D3_BUILD