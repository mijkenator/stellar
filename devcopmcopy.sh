#!/bin/bash

echo "copying beams from _build/default/lib/stellar/ebin/*.beam to _build/default/rel/stellar/lib/stellar-0.1.0/ebin/"
set -x
cp _build/default/lib/stellar/ebin/*.beam to _build/default/rel/stellar/lib/stellar-0.1.0/ebin/
set +x
