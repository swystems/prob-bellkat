#!/usr/bin/env bash

# Set the QKAT executable path if not already set
: ${QKAT_EXE:='cabal run qkat --'}

# Get the directory of the current script
SCRIPT_DIR="$(dirname "$0")"

# Run the PBKAT main function with the Pa example
$QKAT_EXE --probabilitic --network "$SCRIPT_DIR/Pa_network.json" --policy "$SCRIPT_DIR/Pa_policy.qkat" --event "A ~~? B" "$@"
