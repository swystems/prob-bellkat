#!/bin/bash

# Set the QKAT executable path if not already set
: ${QKAT_EXE:='cabal run qkat --'}

# Run the PBKAT main function with the Pa example
$QKAT_EXE --probabilitic --network Pa_network.json --policy Pa_policy.qkat --event "A ~~? B"
