#!/usr/bin/env bash
if [ -e "env-local.sh" ]; then
    echo "Sourcing env-local.sh"
    source env-local.sh
    activator ~run
else
    target/universal/stage/bin/partyserver
fi
