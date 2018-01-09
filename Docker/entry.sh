#!/bin/bash

if ! [ -d /JSONServer ]; then
    git clone https://github.com/Dyalog/JSONServer /JSONServer
else
    cd /JSONServer
    git pull
fi

export port=8080
export codelocation=/JSONServer/Sample
dyalog -ride /JSONServer/Distribution/JSONServer.dws
