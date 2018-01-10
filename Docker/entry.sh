#!/bin/bash

if ! [ -d /JSONServer ]; then
    git clone https://github.com/Dyalog/JSONServer /JSONServer
else
    cd /JSONServer
    git pull
fi

export Port=8080
export CodeLocation=/JSONServer/Sample
dyalog -ride /JSONServer/Distribution/JSONServer.dws
