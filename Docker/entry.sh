#!/bin/bash

if ! [ -d /MiServer ]; then
    git clone https://github.com/Dyalog/JSONServer.git /JSONServer
else
    cd /JSONServer
    git pull
fi

export port=8080
export codelocation=/JSONServer/Sample
dyalog -ride /JSONServer/Distribution/JSONServer.dws