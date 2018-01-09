#!/bin/bash

if ! [ -d /MiServer ]; then
    git clone https://github.com/Dyalog/JSONServer.git /JSONServer
else
    cd /JSONServer
    git pull
fi

dyalog -ride /JSONServer/Distribution/JSONServer port=8080 codelocation=/JSONServer/Sample