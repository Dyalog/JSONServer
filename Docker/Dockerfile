FROM dyalog/dyalog:17.1-dbg

RUN apt-get update && apt-get install -y git && \
    apt-get clean && rm -Rf /var/lib/apt/lists/*

RUN git clone https://github.com/Dyalog/JSONServer /JSONServer

RUN mkdir /app

EXPOSE 8080
ADD run /
