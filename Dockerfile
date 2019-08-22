FROM dyalog/dyalog

ADD Docker/run /
ADD . /JSONServer

RUN mkdir -p /app

EXPOSE 8080
