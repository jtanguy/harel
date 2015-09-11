FROM fpco/stack-build:latest

ADD . /opt/harel

WORKDIR /opt/harel

RUN stack setup

RUN stack build --only-snapshot

RUN stack build

RUN stack install

EXPOSE 8080

CMD stack exec -- harel
