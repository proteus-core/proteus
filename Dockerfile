FROM ubuntu:22.04

WORKDIR ProteusCore

# Set to noninteractive mode
ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update
RUN apt-get -yqq install git lsb-release sudo vim gnupg openjdk-17-jdk verilator curl make gcc g++

# Install sbt - https://www.scala-sbt.org/
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add
RUN apt-get update
RUN apt-get -yqq install sbt

COPY . .

CMD /bin/bash
