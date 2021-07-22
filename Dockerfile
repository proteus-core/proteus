## Based on https://github.com/fritzalder/faulty-point-unit/blob/master/Dockerfile

# Clean Ubuntu 18.04 container
FROM ubuntu:20.04
RUN apt-get update -yqq && apt-get -yqq install git lsb-release sudo vim gnupg

# Config parameters
WORKDIR ProteusCore

# Set to noninteractive mode
ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Europe/Brussels
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# Install dependencies for ProteusCore
RUN apt-get -yqq install openjdk-11-jdk verilator curl make gcc g++

# Install SBT - https://www.scala-sbt.org/
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo apt-key add
RUN apt-get update
RUN apt-get -yqq install sbt

# Copy the core in - this needs to use GIT later
COPY . .

# Display a welcome message for interactive sessions
RUN echo '[ ! -z "$TERM" -a -r /etc/motd ] && cat /etc/motd' \
	>> /etc/bash.bashrc ; echo "\
========================================================================\n\
= ProteusCore simulator Docker container                               =\n\
========================================================================\n\
`lsb_release -d`\n\n\
To get started, see README.md in the current directory\n"\
> /etc/motd

CMD /bin/bash

