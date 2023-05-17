FROM ubuntu:22.04

# Set to noninteractive mode
ARG DEBIAN_FRONTEND=noninteractive

################################################################################
# Basic dependencies
################################################################################

RUN apt-get update && apt-get -yqq install git lsb-release sudo vim gnupg openjdk-17-jdk verilator curl make gcc g++

################################################################################
# Install sbt (https://www.scala-sbt.org/)
################################################################################

RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add
RUN apt-get update
RUN apt-get -yqq install sbt

################################################################################
# Install GNU RISC-V toolchain
################################################################################

WORKDIR /toolchain
RUN git clone https://github.com/riscv/riscv-gnu-toolchain .
RUN apt-get -yqq install autoconf automake autotools-dev curl python3 libmpc-dev libmpfr-dev libgmp-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev libexpat-dev ninja-build
RUN ./configure --prefix=/opt/riscv --with-arch=rv32im_zicsr --with-abi=ilp32 && \
    make && \
    make clean
ENV PATH="${PATH}:/opt/riscv/bin"

################################################################################
# Build dynamic pipeline and run RISC-V tests
################################################################################

WORKDIR /proteus
COPY . .
RUN make -C sim clean
RUN make -C tests CORE=riscv.CoreDynamicExtMem RISCV_PREFIX=riscv32-unknown-elf

################################################################################
# Run newlib example code
################################################################################

WORKDIR /proteus/newlib
RUN make
RUN /proteus/sim/build/sim main.bin

CMD /bin/bash
