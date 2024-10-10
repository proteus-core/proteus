FROM ubuntu:24.04

# Set to noninteractive mode
ARG DEBIAN_FRONTEND=noninteractive

################################################################################
# Basic dependencies
################################################################################

RUN apt-get update && apt-get -yqq install git lsb-release sudo vim gnupg openjdk-17-jdk verilator curl make gcc g++

################################################################################
# Install scala and sbt (https://www.scala-sbt.org/)
################################################################################

RUN curl -fL https://github.com/coursier/coursier/releases/latest/download/cs-x86_64-pc-linux.gz | gzip -d > cs && chmod +x cs && ./cs setup --yes
ENV PATH="$PATH:/root/.local/share/coursier/bin"

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
