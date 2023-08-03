FROM ubuntu:22.04

# Set to noninteractive mode
ARG DEBIAN_FRONTEND=noninteractive

ARG BENCHMARKS
RUN echo "Running benchmarks: ${BENCHMARKS}"

RUN apt-get update
RUN apt-get -yqq install git lsb-release sudo vim gnupg openjdk-17-jdk verilator curl make gcc g++ python3-pip

# Install sbt - https://www.scala-sbt.org/
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add
RUN apt-get update
RUN apt-get -yqq install sbt

# install vcdvcd (for eval)
WORKDIR /vcdvcd

RUN git clone --depth=1 --branch v2.3.3 https://github.com/cirosantilli/vcdvcd .
RUN python3 -m pip install --user /vcdvcd

# install gnu toolchain
WORKDIR /toolchain
RUN git clone --depth=1 --branch 2023.07.07 https://github.com/riscv/riscv-gnu-toolchain .
RUN apt-get -yqq install autoconf automake autotools-dev curl python3 libmpc-dev libmpfr-dev libgmp-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev libexpat-dev ninja-build
RUN ./configure --prefix=/opt/riscv --with-arch=rv32im_zicsr --with-abi=ilp32 && \
    make && \
    make clean

ENV PATH="${PATH}:/opt/riscv/bin"

# Install baseline version of Proteus
WORKDIR /proteus-base
RUN git clone --recurse-submodules --depth=1 --branch prospect_submission https://github.com/proteus-core/proteus.git .
# create simulator binary and run riscv-tests
RUN make -C tests CORE=riscv.CoreDynamicExtMem RISCV_PREFIX=riscv32-unknown-elf
RUN mv sim/build/sim sim/build/base
# comment out waveform dumping to save disk space
RUN sed -e '/tracer->dump(mainTime);/s/^/\/\//g' -i sim/main.cpp
# create simulator binary and run riscv-tests
RUN make -C tests CORE=riscv.CoreDynamicExtMem RISCV_PREFIX=riscv32-unknown-elf
RUN mv sim/build/sim sim/build/base_nodump

# Install Proteus extended with ProSpeCT
WORKDIR /prospect
RUN git clone --recurse-submodules --depth=1 --branch usenix_artifact https://github.com/proteus-core/prospect.git .
# create simulator binary and run riscv-tests
RUN make -C tests CORE=riscv.CoreDynamicExtMem RISCV_PREFIX=riscv32-unknown-elf
RUN mv sim/build/sim sim/build/prospect
# comment out waveform dumping to save disk space
RUN sed -e '/tracer->dump(mainTime);/s/^/\/\//g' -i sim/main.cpp
# create simulator binary and run riscv-tests
RUN make -C tests CORE=riscv.CoreDynamicExtMem RISCV_PREFIX=riscv32-unknown-elf
RUN mv sim/build/sim sim/build/prospect_nodump

WORKDIR /prospect/tests/spectre-tests
RUN ./eval.py /proteus-base/sim/build/base /prospect/sim/build/prospect

WORKDIR /prospect/tests/synthetic-benchmark
RUN if [ "${BENCHMARKS}" = "true" ] ; then ./eval.py /proteus-base/sim/build/base_nodump /prospect/sim/build/prospect_nodump ; else echo Skipping benchmarks... ; fi

CMD /bin/bash
