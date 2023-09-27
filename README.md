# Proteus: a configurable RISC-V core

For a quick introduction to Proteus, and to cite this work, please check [our extended abstract](https://mici.hu/papers/bognar23proteus.pdf) presented at the RISC-V Summit Europe '23.

## Get started with Proteus

- [Installation](#installing-proteus)
- [Building software](#building-software)
- [Running simulations](#running-simulations)
- [Synthesis](#synthesis)
- [Tests and verification](#tests-and-verification)

## Overview

Proteus is an easily configurable RISC-V CPU implemented in SpinalHDL.
It uses a plugin system to make the processor easily configurable and extensible, making the development and testing of hardware features easier. It already comes with easily replaceable or modifiable components such as branch predictors.
Proteus supports the RV32IM instruction set, which includes multi-cycle instructions (e.g., MUL or blocking memory accesses in IF/MEM) and the Machine-Level ISA (CSRs, traps).

The processor is implemented on an SoC, which features integration with AXI4 and APB buses and simple peripherals (machine timer, UART, ...).

Proteus can be configured either in a classic 5-staged static pipelined mode or as a superscalar out-of-order processor with a configurable number of reorder buffer entries and execution units.
Both implementations pass the [riscv-tests](https://github.com/riscv/riscv-tests), and the correctness of the static pipeline has been verified using [riscv-formal](https://github.com/SymbioticEDA/riscv-formal).

## Current extensions on Proteus

- ProSpeCT: Provably Secure Speculation for the Constant-Time Policy: <https://github.com/proteus-core/prospect>.
- Hardware capabilities (CHERI): `src/main/scala/riscv/plugins/cheri`, described in the [CHERI-TrEE](https://github.com/proteus-core/cheritree) work.
- CHERI-TrEE: Flexible enclaves on capability machines: <https://github.com/proteus-core/cheritree>.
- Architectural Mimicry (AMi): <https://github.com/proteus-core/ami>

## Contributing

Pull requests and issue submissions are welcome! If you would like to get in contact with the developer team, please write to us at `marton.bognar@kuleuven.be`.

## Installing Proteus

First, make sure this repository is recursively cloned:

```
git clone --recurse-submodules https://github.com/proteus-core/proteus
```

For getting familiar and experimenting with Proteus, we recommend working with our Docker container, which installs all prerequisites, including the RISC-V toolchain, and simulates the [Newlib](https://github.com/proteus-core/newlib) example code.
[See here](https://docs.docker.com/engine/install/) for instructions on how to install Docker.

The Docker container can be built using the following command:
```shell
docker build -t proteus .
```

Running the container afterwards is possible with:
```shell
docker run -i -h "proteus" -t proteus
```

If, instead of using Docker, you want to install Proteus locally, you can follow the installation steps from our [Dockerfile](./Dockerfile), adjusting them to your setup as needed:
- Install OpenJDK (tested with `openjdk-17-jdk`) and Verilator
- Install sbt: https://www.scala-sbt.org
- Install the RISC-V toolchain with Newlib support, selecting the `rv32im_zicsr` architecture: https://github.com/riscv/riscv-gnu-toolchain

## Building software

To run software on Proteus, it needs to be compiled by a [RISC-V toolchain](https://github.com/riscv-collab/riscv-gnu-toolchain#installation-newlib) with Newlib support (this is automatically installed with [Docker](#installing-proteus)).
Newlib is an implementation of `libc` for embedded devices. Our board support package and an example project can be found in our [Newlib repository](https://github.com/proteus-core/newlib), which is included in this project as a submodule.
This example program is compiled and run on Proteus in our Dockerfile.

### Memory layout

The default binary configuration used in Proteus has the following memory layout:

- `0x80000000`: 10 MiB of RAM initialized with the given memory file
- `0x20000000`: `mtime` register
- `0x20000008`: `mtimecmp` register
- `0x10000000`: Custom character device

The CPU starts executing at `0x80000000`.

The custom character device will print any stored ASCII character to the console.
When the ASCII `EOT` (`0x4`) is stored, the simulation is halted.

## Running simulations

There are different ways of building the hardware implementation and running simulations on it. In the following, these methods will be explained. In general, if you intend to use the dynamic, out-of-order pipeline instead of the 5-stage static pipeline, substitute `riscv.Core` with `riscv.CoreDynamic` in the instructions.

The most common way of running programs is to create a standalone Proteus simulator, which can be done with the following command:

```shell
make -C sim CORE=riscv.CoreExtMem
```

This creates an executable at `sim/build/sim` which can be called with a flat binary file as input:

```
./sim/build/sim program.bin
```

ELF files can be converted to a flat binary with `objcopy`:

```shell
riscv32-unknown-elf-objcopy -O binary program.elf program.bin
```

### Examining simulation output

Running a simulation the above way creates a VCD file called `sim.vcd` in the directory the simulation is run from.

We provide a bare-bones `sim.gtkw` GTKWave savefile to examine this VCD file.
You might need to update the three hardcoded paths in `sim.gtkw` before using it.
Most importantly, this file uses the `res/disas.py` script to decode binary instructions into their textual representation for easier debugging.
This script can also be loaded by right-clicking on an instruction signal (IR registers) and selecting it from `Data Format > Translate Filter Process > Enable and Select`.

### Alternative ways of running simulations

As opposed to the method described [above](#running-simulations), the following two methods do not require building a standalone simulator binary, they recompile the Scala design on each invocation (which is useful when making changes to the hardware).
These methods require the executable file to be converted to the Intel HEX format, which can also be accomplished with `objcopy`:

```shell
riscv32-unknown-elf-objcopy -O ihex program.elf program.ihex
```

It is possible to create a Verilog design (`Core.v`) that uses hardcoded arrays to store the program memory:

```shell
sbt 'runMain riscv.Core program.ihex'
```

The resulting `Core.v` file can subsequently be run with any Verilog simulator.
For convenience, we provide a shorthand that automatically runs the simulation with Verilator:

```shell
sbt 'runMain riscv.CoreSim mem.ihex'
```

This method will generate a value change dump file at `simWorkspace/Core/test.vcd` which can be examined as described [earlier](#examining-simulation-output).

## Synthesis

To synthesize the design for an FPGA, we use Xilinx Vivado.
The standard edition can be downloaded for free [here](https://www.xilinx.com/products/design-tools/vivado/vivado-ml.html).
These instructions were tested with version 2022.2.

Follow these steps to create a project with Proteus and run the synthesis:

0. Make sure that you have a `Core.v` file in the root directory of this project (this can be generated by running `make sim`, [copied from the Docker container](https://stackoverflow.com/a/22050116) if needed).
1. Launch Vivado, and start the Create Project wizard.
2. Choose the project name and location as desired.
3. Project type: RTL Project.
4. Add sources: select `Core.v` and `synthesis/Top.v`. Do **not** check "Copy sources into project" or "Scan and add RTL include files into project".
5. Add constraints: select `synthesis/Constraints.xdc`.
6. Default part: select your target FPGA, e.g., `xc7a50ticsg324-1L`. Proteus requires at least 186 I/O ports.
7. Finish the wizard.
8. When the project is open, if `Top.v` is not selected as the top module (shown in bold), right-click on it and "Set as Top".
9. If needed, change the timing constraint in Constraints.xdc or regenerate `Core.v` by running `make sim`.
10. Run Implementation

After the first run, the project can be opened from Vivado and the last two steps can be repeated to obtain up-to-date measurements.

## Tests and verification

Run the [riscv-tests](https://github.com/riscv/riscv-tests) unit tests (make sure the RISC-V toolchain is in your `PATH`):

```
make -C tests CORE=riscv.CoreExtMem
```

To run [riscv-formal](https://github.com/SymbioticEDA/riscv-formal), first install its [prerequisites](https://symbiyosys.readthedocs.io/en/latest/quickstart.html#installing) and make sure all the tools are in your `PATH`.
Then, run the following (which will take several hours to complete):

```
make -C formal -j<n>
```
