# A configurable RISC-V core

## Features

- RV32I(M)
- Classic 5-stage RISC pipeline
  - Fully bypassed (interlock only when needed)
  - Can be disabled (5 CPI)
- Formally verified using [riscv-formal](https://github.com/SymbioticEDA/riscv-formal)
- Tested using [riscv-tests](https://github.com/riscv/riscv-tests)
- Support for multi-cycle instructions (e.g., MUL or blocking memory accesses in IF/MEM)
- Machine-Level ISA (CSRs, traps)
- SoC:
  - Integration with AXI4 and APB buses
  - Simple peripherals (Machine Timer, UART,. . . )
- Very easy to configure and extend using a plugin system

## Docker

To simplify setting up the toolchain, Docker can be used to boot into a shell with tools:

```
sudo apt install docker.io
sudo usermod -aG docker $(whoami) 
docker build -t proteus .
docker run -i -h "proteus" -t proteus
```

## Build and simulation prerequisites

- OpenJDK 11 (Ubuntu: openjdk-11-jdk)
- [SBT](https://www.scala-sbt.org/download.html)
- [Verilator](https://www.veripool.org/wiki/verilator) (Ubuntu: verilator)
- An RV32I(M) [toolchain](https://github.com/riscv/riscv-gnu-toolchain)

## Building and running simulations

First, make sure this repository is recursively cloned:

```
git clone --recurse-submodules git@gitlab.com:ProteusCore/ProteusCore.git
```

The default configuration uses Verilog arrays to simulate memories which need to be initialized with an Intel HEX file.
We assume this file is called `mem.ihex` (see [later](#building-software) for instructions on creating such files).

To build the default CPU configuration:

```
sbt 'runMain riscv.Core mem.ihex'
```

This produces a file called `Core.v` which contains the full design.
In principle, you could now use any Verilog simulator to run a simulation.

However, we can also run a simulation directly from Scala:

```
sbt 'runMain riscv.CoreSim mem.ihex'
```

Press Ctrl-C to exit the simulation (or write `0x4` to `0xf0002000` to halt the simulation from software).
A VCD file should have been created at `simWorkspace/Core/test.vcd` which can be viewed with any wave viewer (e.g., [GTKWave](http://gtkwave.sourceforge.net/)).

This setup works best while developing the HDL because it automatically rebuilds everything.
However, when developing software, this can be time consuming and a better option is to build a standalone simulator first:

```
make -C sim
```

This will create an executable at `sim/build/sim` which can be called with a flat binary file (see [later](#building-software)) as input:

```
sim/build/sim mem.bin
```

This will create a VCD file called `sim.vcd`.

## Building software

The following basic information should be enough to correctly configure the toolchain to build software.
A more detailed explanation will follow later.

The default configuration used above has the following memory map:

- `0x80000000`: 10 MiB of RAM initialized with the given memory file
- `0x20000000`: `mtime` register
- `0x20000008`: `mtimecmp` register
- `0x10000000`: Custom character devive

The CPU starts executing at `0x80000000`.

The custom character device will print any stored ASCII character to the console.
When ASCII `EOT` (`0x4`) is stored, simulation is halted.

Once an ELF file has been created, an Intel HEX or flat binary file can be created as follows:

```
riscv32-unknown-elf-objcopy -O ihex mem.elf mem.ihex
riscv32-unknown-elf-objcopy -O binary mem.elf mem.bin
```

## Running the tests

To run the [riscv-tests](https://github.com/riscv/riscv-tests) unit tests (make sure the RISC-V toolchain is in your `PATH`):

```
make -C tests
```

To run [riscv-formal](https://github.com/SymbioticEDA/riscv-formal), first install its [prerequisites](https://symbiyosys.readthedocs.io/en/latest/quickstart.html#installing) and make sure all the tools are in your `PATH`.
Then, run the following (which will take several hours to complete):

```
make -C formal -j<n>
```
