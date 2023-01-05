# Proteus: a configurable RISC-V core

Proteus is an easily configurable RISC-V CPU implemented in SpinalHDL.
It uses a plugin system to make the processor easily configurable and extendible, making the development and testing of hardware features easier. It already comes with easily replacable or modifiable components such as branch predictors.
Proteus supports the RV32IM instruction set, which includes multi-cycle instructions (e.g., MUL or blocking memory accesses in IF/MEM) and the Machine-Level ISA (CSRs, traps).

The processor is implemented on a SoC, which features integration with AXI4 and APB buses and simple peripherals (machine timer, UART, ...).

Proteus can be configured either in a classic 5-staged static pipelined mode, or as a superscalar out-of-order processor with a configurable number of reorder buffer entries and execution units.
Both implementations pass the [riscv-tests](https://github.com/riscv/riscv-tests), and the correctness of the static pipeline has been verified using [riscv-formal](https://github.com/SymbioticEDA/riscv-formal).

## Contributing

Pull requests and issue submissions are welcome! If you would like to get in contact with the developer team, please write to us at `marton.bognar@kuleuven.be`.

## Current (security) extensions on Proteus

- Hardware capabilities (CHERI): `src/mail/scala/riscv/plugins/cheri`, under submission

## Working with Proteus

### Hardware and simulations

First, make sure this repository is recursively cloned:

```
git clone --recurse-submodules https://github.com/proteus-core/proteus
```

Simulations can be run either locally or using the prebuilt Docker container.
A local setup is recommended if you're planning to compile your software to run on Proteus.

There are different ways of building the hardware implementation and running simulations on it. In the following, these methods will be explained. In general, if you intend to use the dynamic, out-of-order pipeline instead of the 5-stage static pipeline, substitute `riscv.Core` with `riscv.CoreDynamic` in the instructions.

#### Local prerequisites

- OpenJDK and Verilator: `apt install openjdk-17-jdk verilator`
- sbt: https://www.scala-sbt.org

#### Docker

To run a minimal complete installation of the core in Docker (without a RISC-V toolchain):

```shell
docker build -t proteus .
docker run -i -h "proteus" -t proteus
```

#### Hardcoding memory in Verilog

The default configuration uses Verilog arrays to simulate program and data memories which need to be initialized with an Intel HEX file.

```
sbt 'runMain riscv.Core mem.ihex'
```

This command builds the default CPU configuration with the supplied `mem.ihex` memory file and outputs a `Core.v` Verilog design, which can be run with any Verilog simulator.

#### Running a simulation directly from Scala

It is also possible to build the deisgn and run a simulation in a single sbt step (using Verilator):

```
sbt 'runMain riscv.CoreSim mem.ihex'
```

The simulation will be halted if the software writes the value `0x4` to the address `0xf0002000`, or can simply be stopped by pressing Ctrl-C. This method will generate a value change dump file at `simWorkspace/Core/test.vcd`, which can be viewed with any wave viewer (e.g., [GTKWave](http://gtkwave.sourceforge.net/)).

This setup works best while developing the HDL because it automatically rebuilds the hardware.

#### Generating a standalone simulator

However, when developing software, rebuilding the hardware for every simulation can be time consuming and a better option is to build a standalone simulator first:

```
make -C sim
```

This will create an executable at `sim/build/sim` which can be called with a flat binary file as input:

```
sim/build/sim mem.bin
```

This will create a VCD file called `sim.vcd` in the directory the simulation is ran from.

We provide a bare-bones `sim.gtkw` GTKWave savefile to examine this VCD file.
You might need to update the three hardcoded paths in `sim.gtkw` before using it.
Most importantly, this file uses the `res/disas.py` script to decode binary instructions into their textual representation for easier debugging.
This can also be loaded by right-clicking on an instruction signal (IR registers) and selecting it from `Data Format > Translate Filter Process > Enable and Select`.

### Building software

Running a simulation requires either an `.ihex` or a `.bin` file, as explained above. To generate these, a RISC-V toolchain with Newlib support is needed.
Newlib is an implementation of libc for embedded devices. Our implementation and a skeleton project can be found in our [Newlib repository](https://github.com/proteus-core/newlib).

The RISC-V toolchain can be installed by following the instructions [here](https://github.com/riscv-collab/riscv-gnu-toolchain#installation-newlib).
We use the following configuration option for the toolchain:

```shell
./configure --prefix=/opt/riscv --with-arch=rv32im --with-abi=ilp32
make
```

The default configuration used in Proteus has the following memory layout:

- `0x80000000`: 10 MiB of RAM initialized with the given memory file
- `0x20000000`: `mtime` register
- `0x20000008`: `mtimecmp` register
- `0x10000000`: Custom character device

The CPU starts executing at `0x80000000`.

The custom character device will print any stored ASCII character to the console.
When the ASCII `EOT` (`0x4`) is stored, the simulation is halted.

Once an ELF file has been created, an Intel HEX or flat binary file can be created as follows (see also the Newlib makefiles):

```
riscv32-unknown-elf-objcopy -O ihex mem.elf mem.ihex
riscv32-unknown-elf-objcopy -O binary mem.elf mem.bin
```

### Tests and verification

Run the [riscv-tests](https://github.com/riscv/riscv-tests) unit tests (make sure the RISC-V toolchain is in your `PATH`):

```
make -C tests
```

To run [riscv-formal](https://github.com/SymbioticEDA/riscv-formal), first install its [prerequisites](https://symbiyosys.readthedocs.io/en/latest/quickstart.html#installing) and make sure all the tools are in your `PATH`.
Then, run the following (which will take several hours to complete):

```
make -C formal -j<n>
```
