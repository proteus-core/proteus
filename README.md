# Proteus: a configurable RISC-V core

For a quick introduction to Proteus, and to cite this work, please check [our extended abstract](https://mici.hu/papers/bognar23proteus.pdf) presented at the RISC-V Summit Europe '23.

```bibtex
@inproceedings{bognar23proteus,
  author    = {Bognar, Marton and Noorman, Job and Piessens, Frank},
  title     = {Proteus: An Extensible RISC-V Core for Hardware Extensions},
  booktitle = {{RISC-V} Summit Europe '23},
  month     = jun,
  year      = 2023
}
```

## Overview

Proteus is an easily configurable RISC-V CPU implemented in SpinalHDL.
It uses a plugin system to make the processor easily configurable and extensible, making the development and testing of hardware features easier. It already comes with easily replaceable or modifiable components such as branch predictors.
Proteus supports the RV32IM instruction set, which includes multi-cycle instructions (e.g., MUL or blocking memory accesses in IF/MEM) and the Machine-Level ISA (CSRs, traps).

The processor is implemented on an SoC, which features integration with AXI4 and APB buses and simple peripherals (machine timer, UART, ...).

Proteus can be configured either in a classic 5-staged static pipelined mode or as a superscalar out-of-order processor with a configurable number of reorder buffer entries and execution units.
Both implementations pass the [riscv-tests](https://github.com/riscv/riscv-tests), and the correctness of the static pipeline has been verified using [riscv-formal](https://github.com/SymbioticEDA/riscv-formal).

## Releases and extensions

Proteus uses [calendar versioning](https://calver.org/) with a suffix of `-I` and `-O` to differentiate between the in-order and the out-of-order pipeline implementation.
The following table describes the released versions and extensions forking them.

| Version                                                                  | Extension(s)                                                                                                                                                                                                                                                               |
|:-------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [`25.08`](https://github.com/proteus-core/proteus/releases/tag/v25.08)   |                                                                                                                                                                                                                                                                            |
| [`25.04`](https://github.com/proteus-core/proteus/releases/tag/v25.04)   |                                                                                                                                                                                                                                                                            |
| [`24.01`](https://github.com/proteus-core/proteus/releases/tag/v24.01)   | [Libra: Architectural Support For Principled, Secure And Efficient Balanced Execution On High-End Processors](https://github.com/proteus-core/libra) (only for `24.01-O`)                                                                                                  |
| [`23.03`](https://github.com/proteus-core/proteus/releases/tag/v23.03)   | [Architectural Mimicry: Innovative Instructions to Efficiently Address Control-Flow Leakage in Data-Oblivious Programs](https://github.com/proteus-core/ami) (both `23.03-I` and `23.03-O`)                                                                                |
| [`23.02`](https://github.com/proteus-core/proteus/releases/tag/v23.02)   | [ProSpeCT: Provably Secure Speculation for the Constant-Time Policy](https://github.com/proteus-core/prospect) (only for `23.02-O`), [CHERI-Crypt: Transparent Memory Encryption on Capability Architectures](https://github.com/cap-tee/cheri-crypt) (only for `23.02-I`) |
| [`21.08-I`](https://github.com/proteus-core/proteus/releases/tag/v21.08) | Hardware capabilities (CHERI): `src/main/scala/riscv/plugins/cheri`, providing the basis for the following publication: [CHERI-TrEE: Flexible enclaves on capability machines](https://github.com/proteus-core/cheritree).                                                 |

## Working with Proteus

Starting from release v25.09, Proteus should be used together with the central [ecosystem](https://github.com/proteus-core/ecosystem) repository, which contains all the software components necessary to use the core for research.
You can find detailed installation instructions, scripts, and a Docker-based setup in that repository.

## Building software

To run software on Proteus, it needs to be compiled by a [RISC-V toolchain](https://github.com/riscv-collab/riscv-gnu-toolchain#installation-newlib) with Newlib support (this is automatically installed in the ecosystem).
Newlib is an implementation of `libc` for embedded devices. Our board support package and an example project can be found in the ecosystem.

### Memory layout

The default binary configuration used in Proteus has the following memory layout:

- `0x80000000`: 10 MiB (configurable in `Core.scala`) of RAM initialized with the given memory file
- `0x20000000`: `mtime` register
- `0x20000008`: `mtimecmp` register
- `0x10000000`: Custom character device

The CPU starts executing at `0x80000000`.

The custom character device will print any stored ASCII character to the console.
When the ASCII `EOT` (`0x4`) is stored, the simulation is halted.

## Contributing

Pull requests and issue submissions are welcome! If you would like to get in contact with the developer team, please write to us at `marton.bognar@kuleuven.be`.
