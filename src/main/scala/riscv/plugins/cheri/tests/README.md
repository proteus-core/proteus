# README

_Disclaimer: the information below might not be entirely accurate, any suggestion and improvement is welcome._

In order to run the tests in this folder, a CHERI-aware compiler is needed. The easiest way to install one is through using [`cheribuild`](https://github.com/CTSRD-CHERI/cheribuild) and then running the command `./cheribuild llvm-native`. <!-- Maybe ./cheribuild newlib-baremetal-riscv32-hybrid ? -->

The file `Makefile.include` must then be modified so that `CC` points to the installed compiler. 
By default, this should be `~\cheri\output\sdk\bin\clang`.

Additionally, the RISC-V toolchain mentioned [here](https://github.com/proteus-core/proteus#building-software) is also required. <!-- Why though ? -->
Make sure it is present in your `PATH`.

The tests can then finally be run from the root directory using `make -C tests/ CUSTOM_TESTS_DIR=../src/main/scala/riscv/plugins/cheri/tests/`.
The files `Pcc.S` and `Scr.S` currently fail to compile for an unknown reason.