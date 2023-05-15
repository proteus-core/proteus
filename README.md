# ProSpeCT: Provably Secure Speculation for the Constant-Time Policy

This repository contains the implementation of our defense on the Proteus RISC-V core, along with the tests used for the evaluation in the paper.

```
@inproceedings{daniel23prospect,
  author    = {Daniel, Lesly-Ann and Bognar, Marton and Noorman, Job and Bardin, Sébastien and Rezk, Tamara and Piessens, Frank},
  title     = {ProSpeCT: Provably Secure Speculation for the Constant-Time Policy},
  booktitle = {32nd USENIX Security Symposium (USENIX Security 23)},
  month     = aug,
  year      = {2023}
}
```

For detailed instructions on how to install and use Proteus, we refer to our Dockerfile and the [Proteus repository](https://github.com/proteus-core/proteus).

## Prerequisites

- [Docker](https://docs.docker.com/engine/install/) for installing the core and running the benchmarks.
  + Disk space required for the built container image: 12 GB.
- [Xilinx Vivado](https://www.xilinx.com/products/design-tools/vivado/vivado-ml.html) for running the hardware cost measurements.
  + Requires a Xilinx/AMD account.
  + Select Product to Install: Vivado (ML Standard).
  + Customizing the installation: Vivado Design Suite and the 7 Series Production Devices are sufficient, everything else can be deselected.
  + Disk space required: 52 GB.

## Running the evaluation

Most of the evaluation steps are implemented in our Dockerfile, which can be run with:

```shell
docker build -t prospect .
docker run -i -t prospect
```

By default, the build process only runs the security tests.
The performance benchmarks can also be run already during the build by specifying the `BENCHMARKS=true` build argument (this will add 9+ hours to the build process):

```shell
docker build -t prospect --build-arg BENCHMARKS=true .
```

### Security tests

The security tests (see start of Section 6.2 in the paper) can be found in `/prospect/tests/spectre-tests/` and can be run with `./eval.py /proteus-base/sim/build/base /prospect/sim/build/prospect`, where the two arguments are the simulator binaries for the base and the ProSpeCT-enabled implementation.

Expected output:

```
TEST secret-before-branch
SECURE VARIANT:  	✔ Secret did not leak!
INSECURE VARIANT:	🗲 Secret leaked!

TEST secret-after-branch
SECURE VARIANT:  	✔ Secret did not leak!
INSECURE VARIANT:	🗲 Secret leaked!
```

### Performance benchmarks

Our performance benchmark (see Section 6.2, "Runtime overhead" and Table 1) is located in `/prospect/tests/synthetic-benchmark` and can be run, similar as above, with `./eval.py /proteus-base/sim/build/base_nodump /prospect/sim/build/prospect_nodump`.
This benchmark uses the simulators without VCD dumping, as the generated files would be hundreds of GB in size.

**Important:** Running all simulations takes a long time, on a consumer laptop this is around 9 hours, don't worry if you get no visible progress for a while when running the script.

Expected output:

```
           75      50      25      10
baseline  100%    100%    100%    100%
  p(key)  100%    100%    100%    100%
  p(all)  110%    125%    136%    145%
```

### Hardware overhead

First, save the synthesis files (`Constraints.xdc` and `Top.v`) from [this folder](https://github.com/proteus-core/proteus/tree/main/synthesis) somewhere on your machine.

Then, find the name of the `prospect` container (`stupefied_poincare` in this example):

```shell
$ docker ps -a
CONTAINER ID   IMAGE          COMMAND                  CREATED          STATUS                     PORTS     NAMES
21aa3b3408e6   prospect       "/bin/sh -c /bin/bash"   8 seconds ago    Exited (0) 2 seconds ago             stupefied_poincare

```

Using this name, we can copy the `Core.v` files containing the Verilog design for the base Proteus core and the ProSpeCT-enabled version to the host machine:

```shell
docker cp stupefied_poincare:/proteus-base/Core.v Core.v
docker cp stupefied_poincare:/prospect/Core.v Core-ProSpeCT.v
```

To run the evaluation of the hardware overheads (see Section 6.2, "Hardware cost"), first set up the Vivado project.

#### Creating the Vivado project

1. Launch Vivado, and start the Create Project wizard.
2. Choose the project name and location as desired.
3. Project type: RTL Project.
4. Add sources: select the copied `Core.v` and the saved `Top.v`. Do **not** check "Copy sources into project" or "Scan and add RTL include files into project".
5. Add constraints: select the saved `Constraints.xdc`.
6. Default part: select your target FPGA, the `xc7a35ticsg324-1L`.
7. Finish the wizard.
8. When the project is open, if `Top.v` is not selected as the top module (shown in bold), right-click on it and "Set as Top".

#### Running the Vivado evaluation

9. Change the timing constraint in `Constraints.xdc` to the value indicated in the paper (`30.1 ns`). As the synthesis process is not completely deterministic, you might need to increase this value slightly if the implementation fails with a failed timing.
10. Click "Run Implementation".
11. Replace `Core.v` with `Core-ProSpeCT.v` on your disk, change the timing constraint in `Constraints.xdc` to the value of ProSpeCT (`30.7 ns`) and rerun the implementation, again adjusting the constraint slightly if necessary.

#### Interpreting the results

When the implementation finishes, the relevant values for number of LUTs and registers (as reported in the paper) can be found on the "Design Runs" tab at the bottom of the screen.
If the timing constraint for the critical path is met, this is shown by a black (and positive) number under the WNS field in the same table, failed timings are indicated by an error and a red (negative) WNS number.
