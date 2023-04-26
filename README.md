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

For instructions on how to install and run Proteus, we refer to [its repository](https://github.com/proteus-core/proteus) and our Dockerfile.

## Running the evaluation

Most of the evaluation steps are implemented in our Dockerfile, which can be run with:

```shell
docker build -t prospect .
docker run -i -t prospect
```

To run the evaluation of the hardware overheads, follow the steps outlined [here](https://github.com/proteus-core/proteus#synthesis), using the `Core.v` files obtained from the Docker container at `/proteus-base/Core.v` and `/prospect/Core.v`.
