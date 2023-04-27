import os
import sys


def run_simulation(binary, mode, config):
    os.system(f"sed \"s/#define mode*/#define mode {mode}/g\"")
    os.system(f"sed \"s/#define input*/#define input \\\"{config}\\\"/g\"")


base_proteus = sys.argv[1]
secure_proteus = sys.argv[2]


MODES = [
    (0, base_proteus),  # baseline
    (1, secure_proteus),  # precise boundaries
    (2, secure_proteus),  # all secrets
]

CONFIGS = [
    "10",
    "25",
    "50",
    "75",
]

for mode in MODES:
    for config in CONFIGS:
        run_simulation(mode[1], mode[0], config)
