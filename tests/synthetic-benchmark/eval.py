#!/usr/bin/env python3

import os
import sys
import subprocess
import re

def filename(mode, config):
    return f"specBench_{mode}_{config}"


def create_simulation(mode, config):
    print(f"creating with mode {mode} and config {config}")
    os.system(f"sed \"s/#define mode.*/#define mode {mode}/g\" specBench.c > {filename(mode, config)}.c")
    os.system(
        f"sed \"s/#define input.*/#define input \\\"{config}\\\"/g\" -i {filename(mode, config)}.c")
    os.system(f"make {filename(mode, config)}.bin TARGET={filename(mode, config)}")


def run_simulation(binary, mode, config):
    return {
        'process': subprocess.Popen([binary, f"{filename(mode, config)}.bin"], stdout=subprocess.PIPE, encoding="utf8"),
        'mode': mode,
        'config': config,
    }


base_proteus = sys.argv[1]
secure_proteus = sys.argv[2]


MODES = [
    ("0", base_proteus,   "baseline"),  # baseline
    ("1", secure_proteus, "  p(key)"),  # precise boundaries
    ("2", secure_proteus, "  p(all)"),  # all secrets
]

results = {
    '0': {
        '75': 0,
        '50': 0,
        '25': 0,
        '10': 0,
    },
    '1': {
        '75': 0,
        '50': 0,
        '25': 0,
        '10': 0,
    },
    '2': {
        '75': 0,
        '50': 0,
        '25': 0,
        '10': 0,
    },
}

CONFIGS = [
    "75",
    "50",
    "25",
    "10",
]

for mode in MODES:
    for config in CONFIGS:
        create_simulation(mode[0], config)

processes = []

for mode in MODES:
    for config in CONFIGS:
        processes.append(run_simulation(mode[1], mode[0], config))


print("Waiting for processes...")
with open("benchmark_logs.txt", 'w') as logfile:
    for proc in processes:
        proc['process'].wait()
        logfile.write(f"Benchmark with mode = {proc['mode']} and config = {proc['config']}:\n")
        logfile.write(f"{'-' * 80}\n")
        for line in proc['process'].stdout:
            logfile.write(line)
            match = re.match(r"total time\s*:\[(\d+)\]", line)
            if match:
                results[proc['mode']][proc['config']] = int(match.group(1))

print("\t\t", "\t".join(CONFIGS))

for mode in MODES:
    print(mode[2] + '\t', end='')
    for config in CONFIGS:
        print(str(round(results[mode[0]][config] / results['0'][config] * 100)) + '%\t', end='')
    print()
