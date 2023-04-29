#!/usr/bin/env python3

import os
import sys
import subprocess
import re


def create_simulation(mode, config):
    print(f"creating with mode {mode} and config {config}")
    os.system(f"sed \"s/#define mode.*/#define mode {mode}/g\" -i specBench.c")
    os.system(
        f"sed \"s/#define input.*/#define input \\\"{config}\\\"/g\" -i specBench.c")
    os.system("make specBench.bin")
    os.system(f"mv specBench.bin specBench_{mode}_{config}.bin")


def run_simulation(binary, mode, config):
    return {
        'process': subprocess.Popen([binary, f"specBench_{mode}_{config}.bin"]),
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
        '10': 0,
        '25': 0,
        '50': 0,
        '75': 0,
    },
    '1': {
        '10': 0,
        '25': 0,
        '50': 0,
        '75': 0,
    },
    '2': {
        '10': 0,
        '25': 0,
        '50': 0,
        '75': 0,
    },
}

CONFIGS = [
    "10",
    "25",
    "50",
    "75",
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
