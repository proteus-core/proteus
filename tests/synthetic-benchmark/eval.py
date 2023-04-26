import os

def run_simulation(binary, mode, config):
    os.system(f"sed \"s/#define mode*/#define mode {mode}/g\"")
    os.system(f"sed \"s/#define input*/#define input \\\"{config}\\\"/g\"")
