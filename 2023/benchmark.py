#!/usr/bin/env python3

import subprocess as sp
import os
from pathlib import Path
import json

MAIN_DIR = Path(__file__).resolve().parent
DATA_FILE = "data.txt"
CACHE_DIR = MAIN_DIR.joinpath(".benchmarks")
LANGUAGES = ["C", "Go", "Elixir", "Haskell", "Rust"]
RUNS = 10

def format_res(res):
    if res is None:
        return ""
    mean = res[0]["mean"]
    std = res[0]["stddev"]
    return f"{mean*1000:.1f} ± {std*1000:.1f} ms"

def run_benchmark(day, part):
    language = LANGUAGES[(day-1) // 5]
    day_dir = MAIN_DIR.joinpath(f"day{day}")
    command = ("mix run {}.exs" if language == "Elixir" else "./{}").format(part) + " " + DATA_FILE
    if not os.path.isdir(day_dir):
        return None

    output_file = str(CACHE_DIR.joinpath(str(day) + part + ".json"))
    if not os.path.isfile(output_file):
        try:
            sp.check_call(f"hyperfine --export-json {output_file} "
                          f"--runs {RUNS} --shell=none '{command}'",
                          cwd=day_dir, shell=True,
                          stdout=sp.DEVNULL, stderr=sp.DEVNULL)
        except sp.CalledProcessError:
            return None

    with open(output_file, "r") as file_stream:
        return json.load(file_stream)["results"]

if __name__ == "__main__":
    if not os.path.isdir(CACHE_DIR):
        os.mkdir(CACHE_DIR)

    for day in range(1, 26):
        print(f"Day {day}:")
        for part in ["1", "2"]:
            print(f"  Part {part}: {format_res(run_benchmark(day, 'part'+part))}")
        print()
