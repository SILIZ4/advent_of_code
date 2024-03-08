#!/usr/bin/env python3

import subprocess as sp
import os
from pathlib import Path
import json
from itertools import chain

from matplotlib import pyplot as plt
import numpy as np

MAIN_DIR = Path(__file__).resolve().parent
DATA_FILE = "data.txt"
CACHE_DIR = MAIN_DIR.joinpath(".benchmarks")
LANGUAGES = ["C", "Go", "Elixir", "Haskell", "Rust"]
COLORS = {
    "C": "#555555",
    "Go": "#00ADD8",
    "Elixir": "#6E4A7E",
    "Haskell": "#5e5086",
    "Rust": "#DEA584"
}
RUNS = 10

def format_res(res):
    if res is None:
        return ""
    return f"{res[0]['mean']*1000:.1f} ± {res[0]['stddev']*1000:.1f} ms"

def run_benchmark(day, part, language):
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

    width = 0.15

    plt.figure(figsize=(8, 3))
    days = list(range(1, 26))
    for day in days:
        language = LANGUAGES[(day-1)//5]
        print(f"Day {day}:")
        for part, pos in zip([1, 2], [day-width, day+width]):
            result = run_benchmark(day, f"part{part}", language)
            print(f"  Part {part}: {format_res(result)}")
            plt.bar(pos,
                    0 if result is None else result[0]["mean"],
                    yerr=0 if result is None else result[0]["stddev"],
                    color=COLORS[language]+"99", ecolor=COLORS[language],
                    label=language if (day-1)%5 == 0 and part==1 else None,
                    width=2*width)
        print()
    plt.xticks(days)
    plt.gca().spines[["right", "top"]].set_visible(False)
    plt.xlabel("Day")
    plt.yscale("log")
    plt.ylabel("Time [ms]")
    plt.legend()
    plt.tight_layout()
    plt.show()
