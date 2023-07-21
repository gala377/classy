#!python3

import argparse
import os
import subprocess
from pathlib import Path

CLASSY_PATH = Path(__file__).parent.parent

COMPILE_COMMAND_FMT = [
    "cargo",
    "run",
    "-p=classy_example",
    "--",
    "--example=run-file",
]


def run_classy_c(file: str, rest: list[str]):
    cwd = os.getcwd()
    path = Path(cwd) / file
    command = [*COMPILE_COMMAND_FMT, f"--file={path}", *rest]
    print(f"Running {command}")
    subprocess.run(command, cwd=CLASSY_PATH)


def make_argparser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "file",
        type=str,
        help="Path to the file to be run",
    )
    parser.add_argument(
        "rest",
        nargs="*",
        help="rest of the args, passed directly to the binary",
    )
    return parser


def main():
    parser = make_argparser()
    args = parser.parse_args()
    run_classy_c(args.file, args.rest)


if __name__ == "__main__":
    main()
