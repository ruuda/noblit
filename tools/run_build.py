#!/usr/bin/env python3

# Noblit -- An immutable append-only database
# Copyright 2020 Ruud van Asseldonk

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# A copy of the License has been included in the root of the repository.

"""
Build automation for Noblit.

Noblit consists of many parts. In particular the Rust crate, but also the client
libraries, golden tests, docs, and various supporting scripts. Fully running all
quality checks involves many steps. CI runs all of them, but to individually run
all of them locally is tedious. This script automates that.

This script is not a build system: it runs a dumb sequence of commands, it does
not track dependencies, nor does it avoid running unnecessary steps. This makes
it easier to write (no need to specify dependencies), but slower to run (it may
do useless work).

This script will by default build the Nix development environment and enter it,
if it is not already on the PATH. This can take a second or so, even when it is
a no-op. For lower latency, enter the environment once with "nix run --command
$SHELL", and start ./build inside that shell.

Usage:

  ./build
  ./build RULE...

  By default, all rules are executed. If any arguments are provided, only the
  matching rules get executed.

Flags:

  -h --help          Show this text.
     --no-isolation  Run in the existing environment, avoid Nix.
"""

from __future__ import annotations

import os
import re
import subprocess
import sys
import time

from typing import Dict, Iterable, List, NamedTuple, Optional, Set


class Result(NamedTuple):
    step: Step
    duration_seconds: float
    exit_code: int


class Step(NamedTuple):
    rule: str
    command: List[str]
    directory: Optional[str]

    def run(self) -> Result:
        if self.directory is not None:
            cwd = f'{os.getcwd()}/{self.directory}'
        else:
            cwd = os.getcwd()

        print('\n$', ' '.join(self.command))

        begin_second = time.monotonic()
        completed_proc = subprocess.run(self.command, cwd=cwd)
        end_second = time.monotonic()

        return Result(self, end_second - begin_second, completed_proc.returncode)


def parse_buildfile(buildfile: Iterable[str])-> Iterable[Step]:
    directory: Optional[str] = None
    current_rule: str = ''

    for lineno, line in enumerate(buildfile):

        # Skip comments and blank lines.
        if line.strip() == '' or line.strip().startswith('#'):
            continue

        # A colon marks a build rule, which resets the working directory.
        elif line.strip().endswith(':'):
            current_rule = line.strip().rstrip(':')
            directory = None

        # An "cd" command sets the working directory.
        elif line.startswith('  cd '):
            _cd, path = line.strip().split()
            directory = path

        # Any other command, we execute. We split args on spaces, until now
        # quoting has not been necessary. And at that point, it is probably
        # better to have proper lists (maybe json) instead of an ad-hoc format.
        elif line.startswith('  '):
            cmd = line.strip().split()
            yield Step(current_rule, cmd, directory)

        else:
            print(f'Build: unknown directive at line {lineno + 1}: {line}')
            sys.exit(1)


def get_devenv_path() -> str:
    """
    If a Nix development environment is on the PATH, return it. Otherwise invoke
    Nix to build the development environment, and return its path.
    """
    match = re.search(r'/nix/store/[0-9a-z]{32}-noblit-devenv', os.environ['PATH'])

    if match is not None:
        return match.group(0)

    else:
        print("$ nix-build")
        cmd = ['nix-build', '--no-out-link']
        return subprocess.run(cmd, capture_output=True).stdout.decode('utf-8').strip()


def get_dev_environment() -> Dict[str, str]:
    """
    Set up the PATH and other environment variables to make the tools from the
    Nix development environment available, and nothing else.
    """
    devenv = get_devenv_path()
    return {
        'LANG': 'en_US.UTF8',
        # Binaries in the profile built above may need locales, that they can't
        # find unless we point LOCALE_ARCHIVE at the archive that contains them.
        'LIBRARY_PATH': f'{devenv}/lib',
        'LOCALE_ARCHIVE': f'{devenv}/lib/locale/locale-archive',
        # Prevent another reexec by setting NOBLIT_DEVENV.
        'NOBLIT_DEVENV': '1',
        'PATH': f'{devenv}/bin',
        'HOME': os.environ['HOME'],
        'TERM': os.environ['TERM'],
    }


def main() -> None:
    if '--help' in sys.argv or '-h' in sys.argv:
        print(__doc__.strip())
        sys.exit(0)

    args = [arg for arg in sys.argv[1:] if not arg.startswith('-')]
    buildfile = args[0]
    rules = args[1:]
    results = []

    for step in parse_buildfile(open(buildfile, 'r', encoding='utf-8')):
        if len(rules) == 0 or step.rule in rules:
            results.append(step.run())

    print()
    for result in results:
        cmd = ' '.join(result.step.command)
        if result.exit_code == 0:
            status = '\x1b[32m PASS\x1b[0m'
        else:
            status = '\x1b[31m FAIL\x1b[0m'

        print(f'{status}  {result.duration_seconds:3.1f}s  {cmd}')

    sys.exit(max(result.exit_code for result in results))


if __name__ == '__main__':
    if os.getenv('NOBLIT_DEVENV') == '1':
        main()
    elif '--no-isolation' in sys.argv:
        sys.argv.remove('--no-isolation')
        main()
    else:
        new_environment = get_dev_environment()
        os.execve(sys.argv[0], sys.argv, new_environment)
