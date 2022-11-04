#!/usr/bin/env python3

import os, sys
from pathlib import Path

def h(path: Path) -> str:
    '''Like str(path) but abbreviates ~/'''
    try: return f'~/{path.relative_to(Path.home())}'
    except Exception: return str(path)

def readlink(path: Path) -> Path: return Path(os.readlink(path))

# Gather symlinks and their targets
os_name = 'linux' if sys.platform.startswith('linux') else 'mac'
script_dir_path = Path(__file__).absolute().parent
symlink_dir_path = script_dir_path / 'symlinks'
symlink_os_dir_path = script_dir_path / os_name / 'symlinks'
assert symlink_dir_path.is_dir()
assert symlink_os_dir_path.is_dir()
print(f'=> Installing from [{symlink_dir_path}] and [{symlink_os_dir_path}]')
targets = list(symlink_dir_path.iterdir()) + list(symlink_os_dir_path.iterdir())
symlinks = [Path.home() / t.name.replace('%', '/') for t in targets]

# Sanity check
for tgt in targets: assert tgt.is_file(), tgt
for tgt in targets: assert not tgt.name.startswith('%')

# Create parent dirs
for sym in symlinks:
    if not sym.parent.is_dir():
        print(f'=> Creating {sym.parent}')
        sym.parent.mkdir(parents=True, exist_ok=True)

# Create symlinks (or verify them if they already exist)
errors = False
for tgt, sym in zip(targets, symlinks):
    if not sym.exists():
        sym.symlink_to(tgt)
        print(f'=> Made:   {h(sym):20}  ->  {h(tgt)}')
    elif sym.is_symlink() and readlink(sym) == tgt:
        print(f'=> Good:   {h(sym):20}  ->  {h(tgt)}')
    elif sym.is_symlink():
        print(f'=> Bad link: {h(sym):20}  ->  {h(readlink(sym))} instead of {h(tgt)}')
        errors = True
    else:
        print(f'=> Bad file: {h(sym)}')
        errors = True
print()

if errors:
    print('Failed!  Please remove the bad files/links.'); sys.exit(1)
