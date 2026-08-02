#!/usr/bin/env python3

# TODO: Make generalized `ptr-yt`.
#       It checks that we're not &radio=1, or if we are it handles it correctly.
#       It encodes to m4a (aac).
#       It avoids writing `egg.m4a.m4a` (like `yt-dlp --output=egg.m4a` does).
#       It asks whether to upload.  It does mkdir -p.
#          (like `rsync -s --mkpath`, if you have modern versions on client+server)
#       It uses mutagen for AAC (bc eyeD3 is only mp3).


import subprocess as subp, sys, os, datetime, argparse
from pathlib import Path


def print_and_run(cmd:list[str], capture_output=False) -> None:
    dt_str = datetime.datetime.now().strftime('%H:%M:%S')
    assert isinstance(cmd, list)
    cmd = [str(x) for x in cmd]
    print(f'@{dt_str}', cmd)
    subp.run(cmd, check=True)

def pathsafe(s:str) -> str:
    return s.replace('/','_').replace(':','_').replace('@','_')


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('url')
    parser.add_argument('--title', required=True)
    parser.add_argument('--time-range')
    parser.add_argument('--artist', default='Kid Songs')
    parser.add_argument('--album', default='Kid Songs')
    parser.add_argument('--track', type=int)
    args = parser.parse_args()
    print(f'{args.url=}  {args.title=}  {args.time_range=}  {args.artist=}')

    assert 'radio' not in args.url
    assert 'list' not in args.url

    Path('/tmp/x.mp3').unlink()

    cmd = ['yt-dlp', '-x','--audio-format=mp3', '--output=/tmp/x.mp3']
    if args.time_range: cmd.append(f'--download-sections=*{args.time_range}')
    cmd.append(args.url)
    print_and_run(cmd)

    # TODO: Add 1s fade-in/out via ffmpeg

    cmd = ['eyeD3', f'--artist={args.artist}', f'--album={args.album}', f'--title={args.title}', '/tmp/x.mp3']
    if args.track: cmd.append(f'--track={args.track}')
    print_and_run(cmd)

    print_and_run(['scp', '/tmp/x.mp3', f'kpa@petervh.com:/var/www/html/x/music/Kid Songs/Kid Songs/{pathsafe(args.title)}.mp3'])
