#!/usr/bin/env python

print('\n'.join(__import__('random').sample(
    list(w for w in 
    open('/usr/share/dict/words').read().split()
    if w.islower()
    ),
    8)))
