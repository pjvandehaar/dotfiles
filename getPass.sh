#!/usr/bin/env python

import random

k = 8
words = ['\n']*k

for i, word in enumerate(open('/usr/share/dict/words')):
    if random.randint(0,i) < k:
        words[random.randint(0,k-1)] = word.strip()

print('\n'.join(words))
