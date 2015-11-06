#!/usr/bin/env python3

# -*- mode: python -*-

import bs4
import sys

mess_of_input = sys.stdin.read()

if len(sys.argv) > 1 and sys.argv[1] == '--xml':
    soup = bs4.BeautifulSoup(mess_of_input, features='xml')
else:
    soup = bs4.BeautifulSoup(mess_of_input, 'html.parser')

print(soup.prettify())
