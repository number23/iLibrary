#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import os


def lll(dirname):
    names = os.listdir(dirname)
    names.sort()
    for name in names:
        if name not in (os.curdir, os.pardir):
            full = os.path.join(dirname, name)
            if os.path.islink(full):
                print "%s -> %s" % (name.ljust(16), os.readlink(full))


def main():
    args = sys.argv[1:]
    if not args:
        args = [os.curdir]
    first = 1
    for arg in args:
        if len(args) > 1:
            if not first:
                print
            first = 0
            print arg + ':'
        lll(arg)


if __name__ == '__main__':
    main()
