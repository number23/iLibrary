#!/usr/bin/env python
# -*- coding: utf-8 -*-

__all__ = ['ftw']

import os
import fnmatch


def ftw(root=os.getcwd(), patterns='*'):
    '''file tree walk'''

    pattern_list = patterns.split(';')
    for (path, dirs, files) in os.walk(root):
        for name in files:
            fullname = os.path.join(path, name)
            for pattern in pattern_list:
                if fnmatch.fnmatch(name, pattern):
                    yield fullname


if __name__ == '__main__':
    for name in ftw('/usr/include'):
        print name
