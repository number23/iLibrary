#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re
import sys
from operator import itemgetter
from collections import defaultdict

pat = re.compile('From:\s+?.*<([\da-z_\.-]+@[\da-z\.-]+)>')
mails = defaultdict(int)
for line in open(sys.argv[1], 'rb'):
    if line[0] != 'F':
        continue
    mat = pat.match(line)
    if mat:
        mails[mat.group(1)] += 1

lst = sorted(mails.iteritems(), key=itemgetter(1), reverse=True)
for t in lst[:100 if 100 < len(lst) else len(lst)]:
    print '%s : %d' % (t[0].ljust(16), t[1])
