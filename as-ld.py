#!/usr/bin/env python

import sys, os
# MODIFY ACCORDINGLY
clang_exe = "clang-4.0"
def usage ():
	print "Usage: %s <file.s>" % sys.argv[0]
	exit(1)

if (len(sys.argv) < 2):
	usage ()
filename, file_ext = os.path.splitext(sys.argv[1])
if (file_ext != ".s"):
	usage ()
else:
	pref = os.path.basename(filename)
	cmd = "%s %s.s lib/lib.a -o %s" % (clang_exe, pref, pref)
	os.system(cmd)
