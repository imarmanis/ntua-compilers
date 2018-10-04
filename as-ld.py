#!/usr/bin/env python3

import sys, os, subprocess

# MODIFY ACCORDINGLY IF NEEDED
clang_exe = "clang"

def usage ():
	print ("Usage: %s <file.s>" % sys.argv[0])
	exit(1)

if (len(sys.argv) < 2):
	usage ()
filename, file_ext = os.path.splitext(sys.argv[1])
if (file_ext != ".s"):
	usage ()
else:
	pref = os.path.basename(filename)
	cmd = "%s %s.s lib/lib.a -o %s" % (clang_exe, pref, pref)
	subprocess.check_call(cmd, shell=True)
