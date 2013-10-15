#!/usr/bin/env python

import os,sys
import fnmatch
import string

class GlobDirectoryWalker:
    """ A forward iterator that traverses a directory tree """
    def __init__(self, directory, pattern="*"):
        self.stack = [directory]
        self.pattern = pattern
        self.files = []
        self.index = 0
    def __getitem__(self, index):
        while 1:
            try:
                file = self.files[self.index]
                self.index = self.index + 1
            except IndexError:
                # pop next directory from stack
                self.directory = self.stack.pop()
                self.files = os.listdir(self.directory)
                self.index = 0
            else:
                # got a filename
                fullname = os.path.join(self.directory, file)
                if os.path.isdir(fullname) and not os.path.islink(fullname):
                    self.stack.append(fullname)
                if fnmatch.fnmatch(file, self.pattern):
                    return fullname

if __file__ != os.path.basename(__file__):
    print "You don't seem to be in the CliMT root directory"
    sys.exit()

os.system('rm -fr build')

for file_name in GlobDirectoryWalker('.'):
    # never touch files in CVS directories
    if file_name.find('/CVS/') != -1:
        continue
    extension = os.path.splitext(file_name)[1]
    if extension in ['.o','.mod','.so','.lst','.ipo','.pyc'] \
    or file_name[-1] == '~' or '_lib.' in file_name:
        print file_name
        os.remove(file_name)
