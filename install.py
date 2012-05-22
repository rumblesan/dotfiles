#!/usr/bin/env python

from glob import glob
from os.path import isdir, lexists, islink
from os import getenv, unlink, rmdir, symlink, getcwd
import os
import os.path
from  sys import argv


def main(argv):
    if len(argv) < 2:
        argv.append("install")
    home = getenv("HOME")
    files = glob('*.symlink')
    for filename in files:
        link = ".%s" % filename.split(".")[0]
        linkPath = os.path.join(home, link)
        if lexists(linkPath):
            removeFile(link)
        if argv[1] == "install":
            linkFile(filename, link)

def removeFile(link):
    home = getenv("HOME")
    target = os.path.join(home, link)
    print("removing %s" % target)
    if islink(target):
        unlink(target)
    elif isdir(target):
        rmdir(target)
    else:
        unlink(target)

def linkFile(filename, target):
    dotfilesDir  = getcwd()
    filename = os.path.join(dotfilesDir, filename)
    home = getenv("HOME")
    os.chdir(home)
    print("symlinking %s -> %s" % (filename, target))
    symlink(filename, target)
    os.chdir(dotfilesDir)

if __name__ == "__main__":
    main(argv)

