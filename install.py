#!/usr/bin/env python

from glob import glob
import os
from sys import argv


def main(argv):
    if len(argv) < 2:
        argv.append("install")
    home = os.getenv("HOME")
    files = glob('*.symlink')
    for filename in files:
        link = ".%s" % filename.split(".")[0]
        linkPath = os.path.join(home, link)
        if os.path.lexists(linkPath):
            removeFile(link)
        if argv[1] == "install":
            linkFile(filename, link)

def removeFile(link):
    home = os.getenv("HOME")
    target = os.path.join(home, link)
    print("removing %s" % target)
    if os.path.islink(target):
        os.unlink(target)
    elif os.path.isdir(target):
        os.rmdir(target)
    else:
        os.unlink(target)

def linkFile(filename, target):
    dotfilesDir  = os.getcwd()
    filename = os.path.join(dotfilesDir, filename)
    home = os.getenv("HOME")
    os.chdir(home)
    print("symlinking %s -> %s" % (filename, target))
    os.symlink(filename, target)
    os.chdir(dotfilesDir)

if __name__ == "__main__":
    main(argv)

