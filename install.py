#!/usr/bin/env python

from glob import glob
import os
from sys import argv

import re
from argparse import ArgumentParser

regexp = re.compile(r"@%\{(.+?)}%@")

def parseArgs():
    parser = ArgumentParser(description='Setup dotfiles')
    parser.add_argument('command', metavar='command', default='install',
                        choices=['install', 'uninstall'],
                        help='command to run')

    return parser.parse_args()

def main(argv):

    args = parseArgs()

    home = os.getenv("HOME")
    files = glob('*.symlink')
    for filename in files:
        link = ".%s" % filename.split(".")[0]
        linkPath = os.path.join(home, link)
        if os.path.lexists(linkPath):
            removeFile(link)
        if args.command == "install":
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

