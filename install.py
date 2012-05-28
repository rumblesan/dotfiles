#!/usr/bin/env python

import os
import re
from glob import glob
from argparse import ArgumentParser
from sys import exit

regexp = re.compile(r"@%\{(.+?)}%@")

def parseArgs():
    parser = ArgumentParser(description='Setup dotfiles')
    parser.add_argument('command', metavar='command', default='install',
                        choices=['install', 'uninstall'],
                        help='command to run')

    return parser.parse_args()

def main():

    args = parseArgs()

    dotfileDir = os.path.join(os.getenv("HOME"), '.dotfiles')
    if dotfileDir != os.getcwd():
        print("This script must be run from %s" % dotfileDir)
        exit(1)

    files = glob('%s/*.symlink' % dotfileDir)
    for filename in files:
        linkName = ".%s" % os.path.splitext(os.path.split(filename)[-1])[0]
        linkPath = os.path.join(os.getenv("HOME"), linkName)
        if os.path.lexists(linkPath):
            print("removing %s" % linkPath)
            removeFile(linkPath)
        if args.command == "install":
            print("symlinking %s -> %s" % (filename, linkPath))
            os.symlink(filename, linkPath)

def removeFile(linkPath):
    if os.path.islink(linkPath):
        os.unlink(linkPath)
    elif os.path.isdir(linkPath):
        os.rmdir(linkPath)
    else:
        os.unlink(linkPath)

if __name__ == "__main__":
    main()

