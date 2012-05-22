#!/usr/bin/env python

from glob import glob
import os
from sys import exit

import re
from ConfigParser import SafeConfigParser
from argparse import ArgumentParser

regexp = re.compile(r"@%\{(.+?)}%@")

def parseArgs():
    parser = ArgumentParser(description='Setup dotfiles')
    parser.add_argument('command', metavar='command', default='install',
                        choices=['install', 'uninstall'],
                        help='command to run')
    parser.add_argument('-k', action='store', nargs=1, dest='keys',
                        metavar='keys', help='keys file to use for templating')

    args =  parser.parse_args()
    keys = {}

    if args.keys:
        keyParser = SafeConfigParser()
        keyParser.read(args.keys)
        keyList = keyParser.items('keys')
        for k, v in keyList:
            keys[k] = v

    return (args, keys)

def main():

    args, keys = parseArgs()

    home = os.getenv("HOME")
    files = glob('*.symlink')
    for filename in files:
        link = ".%s" % filename.split(".")[0]
        linkPath = os.path.join(home, link)
        if os.path.lexists(linkPath):
            removeFile(link)
        if args.command == "install":
            linkFile(filename, link)

def template(template, keys):

    outName = os.path.splitext(os.path.split(inFile)[-1])[0]
    outFile = os.path.join('compiled', outName)

    rf = open(template)
    of = open(outFile, 'w')

    for line in rf:
        matches = regexp.search(line)
        if matches:
            matches = list(matches.groups())
            for match in matches:
                replacement = keys[match]
                of.write(regexp.sub(replacement, line))
        else:
            of.write(line)

    rf.close()
    of.close()

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
    main()

