#!/usr/bin/env python

from glob import glob
import os

import re
from ConfigParser import SafeConfigParser
from argparse import ArgumentParser

from sys import exit

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

    dotfileDir = os.path.join(os.getenv("HOME"), '.dotfiles')
    compiledDir = os.path.join(dotfileDir, 'compiled')
    os.chdir(dotfileDir)

    if args.command == "install":
        cleanup(compiledDir)
        findTemplates(compiledDir, keys)

    findSymlinks(args, dotfileDir)
    findSymlinks(args, compiledDir)

    if args.command == "uninstall":
        cleanup(compiledDir)

def cleanup(directory):
    files = glob('%s/*' % directory)
    for filename in files:
        os.unlink(filename)


def findTemplates(outputDir, keys):
    files = glob('*.tmplt')
    for filename in files:
        template(filename, outputDir, keys)

def template(filename, outputDir, keys):

    outName = os.path.splitext(os.path.split(filename)[-1])[0]
    outFile = os.path.join(outputDir, outName)

    rf = open(filename)
    of = open(outFile, 'w')

    for line in rf:
        matches = regexp.search(line)
        if matches:
            matches = list(matches.groups())
            for match in matches:
                if match in keys:
                    replacement = keys[match]
                else:
                    replacement = raw_input('Value for key %s:  ' % match)
                of.write(regexp.sub(replacement, line))
        else:
            of.write(line)

    rf.close()
    of.close()

def findSymlinks(args, folder):
    files = glob('%s/*.symlink' % folder)
    for filename in files:
        link = ".%s" % os.path.splitext(os.path.split(filename)[-1])[0]
        print(filename, link)
        linkPath = os.path.join(os.getenv("HOME"), link)
        if os.path.lexists(linkPath):
            removeFile(link)
        if args.command == "install":
            linkFile(filename, link)

def removeFile(link):
    target = os.path.join(os.getenv("HOME"), link)
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
    os.chdir(os.getenv("HOME"))
    print("symlinking %s -> %s" % (filename, target))
    os.symlink(filename, target)
    os.chdir(dotfilesDir)

if __name__ == "__main__":
    main()

