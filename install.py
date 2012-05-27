#!/usr/bin/env python

import os
import re
from glob import glob
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

    args = parser.parse_args()

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
    if dotfileDir != os.getcwd():
        print("This script must be run from %s" % dotfileDir)
        exit(1)

    compiledDir = os.path.join(dotfileDir, 'compiled')
    os.chdir(dotfileDir)

    if args.command == "install":
        cleanup(compiledDir)
        findTemplates(dotfileDir, compiledDir, keys)

    findSymlinks(args, dotfileDir)
    findSymlinks(args, compiledDir)

    if args.command == "uninstall":
        cleanup(compiledDir)

def cleanup(directory):
    files = glob('%s/*' % directory)
    for filename in files:
        os.unlink(filename)

def findTemplates(searchFolder, outputDir, keys):
    files = glob('%s/*.tmplt' % searchFolder)

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

