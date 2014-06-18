#!/usr/bin/python
import re
import subprocess


def get_keychain_pass(account=None, server=None):
    params = {
        'security': '/usr/bin/security',
        'command': 'find-internet-password',
        'account': account,
        'server': server
    }
    command = "sudo -u guy %(security)s -v %(command)s -g -a %(account)s -s %(server)s" % params
    output = subprocess.check_output(
        command,
        shell=True,
        stderr=subprocess.STDOUT
    )
    outtext = [l for l in output.splitlines()
               if l.startswith('password: ')][0]

    return re.match(r'password: "(.*)"', outtext).group(1)

if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(description='Retrieve a password')
    parser.add_argument('--account', help='The account')
    parser.add_argument('--server', help='The server')
    args = parser.parse_args()

    password = get_keychain_pass(args.account, args.server)
    print(password)

