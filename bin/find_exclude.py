#!/usr/bin/env python3
from __future__ import print_function

import argparse
import logging
import os
import re
import sys
import subprocess
import time
import sh
from sh import find

try:
    from termcolor import colored, cprint
    termcolor = True
except:
    termcolor = False
    def cprint(*arg, **kwargs):
        print(*arg)
    def colored(text, color):
        return text


def build_cmd_list(args, remainder, quote_pattern=False):
    cmd_list = []

    exclude_cmd = []
    if args.exclude_dir:
        for d in args.exclude_dir:
            if exclude_cmd:
                exclude_cmd.append('-o')

            if d.endswith('/'):
                if args.verbose:
                    print("Exclude with cannot end with slash. Removing ending slash from '%s'" % d)
                d = d[:-1]

            if not os.path.isdir(d):
                print("Exclude dir does not exist: '%s'" % d)
                if args.fail_on_error:
                    sys.exit(1)
            elif not os.path.isabs(d):
                if not d.startswith('./'):
                    d = './' + d

            exclude_cmd.extend(('-path', d))

    if exclude_cmd:
        if len(args.exclude_dir) > 1:
            if quote_pattern:
                exclude_cmd = ['\('] + exclude_cmd + ['\)']
            else:
                exclude_cmd = ['('] + exclude_cmd + [')']
        exclude_cmd += ['-prune']

        cmd_list.extend(exclude_cmd)

    if args.name:
        if cmd_list:
            cmd_list.append('-o')
        cmd_list.extend(('-name', "'%s'" % args.name if quote_pattern else args.name))

    if remainder:
        cmd_list.extend(remainder)

    cmd_list.insert(0, args.path)
    return cmd_list


def main(args, remainder):
    if args.verbose > 1:
        print("Main arguments: %s\nRemaining args: %s" % (args, remainder))

    cmd_list = build_cmd_list(args, remainder)

    cmd_list_print = build_cmd_list(args, remainder, quote_pattern=True)
    cmd_str = "find " + " ".join(cmd_list_print)

    if args.verbose or args.print:
        print("Command: '%s'" % (cmd_str))
        if args.print:
            sys.exit(0)

    try:
        exec_cmd = find(*cmd_list, _iter=True)
        for l in exec_cmd:
            print(l, end='')
    except sh.ErrorReturnCode_1 as err:
        cprint("Error when executing command: '%s': %s" % (cmd_str, err), color='red')


def print_help(errmsg):
    cprint("\nError: '%s'\n" % errmsg, color='red')
    parser.print_help()
    sys.exit(1)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Repeat shell command')
    parser.error = print_help
    parser.add_argument('-v', '--verbose', action='count', default=0, help='Be verbose.')
    parser.add_argument('path', default=None, help='Path to search')
    parser.add_argument('-f', '--fail-on-error', action='store_true', help='Exit with status 1 on error')
    parser.add_argument('-e', '--exclude-dir', action='append', help='Dir to exclude')
    parser.add_argument('-p', '--print', action='store_true', help='Print find command and exit')
    pattern_group = parser.add_mutually_exclusive_group(required=True)
    pattern_group.add_argument('-name', action='store', help='Name expression')
    pattern_group.add_argument('-iname', action='store', help='Case insensitive name expression')
    parser.add_argument('command', metavar='find-arguments', nargs=argparse.REMAINDER, help='The command to execute')

    #args, remainder = parser.parse_known_args()
    args = parser.parse_args()
    remainder = args.command
    sys.exit(main(args, remainder))
