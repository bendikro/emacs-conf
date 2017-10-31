#!/usr/bin/env python
from __future__ import print_function

import argparse
import logging
import os
import re
import sys
import subprocess
import time

try:
    from termcolor import colored, cprint
    termcolor = True
except:
    termcolor = False
    def cprint(*arg, **kwargs):
        print(*arg)
    def colored(text, color):
        return text

from datetime import timedelta

def str_to_timedelta(time_val):
    """
    Given a *time_val* (string) such as '5d', returns a timedelta object
    representing the given value (e.g. timedelta(days=5)).  Accepts the
    following '<num><char>' formats:

    =========   ===========   ===================
    Character   Meaning       Example
    =========   ===========   ===================
    ms          Miliseconds   '60ms' -> 60 Seconds
    s           Seconds       '60s' -> 60 Seconds
    m           Minutes       '5m'  -> 5 Minutes
    h           Hours         '24h' -> 24 Hours
    d           Days          '7d'  -> 7 Days
    =========   ===========   ===================

    Examples::

        >>> convert_to_timedelta('7d')
        datetime.timedelta(7)
        >>> convert_to_timedelta('24h')
        datetime.timedelta(1)
        >>> convert_to_timedelta('60m')
        datetime.timedelta(0, 3600)
        >>> convert_to_timedelta('120s')
        datetime.timedelta(0, 120)
    """
    args = {}
    result = re.match('(?P<duration>\d+(\.\d+)?)(?P<unit>.*)', time_val)

    if not result:
        return None

    groups = result.groupdict()
    unit_to_time = {'ms': 'milliseconds',
                        's': 'seconds',
                        'm': 'minutes',
                        'h': 'hours',
                        'd': 'days'
                        }
    args.update({unit_to_time.get(groups['unit'], 'seconds'): float(groups['duration'])})
    return timedelta(**args)

#def get_formatted_timedelta(td):
#    return td.strftime("%H:%M:%S.%f").rstrip('0')


def get_formatted_time_elapsed(sec_start):
    return "%2f" % (time.time() - sec_start)


def execute(args):
    print("execute")
    wait = str_to_timedelta(args.wait)
    print("Wait:", wait)

    start = time.time()

    if args.verbose:
        if not args.until_fail:
            cprint('[V] Repeating command %d times: "%s"' % (args.count, args.command), 'green')
        else:
            cprint('[V] Repeating command until failure: "%s"' % (args.command), 'green')
        print()

    count = 0
    error_ret = 0
    while True:
        count += 1
        if args.verbose > 1:
            print("[V] %d command execution: " % count, end='')
            sys.stdout.flush()

        ret = subprocess.call(
            args.command,
            stderr=subprocess.STDOUT,
            shell=True)

        if args.fail_exit_value:
            if ret in args.fail_exit_value:
                error_ret = ret
                break
        else:
            if ret != 0:
                error_ret = ret
                break

        if not args.until_fail and args.count and count == args.count:
            break

        if wait:
            if args.verbose > 2:
                print("[V] Sleeping for %s seconds: " % wait.total_seconds())
            time.sleep(wait.total_seconds())

        if args.time:
            if args.verbose > 1:
                print("[V] Time elapsed: %s" % get_formatted_time_elapsed(start))


    if error_ret is not 0:
        cprint("\nCommand exited with status '%s' on run %d " % (error_ret, count), 'red')

    end = time.time()

    if args.verbose > 1:
        print("[V] Total executions: %s" % count)

    if args.time:
        print("Time elapsed: %s seconds" % get_formatted_time_elapsed(start))

    return error_ret


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Repeat shell command')
    parser.add_argument('-c', '--count', default=2, type=int, help='Times to repeat the command. Default: %(default)s')
    parser.add_argument('-w', '--wait', default='0s', type=str, help='Wait for given number of seconds between each call. Default: %(default)s')
    parser.add_argument('-t', '--time', action='store_true', help='Print total time elapsed')
    parser.add_argument('-u', '--until-fail', action='store_true', help='Repeat command until it fails')
    parser.add_argument('-f', '--fail-exit-value', type=int, action='append', help='Exit values considered as failure')
    parser.add_argument('-v', '--verbose', default=1, type=int, help='Be verbose. (0-2). Default: %(default)s')
    parser.add_argument('command', help='The command to execute')
    args = parser.parse_args()
    sys.exit(execute(args))
