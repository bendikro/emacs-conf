#!/usr/bin/env python
from __future__ import print_function

import argparse
import logging
import os
import re
import sys
import signal
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

import datetime
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


def print_log(msg, **args):
    print("[%s] %s" % (datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S:%f")[:-3], msg), **args)


def get_formatted_time_elapsed(sec_start, sec_end=None):
    if sec_end is None:
        sec_end = time.time()
    return "%2f" % (sec_end - sec_start)


begin = None
count = 0


def execute(args):
    global begin, count
    wait = str_to_timedelta(args.wait)
    begin = time.time()

    if args.verbose:
        if not args.until_fail:
            cprint('[V] Repeating command %d times: "%s"' % (args.count, args.command), color='green')
        else:
            cprint('[V] Repeating command until failure: "%s"' % (args.command), color='green')
        print()


    env = os.environ.copy()
    if 'pytest ' in args.command:
        env['PYTEST_ADDOPTS'] ='--color=yes'

    count = 0
    error_ret = 0
    while True:
        count += 1
        if args.verbose > 1:
            print_log("%d command execution: " % count, end='\n')
            sys.stdout.flush()

        start_cmd = time.time()
        proc = subprocess.Popen(
            args.command,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            shell=True,
            env=env
        )

        for i, line in enumerate(iter(proc.stdout.readline, b'')):
            print(line.decode().strip())

        while proc.poll() is None:
            time.sleep(.1)

        end_cmd = time.time()

        if args.fail_exit_value:
            if proc.returncode in args.fail_exit_value:
                error_ret = ret
                break
        else:
            if proc.returncode != 0:
                error_ret = proc.returncode
                break

        if args.time:
            print_log("Execution time: %s" % get_formatted_time_elapsed(start_cmd, sec_end=end_cmd))

        if not args.until_fail and args.count and count == args.count:
            break

        if wait:
            if args.verbose > 2:
                print_log("Sleeping for %s seconds: " % wait.total_seconds())
            time.sleep(wait.total_seconds())

        if args.time:
            if args.verbose > 1:
                print_log("Time elapsed: %s" % get_formatted_time_elapsed(begin))


    if error_ret != 0:
        cprint("\nCommand exited with status '%s' on run %d " % (error_ret, count), color='red')

    end = time.time()

    if args.verbose > 1:
        print_log("Total executions: %s" % count)

    if args.time:
        print("Time elapsed: %s seconds" % get_formatted_time_elapsed(begin))

    return error_ret


def _signal_handler(*args) -> None:
    print()
    print("Aborted after successful runs: %d" % (count))
    print("Time elapsed: %s seconds" % get_formatted_time_elapsed(begin))
    sys.exit()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Repeat shell command')
    parser.add_argument('-c', '--count', default=2, type=int, help='Times to repeat the command. Default: %(default)s')
    parser.add_argument('-w', '--wait', default='0s', type=str,
                        help='Wait for given number of seconds between each call. Default: %(default)s')
    parser.add_argument('-t', '--time', action='store_true', help='Print total time elapsed')
    parser.add_argument('-u', '--until-fail', action='store_true', help='Repeat command until it fails')
    parser.add_argument('-f', '--fail-exit-value', type=int, action='append', help='Exit values considered as failure')
    parser.add_argument('-v', '--verbose', default=1, type=int, help='Be verbose. (0-2). Default: %(default)s')
    parser.add_argument('command', help='The command to execute')
    args = parser.parse_args()
    signal.signal(signal.SIGINT, _signal_handler)
    sys.exit(execute(args))
