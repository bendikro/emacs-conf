#!/usr/bin/env python
import argparse
import logging
import os
import re
import sys
import signal
import selectors
import subprocess
import time
import datetime

try:
    from termcolor import colored, cprint
except:
    def cprint(*arg, **kwargs):
        print(*arg)
    def colored(text, color):
        return text


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
    return datetime.timedelta(**args)


def print_log(msg, **args):
    print("[%s] %s" % (datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S:%f")[:-3], msg), **args)


def get_formatted_time_elapsed(sec_start, sec_end=None):
    if sec_end is None:
        sec_end = time.time()
    return "%2f" % (sec_end - sec_start)


begin_time = None
exec_count = 0


def execute(args):
    global begin_time, exec_count
    wait = str_to_timedelta(args.wait)
    begin_time = time.time()

    if args.verbose:
        if not args.until_fail:
            cprint('[V] Repeating command %d times: "%s"' % (args.count, args.command), color='green')
        else:
            cprint('[V] Repeating command until failure: "%s"' % (args.command), color='green')
        print()


    env = os.environ.copy()
    if 'pytest ' in args.command:
        env['PYTEST_ADDOPTS'] ='--color=yes'

    exec_count = 0
    error_ret = 0
    while True:
        exec_count += 1
        if args.verbose > 1:
            print_log("%d command execution: " % exec_count, end='\n')
            sys.stdout.flush()

        start_cmd = time.time()
        proc = subprocess.Popen(
            args.command,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            shell=True,
            env=env
        )

        sel = selectors.DefaultSelector()

        sel.register(proc.stdout, selectors.EVENT_READ)
        sel.register(proc.stderr, selectors.EVENT_READ)

        def read_output(selector):
            descriptors = list(selector.get_map().keys())
            while descriptors:
                for key, _ in selector.select():
                    data = key.fileobj.read()
                    if not data:
                        if key.fileobj.name in descriptors:
                            descriptors.remove(key.fileobj.name)
                        continue
                    data = data.decode()
                    prefix = ""
                    if args.prefix_output:
                        prefix = "[STDOUT] " if key.fileobj.name == proc.stdout.name else "[STDERR] "

                    # Add the prefix on each line
                    data = data.replace('\n', f'\n{prefix}')
                    if key.fileobj.name == proc.stdout.name:
                        print(f"{data}", end="", flush=True)
                    else:
                        print(f"{data}", end="", flush=True, file=sys.stderr)

        read_output(sel)

        while proc.poll() is None:
            time.sleep(.1)

        cmd_end_time = time.time()

        if args.fail_exit_value:
            if proc.returncode in args.fail_exit_value:
                error_ret = ret
                break
        else:
            if proc.returncode != 0:
                error_ret = proc.returncode
                break

        if args.time:
            print_log("Execution time: %s" % get_formatted_time_elapsed(start_cmd, sec_end=cmd_end_time))

        if not args.until_fail and args.count and exec_count == args.count:
            break

        if wait:
            if args.verbose > 2:
                print_log("Sleeping for %s seconds: " % wait.total_seconds())
            time.sleep(wait.total_seconds())

        if args.time:
            if args.verbose > 1:
                print_log("Time elapsed: %s" % get_formatted_time_elapsed(begin_time))


    if error_ret != 0:
        cprint("\nCommand exited with status '%s' on run %d " % (error_ret, exec_count), color='red')

    end = time.time()

    if args.verbose > 1:
        print_log("Total executions: %s" % exec_count)

    if args.time:
        print("Time elapsed: %s seconds" % get_formatted_time_elapsed(begin_time))

    return error_ret


def _signal_handler(*args) -> None:
    print()
    print("Aborted after successful runs: %d" % (exec_count))
    print("Time elapsed: %s seconds" % get_formatted_time_elapsed(begin_time))
    sys.exit()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Repeat shell command')
    parser.add_argument('-c', '--count', default=2, type=int, help='Times to repeat the command. Default: %(default)s')
    parser.add_argument('-w', '--wait', default='0s', type=str,
                        help='Wait for given number of seconds between each call. Default: %(default)s')
    parser.add_argument('-t', '--time', action='store_true', help='Print total time elapsed')
    parser.add_argument('-u', '--until-fail', action='store_true', help='Repeat command until it fails')
    parser.add_argument('-f', '--fail-exit-value', type=int, action='append', help='Exit values considered as failure')
    parser.add_argument('-p', '--prefix-output', action='store_true', help='Prefix the output indicating file descriptor')
    parser.add_argument('-v', '--verbose', default=1, type=int, help='Be verbose. (0-2). Default: %(default)s')
    parser.add_argument('command', help='The command to execute')
    args = parser.parse_args()
    signal.signal(signal.SIGINT, _signal_handler)
    sys.exit(execute(args))
