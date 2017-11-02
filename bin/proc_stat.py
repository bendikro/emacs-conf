#!/usr/bin/env python
from __future__ import print_function

import argparse
import csv
import re
import sys
import time
from datetime import datetime, timedelta

import psutil

try:
    from termcolor import colored, cprint
    termcolor = True
except:  # noqa E722
    termcolor = False

    def cprint(*arg, **kwargs):
        print(*arg)

    def colored(text, color):
        return text


def sizeof_fmt(num, suffix='iB'):
    for unit in ['', 'K', 'M', 'G', 'T', 'P', 'E', 'Z']:
        if abs(num) < 1000.0:
            return "%3.1f%s%s" % (num, unit, suffix if unit else '')
        num /= 1000.0

    return "%.1f%s%s" % (num, 'Y', suffix)


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
                    'd': 'days'}
    args.update({unit_to_time.get(groups['unit'], 'seconds'): float(groups['duration'])})
    return timedelta(**args)


def get_formatted_time_elapsed(sec_start):
    return "%2f" % (time.time() - sec_start)


def get_formatted_datetime(dt, full=False, micro_dec=3):
    fmt = "%H:%M:%S.%f"
    if full:
        fmt = "%Y:%m:%d:" + fmt
    return dt.strftime(fmt)[:-(6-micro_dec)]


def get_procs(args):
    pids = []
    curr_proc = psutil.Process()

    if args.pids:
        for pid_str in args.pids.split(','):
            proc = psutil.Process(int(pid_str))
            if proc.pid != curr_proc.pid:
                pids.append(proc)

    if args.name or args.cmd:
        for proc in psutil.process_iter():
            add = False

            if proc.name() in args.name:
                add = True

            if args.cmd:
                cmdline = ' '.join(proc.cmdline())
                for cmd in args.cmd:
                    if re.search(cmd, cmdline):
                        add = True
                        break

            if add and proc.pid != curr_proc.pid:
                pids.append(proc)
    return pids


def update_proc_stats(pios, proc, wait_sec):
    new_pio = proc.io_counters()
    ret = None

    if proc.pid in pios:
        old_pio = pios[proc.pid]
        read_count = new_pio.read_count - old_pio.read_count
        write_count = new_pio.write_count - old_pio.write_count
        read_bytes = new_pio.read_bytes - old_pio.read_bytes
        write_bytes = new_pio.write_bytes - old_pio.write_bytes
        read_chars = new_pio.read_chars - old_pio.read_chars
        write_chars = new_pio.write_chars - old_pio.write_chars

        ret = {'pid': str(proc.pid),
               'timestamp': get_formatted_datetime(datetime.now(), full=True),
               'name': proc.name(),
               'read_count': read_count,
               'write_count': write_count,
               'read_bytes': read_bytes,
               'write_bytes': write_bytes,
               'read_chars': read_chars,
               'write_chars': write_chars,
               'read_per_sec': sizeof_fmt(read_chars / wait_sec.total_seconds()),
               'write_per_sec': sizeof_fmt(write_chars / wait_sec.total_seconds()),
               'cpu_percent': proc.cpu_percent(interval=None),
               'memory_percent': proc.memory_percent()}

    pios[proc.pid] = new_pio
    return ret


def execute(args, procs):
    wait_sec = str_to_timedelta(args.wait)

    pios = {}
    csv_file = None
    csv_writer = None
    csv_fields = ["timestamp", "pid", "name",
                  "cpu_percent", "memory_percent",
                  "read_count", "write_count",
                  "read_bytes", "write_bytes",
                  "read_chars", "write_chars",
                  "read_per_sec", "write_per_sec"]

    base_fmt = "%(timestamp)-30s %(pid)-12s %(name)-12s %(read_count)-12s %(write_count)-12s %(read_bytes)-12s  %(write_bytes)-12s %(read_chars)-12s %(write_chars)-12s %(read_per_sec)-12s %(write_per_sec)-12s"
    title_row = {"pid": "PID", "name": "Name", "timestamp": "Time",
                 "read_count": "Read count", "write_count": "Write count",
                 "read_bytes": "Read bytes", "write_bytes": "Write bytes",
                 "read_chars": "read_chars", "write_chars": "write_chars",
                 "read_per_sec": "read_per_sec", "write_per_sec": "write_per_sec"}

    if args.csv:
        csv_file = open(args.csv, 'w')
        csv_writer = csv.DictWriter(csv_file, fieldnames=csv_fields)
        csv_writer.writeheader()

    if args.verbose:
        print("Collection stats every %s for %d processes: %s" %
              (args.wait, len(procs), ', '.join([p.name() for p in procs])))

    while True:
        if pios and not args.no_print:
            print(base_fmt % title_row)

        for proc in procs:
            stats = update_proc_stats(pios, proc, wait_sec)
            if stats:
                if not args.no_print:
                    print(base_fmt % stats)

                if csv_writer:
                    csv_writer.writerow(stats)
                    csv_file.flush()

        time.sleep(wait_sec.total_seconds())

    if csv_file:
        csv_file.close()

    return 0


def main(args):
    procs = get_procs(args)
    execute(args, procs)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Repeat shell command')
    parser.add_argument('-r', '--repeat', default=0, type=int,
                        help='Times to repeat the command. Default: %(default)s')
    parser.add_argument('-w', '--wait', default='1s', type=str,
                        help='Wait for given number of seconds between each call. Default: %(default)s')
    parser.add_argument('-p', '--pids', type=str, help='Comma separated list of pids')

    parser.add_argument('-c', '--cmd', type=str, action='append', help='Expression matching a proceess command line')
    parser.add_argument('-n', '--name', type=str, action='append', default=[], help='Name of binary to monitor')
    parser.add_argument('--csv', help='Write to CSV file')
    parser.add_argument('-v', '--verbose', default=1, choices=range(0, 3), action='store', type=int,
                        help='Be verbose. Default: %(default)s')
    parser.add_argument('--no-print', action='store_true', help='Do not print to stdout')

    args = parser.parse_args()
    sys.exit(main(args))
