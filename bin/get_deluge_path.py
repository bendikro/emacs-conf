#!/usr/bin/env python

import argparse
import os, sys

if __name__ == '__main__':
    argparser = argparse.ArgumentParser(description="Change active Deluge version")
    argparser.add_argument("-l", "--list-versions", help="List all availlable versions.", required=False, action='store_true')
    argparser.add_argument("-d", "--dir", help="Directory with the develop installs.", required=False, default=".", action='store_true')
    argparser.add_argument("-tn", "--target-name", help="The name of the symlink target.", required=False, default="deluge", action='store_true')
    argparser.add_argument('version', help='The new version.', nargs="?")
    args = argparser.parse_args()

    PYTHONPATH = os.environ["PYTHONPATH"]
    cwd = os.getcwd()
    link = os.path.join(cwd, "deluge.egg-link")
    if os.path.isfile(link):
        with open(link, "r") as f:
            path = f.readline().strip()
            PYTHONPATH = "%s:%s" % (path, PYTHONPATH)

    print PYTHONPATH
