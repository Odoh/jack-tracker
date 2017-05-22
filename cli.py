#!/usr/bin/env python3

''' cli.py: A CLI for tracking that dastardly Jack. '''

import sys
import cmd
import argparse
import json
import requests
import platform
import time
import pprint

pp = pprint.PrettyPrinter(indent=2)

class Completions:
    ''' Auto-Completions for root commands. '''

    jack = {
            "alley":[],
            "car":[],
            "walk":[],
            "locs":[],
            "newlocs":[],
            "prevlocs":[],
            "start":["<nodes>", ""]
            }

    det = {
            "locs":[],
            "hit":["<nodes>", ""],
            "miss":["<nodes>", ""],
            "arrest":["<nodes>", ""],
            "setlocs":["<nodes>", ""]
            }

    @staticmethod
    def from_text(text, line, completions_dict):
        ''' Retrives completion entries from completions_dict based on text and line context '''

        num_args = len(line.replace(text,"").split()) # don't include text
        args = line.split()

        # get relevant completions based on context
        completions = []
        if num_args <= 1:
            completions = completions_dict.keys()
        else:
            subcmd = args[1]
            completions = completions_dict[subcmd]

        # complete a portion of a word or list all possibilities
        if text:
            return [cmpl for cmpl in completions if cmpl.startswith(text)]
        else:
            return completions


class JackTrackShell(cmd.Cmd):
    ''' Jack Track Shell. '''
    def __init__(self, root_url, session):
        cmd.Cmd.__init__(self)
        self.root_url = root_url
        self.session = session
        self.prompt = session + "> "

    def send_jack_cmd(self, arg):
        args = list(filter(None, arg.split(" ")))
        if len(args) == 0:
            print("jack <alley, car, walk, locs, newlocs, prevlocs, start>")
            return

        subcmd = args[0]
        if subcmd == "alley":
            r = requests.post(self.root_url + "/jack/alley", data=json.dumps({"session":self.session}))
            if r.status_code == requests.codes.ok:
                print("Jack took an alley.")
            else:
                print("Problem taking alley: " + r.text)
        elif subcmd == "car":
            r = requests.post(self.root_url + "/jack/car", data=json.dumps({"session":self.session}))
            if r.status_code == requests.codes.ok:
                print("Jack took a Carriage.")
            else:
                print("Problem taking carriage: " + r.text)
        elif subcmd == "walk":
            r = requests.post(self.root_url + "/jack/walk", data=json.dumps({"session":self.session}))
            if r.status_code == requests.codes.ok:
                print("Jack has walked.")
            else:
                print("Problem walking: " + r.text)
        elif subcmd == "locs":
            r = requests.get(self.root_url + "/jack/locs/" + self.session)
            if r.status_code == requests.codes.ok:
                print("Jack's potential current locations")
                print(r.json())
            else:
                print("Problem finding possible current Jack locations: " + r.text)
        elif subcmd == "newlocs":
            r = requests.get(self.root_url + "/jack/newlocs/" + self.session)
            if r.status_code == requests.codes.ok:
                print("Jack's new potential current locations")
                print(r.json())
            else:
                print("Problem finding new possible current Jack locations: " + r.text)
        elif subcmd == "prevlocs":
            r = requests.get(self.root_url + "/jack/prevlocs/" + self.session)
            if r.status_code == requests.codes.ok:
                print("Jack's potential previous locations")
                print(r.json())
            else:
                print("Problem finding new possible current Jack locations: " + r.text)
        elif subcmd == "start":
            if len(args) < 2 or len(args) > 3:
                print("jack start <1-2 start locs>")
                return

            try:
                nodes = []
                for i in range(1, len(args)):
                    nodes.append(int(args[i]))
            except ValueError:
                print("jack start <1-2 start locs>")
                return

            r = requests.post(self.root_url + "/jack/startlocs", data=json.dumps({"session":self.session,
                                                                                  "nodes":nodes}))
            if r.status_code == requests.codes.ok:
                print("Jack has killed at ", end="")
                pp.pprint(nodes)
            else:
                print("Problem with Jack starting: " + r.text)
        else:
            print("jack <alley, car, walk, locs, start>")

    def send_det_cmd(self, arg):
        args = list(filter(None, arg.split(" ")))
        if len(args) < 1:
            print("det <locs, hit, miss, arrest, setlocs>")
            return

        subcmd = args[0]
        if subcmd == "locs":
            r = requests.get(self.root_url + "/det/locs/" + self.session)
            if r.status_code == requests.codes.ok:
                print("Detective's current locations")
                print(r.json())
            else:
                print("Problem finding current detective locations: " + r.text)
            return

        if len(args) < 2:
            print("det <hit, miss, arrest, setlocs> <nodes>")
            return

        try:
            nodes = []
            for i in range(1, len(args)):
                nodes.append(int(args[i]))
        except ValueError:
            print("det <hit, miss, arrest, setlocs> <nodes>")
            return
        if subcmd == "hit":
            r = requests.post(self.root_url + "/det/sniffhit", data=json.dumps({"session":self.session,
                                                                                "nodes":nodes}))
            if r.status_code == requests.codes.ok:
                print("Detectives successfully sniffed ", end="")
                pp.pprint(nodes)
            else:
                print("Problem with Detectives successful sniffing: " + r.text)
        elif subcmd == "miss":
            r = requests.post(self.root_url + "/det/sniffmiss", data=json.dumps({"session":self.session,
                                                                                 "nodes":nodes}))
            if r.status_code == requests.codes.ok:
                print("Detectives failed to sniff ", end="")
                pp.pprint(nodes)
            else:
                print("Problem with Detectives failing to sniff: " + r.text)
        elif subcmd == "arrest":
            r = requests.post(self.root_url + "/det/arrestmiss", data=json.dumps({"session":self.session,
                                                                                  "nodes":nodes}))
            if r.status_code == requests.codes.ok:
                print("Detectives failed to arrest ", end="")
                pp.pprint(nodes)
            else:
                print("Problem with Detectives failing to arrest: " + r.text)
        elif subcmd == "setlocs":
            r = requests.post(self.root_url + "/det/setlocs", data=json.dumps({"session":self.session,
                                                                               "nodes":nodes}))
            if r.status_code == requests.codes.ok:
                print("Detectives changing locations to ", end="")
                pp.pprint(nodes)
            else:
                print("Problem with Detectives changing locations: " + r.text)
        else:
            print("det <hit, miss, arrest, setlocs> <nodes>")
            return

    ## cmd commands
    ## ------------
    def default(self, line):
        print("invalid command: {}".format(line))

    def preloop(self):
        None

    def postloop(self):
        None

    def precmd(self, line):
        return line

    def postcmd(self, stop, line):
        return stop

    ## jack tracker cmd commands
    ## -------------------------
    def do_q(self, arg):
        return True # returns stop cmd

    def do_jack(self, arg):
        self.send_jack_cmd(arg)

    def complete_jack(self, text, line, starti, endi):
        return Completions.from_text(text, line, Completions.jack)

    def do_det(self, arg):
        self.send_det_cmd(arg)

    def complete_det(self, text, line, starti, endi):
        return Completions.from_text(text, line, Completions.det)

    def do_help(self, arg):
        help_str = "".join(["JackTrack CLI:\n"
                            "  command line interface to track down Jack.\n"
                            "\n"
                            "generic commands:\n"
                            "  '?'  - this help menu\n"
                            "  'q'  - quit the cli\n"
                            "\n"
                            "additional help for commands:\n"
                            "  '? <command>'\n"
                            "commands:\n"
                            "  jack\n"
                            "  det\n"
                            ])

        if "jack" in arg:
            help_str = "".join(["root command for Jack actions\n"
                                "\n"
                                "Additional help for Jack subcommands:\n"
                                "  '? jack <subcommand>'\n"
                                "\n"
                                "jack subcommands:\n"
                                "  alley\n"
                                "  car\n"
                                "  walk\n"
                                "  locs\n"
                                "  newlocs\n"
                                "  start\n"
                                ])
            if "alley" in arg:
                help_str = "".join(["record an Alley move for Jack\n"
                                    "\n"
                                    "  'jack alley'"
                                    ])
            if "car" in arg:
                help_str = "".join(["record a car move for Jack\n"
                                    "\n"
                                    "  'jack car'"
                                    ])
            if "walk" in arg:
                help_str = "".join(["record a walk move for Jack\n"
                                    "\n"
                                    "  'jack walk'"
                                    ])
            if "locs" in arg:
                help_str = "".join(["return Jack's current position locations\n"
                                    "\n"
                                    "  'jack locs'"
                                    ])
            if "newlocs" in arg:
                help_str = "".join(["return Jack's current position locations that he wasn't at before\n"
                                    "\n"
                                    "  'jack newlocs'"
                                    ])
            if "prevlocs" in arg:
                help_str = "".join(["return Jack's posible previous locations\n"
                                    "\n"
                                    "  'jack prevlocs'"
                                    ])
            if "start" in arg:
                help_str = "".join(["set Jack's starting location(s)\n"
                                    "\n"
                                    "  'jack start 1'\n"
                                    "  'jack start 1 2'\n"
                                    ])

        if "det" in arg:
            help_str = "".join(["root command for Detective actions\n"
                                "\n"
                                "Additional help for Detective subcommands:\n"
                                "  '? det <subcommand>'\n"
                                "\n"
                                "det subcommands:\n"
                                "  locs\n"
                                "  hit\n"
                                "  miss\n"
                                "  arrest\n"
                                "  setlocs\n"
                                ])
            if "locs" in arg:
                help_str = "".join(["return the detective's current positions\n"
                                    "\n"
                                    "  'det locs'"
                                    ])
            if "hit" in arg:
                help_str = "".join(["record successful Detective sniff hit(s)\n"
                                    "\n"
                                    "  'det hit 1'\n"
                                    "  'det hit 1 2 3'\n"
                                    ])
            if "miss" in arg:
                help_str = "".join(["record failed Detective sniff(s), miss(es)\n"
                                    "\n"
                                    "  'det miss 1'\n"
                                    "  'det miss 1 2 3'\n"
                                    ])
            if "arrest" in arg:
                help_str = "".join(["record failed Detective arrest(s)\n"
                                    "\n"
                                    "  'det arrest 1'\n"
                                    "  'det arrest 1 2 3'\n"
                                    ])
            if "setlocs" in arg:
                help_str = "".join(["set the Detective's location(s)\n"
                                    "\n"
                                    "  'det setlocs 1'\n"
                                    "  'det setlocs 1 2'\n"
                                    ])
        print(help_str)


def main(args):
    if args.port:
        port = args.port
    else:
        port = "8080"

    if args.host == "whodag":
        root_url = "http://whodag.us/jacktrack"
    else:
        root_url = "http://" + args.host + ":" + port

    if args.session:
        session = args.session
    else:
        session = "".join([platform.node(), "_",
                           time.strftime("%m-%d-%Y"), "_",
                           time.strftime("%I-%M-%S")])

    shell = JackTrackShell(root_url, session)
    shell.cmdloop()

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="CLI for Jack Track.")
    parser.add_argument("host", help="The host running the Jack Tracker server.")
    parser.add_argument("-p", "--port", required=False, help="The port of the host to connect to.")
    parser.add_argument("-s", "--session", required=False, help="The session string to use.")
    args = parser.parse_args()

    main(args)

