#! /usr/bin/env python2
import os

import argparse
import datetime as dt
import shutil

parser = argparse.ArgumentParser(description='Run Deimos Program and output contents to file in theories directory.')
parser.add_argument( '-t', '--theory', type=str, help="Theory file path", default="./theories/01huskies.t")
parser.add_argument('-p', '--proof', type=str, help="Proof to run against theory", default="+d b\nq")
parser.add_argument('-rg', '--regexp', type=str, help="Enter regular expression", default="\"In DInfernce\"")
parser.add_argument('-n', '--num_of_files', type=int, help="Number of files to generate", default=1)
args = parser.parse_args()

dest_dir_name = "./theories/" + args.theory[args.theory.rfind("/")+1:-2]

if os.path.isdir(dest_dir_name):
    shutil.rmtree(dest_dir_name)

target_dir_path = args.theory[:-2] + "/"
os.mkdir(target_dir_path)

def run_deimos():
    target_file_dest  = target_dir_path +  str(dt.datetime.now()).replace(" ","") + ".txt"
    proof = args.proof if args.proof.endswith("q") else args.proof + "\nq"
    echoProof = "echo \"" + proof + "\"|"
    os.system(echoProof + " ./bin/i386-Darwin/DProver " + args.theory + ">& " + target_file_dest)
    os.system("grep -n " + args.regexp + " " + target_file_dest)
    print os.linesep



for n in range(0,args.num_of_files):
    run_deimos()
