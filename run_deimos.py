#! /usr/bin/env python2
import os

import argparse
import datetime as dt 
import shutil

parser = argparse.ArgumentParser(description='Run Deimos Program and out contents to file in theories directory.')
parser.add_argument( '-t', '--theory', type=str, help="Theory file path")
args = parser.parse_args()

dest_dir_name = "./theories/" + args.theory[args.theory.rfind("/")+1:-2]

if os.path.isdir(dest_dir_name):
    shutil.rmtree(dest_dir_name)

target_dir_path = args.theory[:-2] + "/"
target_file_dest  = target_dir_path +  str(dt.datetime.now()).replace(" ","") + ".txt"

os.mkdir(target_dir_path)
os.system("time ./bin/i386-Darwin/DProver " + args.theory + " > " + target_file_dest)