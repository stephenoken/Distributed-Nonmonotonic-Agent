# Distributed-Nonomontic-Agent
Deimos Agent http://www.ict.griffith.edu.au/arock/defeasible/Defeasible.cgi

## Install
This codebase has been updated to work with Haskell 2010

The following commands should compile to code, for more information consult the documentation in the docs directory
- `cd src`
- `make bin`


## Run
In `bin` directory run `./DProver ../../theories/01huskies.t`
To generate scalable problems  run `./DTScale -t mix 100 50 30 >> theory.t` this creates a
ridiculously long problem.

This will load the huskies problem from here you can query proofs such as `+d b`

## Debugging Deimos
The `run_deimos.py` script will help automating the debugging of Deimos

### Current features
- `python2 ./run_deimos.py -t ./theories/01huskies.t` will run DProver and output the contents in a `./theories/[theory name]/file_name.txt`

### Planned features
- Implement a watch feature to build deimos on the fly
- Generate multiple files and compare the output

Automate user input in Deimos
Happy Hunting 😅
