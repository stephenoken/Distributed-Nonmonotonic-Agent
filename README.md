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

Happy Hunting 😅
