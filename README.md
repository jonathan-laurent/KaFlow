## KaFlow

Causal analysis for Kappa. In its current state, 
this program does the following:

* Compute the causal cores of every observable event in a given trace
* Render one dot file per causal core

### How to build

Install KappaLib using Opam: `opam pin --dev add KaSim`. Then,
just type `make`. In order to build the tests, type
`cd tests/contextual-egfr ; make`