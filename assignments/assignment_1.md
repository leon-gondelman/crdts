
# Assignment 1

The main goal of this assignment is twofold:

- Firstly, to implement two op-based CRDTs, namely GSet and 2PSet,
  following the description in the 8.1.2 and 8.1.3 respectively
  of the "Pure Operation-Based Replicated Data Types" paper.

  Provide your implementations in the files
  `/ml_sources/rcb/crdt/gset/gset_code.ml`
  `/ml_sources/rcb/crdt/two_pset/two_pset_code.ml`

  and provide testing for them in the files
  `/ml_sources/rcb/crdt/gset/gset_code_runner.ml`
  `/ml_sources/rcb/crdt/two_pset/two_pset_code_runner.ml `

  In this assignment, you can simply try to adapt code from
  `/ml_sources/rcb/crdt/counter/counter_code.ml` and
  `/ml_sources/rcb/crdt/counter/counter_runner.ml`
  files for your implementations.



- Secondly, to study how the reliable causal broadcast (rcb) is implemented and used:
  - How the causal delivery is enforced?
  - How the reliability part is implemented?
  - What are limitations of the code?
  - Is that implementation partition-tolerant? Why partition tolerance matter here?
  - How the rcb is used as a middle-ware by op-based crdts?
  - How do/can we test our programs w.r.t. to UDP and network partitioning?
  - Does the rcb code seems correct to you?

	NB: we did not yet proved this implementation, but it is closely based on the
	causally-consistent distributed database that we have formally proved and
	that you can find in the `ml_sources/ccddb/` folder. It might be interesting for you
	to actually compare the code of the `ccddb_code.ml` and the rcb implementation `rcb_code.ml`
