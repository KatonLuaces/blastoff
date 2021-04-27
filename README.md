# BLAStoff

Easily express graph algorithms in the language of linear algebra.

## Build and run the compiler
First, download [GraphBLAS v4.0.3](https://github.com/DrTimothyAldenDavis/GraphBLAS/releases/tag/v4.0.3) and compile the library using their [given instructions](https://github.com/DrTimothyAldenDavis/GraphBLAS/blob/stable/README.md). This should yield the library called `libgraphblas.so.4.0.3` which must be placed in the top-level directory before you attempt to build the BLAStoff compiler.

Next, run `run-docker.sh` to spin up a Docker container corresponding to the provided Dockerfile. Inside the container, run `make` to build the compiler and automatically run all tests.

To re-run tests without re-building, you can also directly run the `testall.sh` script.

To run an individual .bl file and see the output, you can run the `testlocal.sh` script, with the filename as an argument.
