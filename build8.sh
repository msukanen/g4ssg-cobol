#!/usr/bin/bash

# Build stuff as 8 jobs (at max) in parallel.
cmake --build . --parallel 8
