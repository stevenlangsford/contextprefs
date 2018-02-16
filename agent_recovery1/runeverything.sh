#!/bin/bash
find . -maxdepth 1 -mindepth 1 -type d \( ! -name . \) -exec bash -c "cd '{}' && Rscript minrun.R" \;
