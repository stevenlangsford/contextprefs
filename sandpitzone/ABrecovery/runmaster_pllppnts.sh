#!/bin/bash
for i in {0..39}; do (Rscript bashreliant_pllppnts_ABrecovery.R $i &); done
#loop is over participant id.
