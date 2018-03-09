#!/bin/bash
for i in {0..10}; do (echo $i; Rscript bashreliant_pllppnts_ABrecovery.R $i &); done
#loop is over participant id, max atm is 0..39
