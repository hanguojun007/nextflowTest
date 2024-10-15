#!/bin/bash

Rscript $baseDir/functionCode/violinplot.R \
    --diffFile "${diffFile}" \
    --sampleInfoFile ${sampleInfo} \
    --logFile ${params.log}