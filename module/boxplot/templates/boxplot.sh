#!/bin/bash

Rscript $baseDir/functionCode/boxplot.R \
    --diffFile "${diffFile}" \
    --sampleInfoFile ${sampleInfo} \
    --logFile ${params.log}