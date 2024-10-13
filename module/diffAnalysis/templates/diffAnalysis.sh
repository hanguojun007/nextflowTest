#!/bin/bash
Rscript $baseDir/functionCode/diffAnalysis.R \
    --dataFile ${exprMatrix} \
    --sampleInfoFile ${sampleInfo} \
    --compareInfoFile ${compareInfo} \
    --logFile ${params.log}