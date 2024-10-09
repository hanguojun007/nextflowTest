#!/usr/bin/env nextflow

params.exprMatrix = "$baseDir/data/exprMatrix.csv"
params.sampleInfo = "$baseDir/data/sampleInfo.csv"
params.compareInfo = "$baseDir/data/compareInfo.csv"
params.outdir = "/home/hgj/UbuntuData/NextFlow/Result"
params.log = "log.txt"

// 差异分析后进行绘制箱线图，小提琴图，误差棒图

process diffAnalysis {
    beforeScript 'echo start'
    afterScript 'echo end'
    cache true
    publishDir path:"${params.outdir}", mode:"copy", overwrite: true

    input:
    path exprMatrix
    path sampleInfo
    path compareInfo

    output:
    path "_*_diff.csv"

    script:
    """
    Rscript $baseDir/module/diffAnalysis.R --dataFile ${exprMatrix} --sampleInfoFile ${sampleInfo} --compareInfoFile ${compareInfo} --logFile ${params.log}
    """
}

workflow {
    diffAnalysis(params.exprMatrix, params.sampleInfo, params.compareInfo)
}