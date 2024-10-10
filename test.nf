#!/usr/bin/env nextflow

// 参数定义
params.exprMatrixFile = "$baseDir/data/exprMatrix.csv"
params.sampleInfoFile = "$baseDir/data/sampleInfo.csv"
params.compareInfoFile = "$baseDir/data/compareInfo.csv"
params.outdir = "/home/hgj/UbuntuData/NextFlow/Result"
params.plotdir = "/home/hgj/UbuntuData/NextFlow/Result/Plot"
params.log = "log.txt"

// 解析比较组信息的函数
def parseCompareInfo(file) {
    def content = new File(file).getText().trim()
    def rows = content.split("\n").drop(1) // 跳过表头
    return rows.collect { it.split(",")[0] } // 假设第一列是比较组名
}

process diffAnalysis {
    beforeScript 'echo start'
    afterScript 'echo end'
    cache true
    publishDir path: "${params.outdir}", mode: "copy", overwrite: true

    input:
    path exprMatrix
    path sampleInfo
    path compareInfo

    output:
    path "_*_diff.csv"

    script:
    """
    Rscript $baseDir/module/diffAnalysis.R \
    --dataFile ${exprMatrix} \
    --sampleInfoFile ${sampleInfo} \
    --compareInfoFile ${compareInfo} \
    --logFile ${params.log}
    """
}

process drawBoxPlot {
    beforeScript "mkdir -p ${params.plotdir}/${compareGroup}/boxplot"
    cache false
    publishDir path: "${params.plotdir}/${compareGroup}/boxplot", mode: "copy", overwrite: true

    input:
    val compareGroup
    path diffFile
    path sampleInfo

    output:
    path "*.png"

    script:
    """
    Rscript $baseDir/module/boxplot.R \
    --diffFile ${diffFile} \
    --sampleInfoFile ${sampleInfo} \
    --logFile ${params.log}
    """
}

workflow {
    // 调用 diffAnalysis 进程，收集结果
    def diffResults = diffAnalysis(params.exprMatrixFile, params.sampleInfoFile, params.compareInfoFile)

    // 读取并解析比较组信息
    def compareList = parseCompareInfo(params.compareInfoFile)

    // 使用 each 逐个调用 drawBoxPlot，确保每个进程调用唯一
    compareList.map { compareGroup ->
        drawBoxPlot(compareGroup, "${params.outdir}/_${compareGroup}_diff.csv", params.sampleInfoFile)
    }
}
