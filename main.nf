#!/usr/bin/env nextflow

// 参数定义
params.exprMatrixFile = "$baseDir/data/exprMatrix.csv"
params.sampleInfoFile = "$baseDir/data/sampleInfo.csv"
params.compareInfoFile = "$baseDir/data/compareInfo.csv"
params.outdir = "Result"
params.plotdir = "Result/Plot"
params.log = "log.txt"


process diffAnalysis {
    beforeScript "mkdir -p ${params.outdir}"
    afterScript 'echo end'
    cache true
    publishDir path: "${params.outdir}", mode: "copy", overwrite: true

    input:
    path exprMatrix
    path sampleInfo
    path compareInfo

    output:
    path "_*_diff.csv"  // 多个文件输出，后续调用使用flatten

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
    --diffFile "${diffFile}" \
    --sampleInfoFile ${sampleInfo} \
    --logFile ${params.log}
    """
}

workflow {
    // 调用 diffAnalysis 进程，收集结果
    diffResults = diffAnalysis(params.exprMatrixFile, params.sampleInfoFile, params.compareInfoFile)

    // 生成 compareGroups 通道
    compareGroups = diffResults
        .flatten()
        .map { file ->
            def match = file.name =~ /_(.*)_diff\.csv/
            return match ? match[0][1] : null // 提取比较组名
        }
        .filter { it != null } // 过滤掉可能的 null 值

    drawBoxPlot(compareGroups, diffResults.flatten(), params.sampleInfoFile)
}
