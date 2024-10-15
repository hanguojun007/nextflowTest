#!/usr/bin/env nextflow

// 参数定义
params.exprMatrixFile = "$baseDir/data/exprMatrix.csv"
params.sampleInfoFile = "$baseDir/data/sampleInfo.csv"
params.compareInfoFile = "$baseDir/data/compareInfo.csv"
params.outdir = "./Result"
params.plotdir = "./Result/Plot"
params.log = "log.txt"
params.test = 1


include { diffAnalysis } from './module/diffAnalysis'
include { drawBoxPlot } from './module/boxplot'
include { drawViolinPlot } from './module/violinplot'

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
    drawViolinPlot(compareGroups, diffResults.flatten(), params.sampleInfoFile, params.test)
}
