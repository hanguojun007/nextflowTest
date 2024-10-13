params.outdir = "Result"
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
    template 'diffAnalysis.sh'
}
