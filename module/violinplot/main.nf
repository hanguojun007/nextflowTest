params.plotdir = "Result/Plot"

process drawViolinPlot {
    beforeScript "mkdir -p ${params.plotdir}/${compareGroup}/violinplot"
    cache false
    publishDir path: "${params.plotdir}/${compareGroup}/violinplot", mode: "copy", overwrite: true

    input:
    val compareGroup
    path diffFile
    path sampleInfo
    val test

    output:
    path "*.png"

    script:
    template 'violinplot.sh'
}