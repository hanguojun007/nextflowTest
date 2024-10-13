params.plotdir = "Result/Plot"

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
    template 'boxplot.sh'
}