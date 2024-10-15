argv <- argparser::arg_parser(description = "根据差异结果进行boxplot图绘制")
argv <- argparser::add_argument(argv, "--diffFile", help = "差异结果文件", type = "character")
argv <- argparser::add_argument(argv, "--sampleInfoFile", help = "样本分组信息文件, 第一列为样本列, 第二列为分组列", type = "character")
argv <- argparser::add_argument(argv, "--logFile", help = "日志信息文件", type = "character", default = "./scriptRUNinfo.log")
argv <- argparser::add_argument(argv, "--outpath", help = "输出路径", type = "character", default = "./")

args <- argparser::parse_args(argv)

diffFile <- args$diffFile
sampleInfoFile <- args$sampleInfoFile
logFile <- args$logFile
outpath <- args$outpath

# diffFile <- "/home/hgj/UbuntuData/NextFlow/Result/_B_vs_A_diff.csv"
# sampleInfoFile <- "/home/hgj/UbuntuData/NextFlow/nextflowTest/data/sampleInfo.csv"
# logFile <- "./scriptRUNinfo.log"
# outpath <- "./"


library(tidyverse)
library(foreach)

###########################################
# tool function
###########################################
# 自定义日志记录函数
LogMessage <- function(..., type = c("INFO", "WARN", "ERROR"), logFile = "log") {
  type <- match.arg(type)
  message <- paste(..., collapse = " ")

  # 追加到日志文件
  cat(glue::glue("{Sys.time()} - {type} -:{message}\n"), file = logFile, append = TRUE, sep = "\n")

  # 打印到控制台
  switch(type,
    INFO = base::message(message),
    WARN = base::warning(message),
    ERROR = base::stop(message)
  )
}

# 捕获message信息
MyMessage <- function(...) {
  LogMessage(..., type = "INFO")
}

# 捕获warning信息
MyWarning <- function(...) {
  LogMessage(..., type = "WARN")
}

# 捕获error信息
MyStop <- function(...) {
  traceback()
  LogMessage(..., type = "ERROR")
}


#' 读取文件，支持csv，tsv，xlsx
#' @param path 文件路径
ReadFile <- function(path) {
  if (tools::file_ext(path) == "xlsx") {
    dat <- readxl::read_excel(path)
  } else if (tools::file_ext(path) == "csv") {
    dat <- readr::read_csv(path)
  } else if (tools::file_ext(path) == "tsv") {
    dat <- readr::read_tsv(path)
  } else {
    dat <- readr::read_delim(path)
  }
  return(dat)
}


Violinplot <- function(feature, data) {
  g <- ggplot(data, aes(x = Group, y = value, fill = Group)) +
    geom_violin()

  ggsave(filename = glue::glue("{feature}.png"), plot = g, path = outpath, width = 8, height = 6)
}

diffDat <- ReadFile(diffFile)
sampleInfo <- ReadFile(sampleInfoFile)


plotdat <- diffDat |>
  top_n(10, desc(pvalue)) |>
  select(ID, starts_with("Feature"), pvalue) |>
  pivot_longer(starts_with("Feature"), names_to = "sample", values_to = "value") |>
  dplyr::left_join(sampleInfo, by = c("sample" = "Sample")) |>
  dplyr::mutate(Group = factor(Group, levels = c("B", "A"))) |>
  group_by(ID) |>
  nest()

map2(plotdat$ID, plotdat$data, Violinplot)
