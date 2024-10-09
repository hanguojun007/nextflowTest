argv <- argparser::arg_parser(description = "组别间进行差异比较分析, 差异比较方法包括: t.test, anova, wilcox.test, sigAB")
argv <- argparser::add_argument(argv, "--dataFile", help = "定量数据文件, 首列为ID列(列名不固定), 其余为样本定量数据列", type = "character")
argv <- argparser::add_argument(argv, "--sampleInfoFile", help = "样本分组信息文件, 第一列为样本列, 第二列为分组列", type = "character")
argv <- argparser::add_argument(argv, "--compareInfoFile", help = "比较信息文件, 第一列为比较组, 第二列为比较方法", type = "character")
argv <- argparser::add_argument(argv, "--logFile", help = "日志信息文件", type = "character", default = "./scriptRUNinfo.log")
argv <- argparser::add_argument(argv, "--outpath", help = "输出路径", type = "character", default = "./")

args <- argparser::parse_args(argv)

dataFile <- args$dataFile
sampleInfoFile <- args$sampleInfoFile
compareInfoFile <- args$compareInfoFile
logFile <- args$logFile
outpath <- args$outpath


# dataFile <- "/root/data/YAS202406270013-1/Result/Schema/_dingliang.csv"
# sampleInfoFile <- "/root/data/YAS202406270013-1/Result/Schema/_sampleInfo.csv"
# compareInfoFile <- "/root/data/YAS202406270013-1/Result/Schema/Design/GroupVS_with_method.xlsx"
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



#' 检查文件之间的样本分组信息是否对应
#' @param dataFile 数据文件
#' @param sampleInfoFile 样本信息文件
#' @param compareInfoFile 比较信息文件
CheckFile <- function(dataFile, sampleInfoFile, compareInfoFile, logFile) {
  dat <- ReadFile(dataFile)
  colnames(dat)[1] <- "ID"
  dataFile <- basename(dataFile)

  sampleInfo <- ReadFile(sampleInfoFile)
  colnames(sampleInfo) <- c("sample", "group")
  sampleInfoFile <- basename(sampleInfoFile)

  compareInfo <- ReadFile(compareInfoFile) |>
    dplyr::rename(groupvs = 1, method = 2) |>
    dplyr::mutate(
      compare = stringr::str_remove_all(groupvs, "_oneway"),
      groups = stringr::str_split(compare, "_vs_|_")
    )
  compareInfoFile <- basename(compareInfoFile)

  # 首先进行组别之间的对应检查，默认组别之间关系等级最高
  groupA <- unique(sampleInfo$group)
  groupB <- unique(unlist(stringr::str_split(compareInfo$compare, "_vs_|_")))

  groupApart <- setdiff(groupA, groupB)
  groupBpart <- setdiff(groupB, groupA)
  groupABpart <- intersect(groupA, groupB)
  if (length(groupABpart) == 0) {
    MyStop(sprintf("比较组信息文件和样本分组信息文件中的组别信息无法对应!!! 请检查 %s 和 %s", compareInfoFile, sampleInfoFile), logFile = logFile)
  } else {
    if (length(groupBpart) > 0) {
      MyStop(sprintf("比较组信息文件 %s 中的组别 %s 在样本分组信息文件 %s 中不存在!!! 请检查%s", compareInfoFile, paste(groupBpart, collapse = ", "), sampleInfoFile, sampleInfoFile), logFile = logFile)
    } else if (length(groupApart) > 0) {
      MyWarning(sprintf("样本分组信息文件 %s 中的组别 %s 不在比较组信息文件 %s 中, 表示这些组别不进行差异分析, 已忽略", sampleInfoFile, paste(groupApart, collapse = ", "), compareInfoFile), logFile = logFile)
      sampleInfo <- dplyr::filter(sampleInfo, group %in% groupABpart)
    }
  }
  # 进行样本之间的对应检查，以检查过后的sampleInfo中的样本信息为准
  sampleA <- colnames(dat)[-1]
  sampleB <- sampleInfo$sample

  sampleApart <- setdiff(sampleA, sampleB)
  sampleBpart <- setdiff(sampleB, sampleA)
  sampleABpart <- intersect(sampleA, sampleB)

  if (length(sampleABpart) == 0) {
    MyStop(sprintf("数据文件和样本分组信息文件中没有相同的样本!!! 请检查 %s 和 %s", dataFile, sampleInfoFile), logFile = logFile)
  } else {
    if (length(sampleBpart) > 0) {
      Mystop(sprintf("待分析样本 %s 在数据文件 %s 中不存在!!! 请检查 %s", paste(sampleApart, collapse = ", "), dataFile, dataFile), logFile = logFile)
    } else if (length(sampleApart) > 0) {
      MyWarning(sprintf("数据文件 %s 中的样本 %s 不在样本分组信息文件 %s 中, 表示这些样本不进行差异分析, 已忽略", dataFile, paste(sampleBpart, collapse = ", "), sampleInfoFile), logFile = logFile)
      dat <- dplyr::select(dat, ID, dplyr::all_of(sampleABpart))
    }
  }

  return(list(dat = dat, sampleInfo = sampleInfo, compareInfo = compareInfo))
}


MyTest <- function(groups, method, sampleInfo, logFile = "./diff.log") {
  .pairedTtest <- function(x, y) {
    if (length(x) != length(y)) {
      MyStop("数据长度不一致，无法进行配对t检验", logFile = logFile)
    } else {
      if (sum(!is.na(x)) > 1 & sum(!is.na(y)) > 1) { # 保证每组至少有两个以上非NA的值，才进行t.test
        if (!sd(x, na.rm = T) == 0 | !sd(y, na.rm = T) == 0) { # 保证至少有一组数据方差不为0
          pvalue <- t.test(x, y, alternative = "two.side", var.equal = TRUE, paired = FALSE)$p.value
        } else {
          pvalue <- NA
        }
      } else {
        pvalue <- NA
      }
      return(pvalue)
    }
  }

  .unpairedTtest <- function(x, y) {
    if (sum(!is.na(x)) > 1 & sum(!is.na(y)) > 1) { # 保证每组至少有两个以上非NA的值，才进行t.test
      if (!sd(x, na.rm = T) == 0 | !sd(y, na.rm = T) == 0) { # 保证至少有一组数据方差不为0
        pvalue <- t.test(x, y, alternative = "two.side", var.equal = TRUE, paired = FALSE)$p.value
      } else {
        pvalue <- NA
      }
    } else {
      pvalue <- NA
    }
    return(pvalue)
  }

  # Warning message:
  # In wilcox.test.default(c(8, 4, 4), c(1, 6, 2)) : 无法精確計算带连结的p值
  # 上述警告是因为数据中存在相同的值，相同的值被称为结。可以使用参数exact=FALSE来解决。
  .pairedWilcoxTest <- function(x, y) {
    if (length(x) != length(y)) {
      MyStop("数据长度不一致，无法进行配对wilcox检验", logFile = logFile)
    } else {
      pvalue <- wilcox.test(x, y, paired = TRUE)$p.value
      return(pvalue)
    }
  }

  .unpairedWilcoxTest <- function(x, y) {
    pvalue <- wilcox.test(x, y)$p.value
    return(pvalue)
  }

  MeanRes <- foreach(g = groups, .combine = cbind) %do% {
    samples <- sampleInfo$sample[sampleInfo$group == g]
    colName <- glue::glue("{g} Mean")
    value <- mean(dplyr::c_across(dplyr::all_of(samples)), na.rm = TRUE)
    res <- tibble(!!rlang::enquo(colName) := value)
    return(res)
  }

  if (length(groups) == 2) {
    groupAName <- glue::glue("{groups[1]} Mean")
    groupBName <- glue::glue("{groups[2]} Mean")
    sampleA <- sampleInfo$sample[sampleInfo$group == groups[1]]
    sampleB <- sampleInfo$sample[sampleInfo$group == groups[2]]

    .test <- switch(method,
      "paired_t-test" = .pairedTtest,
      "unpaired_t-test" = .unpairedTtest,
      "paired_wilcox" = .pairedWilcoxTest,
      "unpaired_wilcox" = .unpairedWilcoxTest
    )

    MeanRes <- MeanRes |>
      dplyr::mutate(`Fold Change` = !!rlang::sym(groupAName) / !!rlang::sym(groupBName))
    pvalue <- .test(dplyr::c_across(dplyr::all_of(sampleA)), dplyr::c_across(dplyr::all_of(sampleB)))
    res <- cbind(MeanRes, tibble::tibble(pvalue = pvalue))
  } else if (length(groups) > 2) {
    samples <- sampleInfo$sample[sampleInfo$group %in% groups]
    tmpData <- sampleInfo |>
      dplyr::filter(group %in% groups) |>
      dplyr::mutate(group = factor(group, levels = groups)) |>
      dplyr::left_join(tibble(sample = samples, value = c_across(all_of(samples))), by = "sample")
    # print(tmpData)
    #+++++++++++++++++++++++++++++++++++++#
    # 对数据中的NA进行检测，当至少有两组且每组
    # 至少有两个以上非NA的值时，才进行ANOVA检验
    #+++++++++++++++++++++++++++++++++++++#
    testTmp <- tmpData |>
      dplyr::group_by(group) |>
      dplyr::summarise(n = sum(!is.na(value)))
    if (sum(testTmp$n > 1) > 1) {
      lamp.acv <- aov(value ~ group, data = tmpData)
      # print(lamp.acv)
      saov <- summary(lamp.acv)
      pvalue <- saov[[1]]$`Pr(>F)`[1]
      # post <- TukeyHSD(lamp.acv)
    } else {
      pvalue <- NA
    }
    res <- cbind(MeanRes, tibble::tibble(pvalue = pvalue))
  }
  return(res)
}


Diff <- function(dataFile, sampleInfoFile, compareInfoFile, logFile, outpath) {
  res <- CheckFile(dataFile, sampleInfoFile, compareInfoFile, logFile)
  dat <- res$dat
  sampleInfo <- res$sampleInfo
  compareInfo <- res$compareInfo

  for (i in 1:nrow(compareInfo)) {
    # i <- 1
    groups <- compareInfo$groups[[i]]
    compare <- compareInfo$compare[[i]]
    method <- compareInfo$method[[i]]
    samples <- sampleInfo$sample[sampleInfo$group %in% groups]

    dat |>
      dplyr::select(ID, dplyr::all_of(samples)) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        res = MyTest(groups = groups, method = method, sampleInfo = sampleInfo, logFile = logFile)
      ) |>
      tidyr::unnest(res) |>
      readr::write_csv(glue::glue("{outpath}/_{compare}_diff.csv"))
  }
}

Diff(dataFile, sampleInfoFile, compareInfoFile, logFile, outpath)
