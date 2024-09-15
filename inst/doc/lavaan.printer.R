## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(lavaan)
library(lavaan.printer)

## -----------------------------------------------------------------------------
# Adapted from the example of cfa()
model_cfa <- "visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6"
fit <- cfa(model_cfa,
           data = HolzingerSwineford1939,
           group = "school")
est <- parameterEstimates_table_list(fit,
                                     rsquare = TRUE)

## -----------------------------------------------------------------------------
print_parameterEstimates_table_list(est)

## -----------------------------------------------------------------------------
print_parameterEstimates_table_list(est,
                                    nd = 2,
                                    by_group = FALSE,
                                    drop_cols = "Z",
                                    na_str = "--")

## -----------------------------------------------------------------------------
add_sig <- function(object,
                    breaks = c(1, .05, .01, .001, -Inf),
                    labels = c("***", "** ", "*  ", "  ")) {
    tmp <- object[, "pvalue", drop = TRUE]
    if (!is.null(tmp)) {
        tmp[is.na(tmp)] <- 1
        tmp2 <- cut(tmp,
                    breaks = breaks,
                    labels = labels)
        i <- match("pvalue", colnames(object))
        out <- data.frame(object[, 1:i],
                          Sig = tmp2,
                          object[, seq(i + 1, ncol(object))])
      }
    out
  }

## -----------------------------------------------------------------------------
model_cfa <- "visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6"
fit <- cfa(model_cfa,
           data = HolzingerSwineford1939[1:100, ])
est <- parameterEstimates_table_list(fit,
                                     est_funs = list(add_sig))
print_parameterEstimates_table_list(est)

## -----------------------------------------------------------------------------
lavaan_missing <- function(x) {
    out0 <- attr(x, "missing")
    out1 <- data.frame(Option = "Missing",
                       Setting = out0)
    attr(out1, "section_title") <- "Additional Information:"
    out1
  }

## -----------------------------------------------------------------------------
footnotes <- function(x) {
    out0 <- c("- This is footnote 1.",
              paste("- This is footnote 2, a very very very very very",
                    "very very very very very very very very very very",
                    "very very long one. Wrapped by default"))
    attr(out0, "section_title") <- "Footnote:"
    attr(out0, "print_fun") <- "cat"
    out0
  }

## -----------------------------------------------------------------------------
model_cfa <- "visual  =~ x1 + x2 + x3"
fit <- cfa(model_cfa,
           data = HolzingerSwineford1939)
est <- parameterEstimates_table_list(fit,
                                     header_funs = list(lavaan_missing),
                                     footer_funs = list(footnotes))
print_parameterEstimates_table_list(est)

