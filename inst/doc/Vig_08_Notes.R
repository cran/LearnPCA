## ----SetUp, echo = FALSE, eval = TRUE, results = "hide"-----------------------
# R options & configuration:
set.seed(123)
suppressPackageStartupMessages(library("knitr"))
suppressPackageStartupMessages(library("LearnPCA"))
suppressPackageStartupMessages(library("xtable"))

# Stuff specifically for knitr:
opts_chunk$set(eval = TRUE, echo = TRUE, results = "show")

## ----echo = FALSE-------------------------------------------------------------
desc <- packageDescription("LearnPCA")

## ----echo = FALSE, results = "asis"-------------------------------------------
res <- knitr::knit_child("top_matter.md", quiet = TRUE)
cat(res, sep = '\n')

## ----echo = FALSE, results = "asis"-------------------------------------------
res <- knitr::knit_child("refer_to_works_consulted.md", quiet = TRUE)
cat(res, sep = '\n')

