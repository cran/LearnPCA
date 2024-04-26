## ----echo = FALSE-------------------------------------------------------------
desc <- packageDescription("LearnPCA")

## ----echo = FALSE, results = "asis"-------------------------------------------
res <- knitr::knit_child("top_matter.md", quiet = TRUE)
cat(res, sep = '\n')

## ----echo = FALSE, results = "asis"-------------------------------------------
res <- knitr::knit_child("works_consulted.md", quiet = TRUE)
cat(res, sep = '\n')

