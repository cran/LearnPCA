## ----SetUp, echo = FALSE, eval = TRUE, results = "hide"-----------------------
# R options & configuration:
set.seed(123)
suppressPackageStartupMessages(library("knitr"))
suppressPackageStartupMessages(library("LearnPCA"))
suppressPackageStartupMessages(library("kableExtra"))
suppressPackageStartupMessages(library("xtable"))


# Stuff specifically for knitr:
opts_chunk$set(eval = TRUE, echo = TRUE, results = "show")

## ---- echo = FALSE------------------------------------------------------------
desc <- packageDescription("LearnPCA")

## ---- echo = FALSE, results = "asis"------------------------------------------
res <- knitr::knit_child("top_matter.md", quiet = TRUE)
cat(res, sep = '\n')

## -----------------------------------------------------------------------------
A <- matrix(1:20, nrow = 4)
dim(A)

## -----------------------------------------------------------------------------
Ac <- scale(A, center = TRUE, scale = FALSE)
covA <- (t(Ac) %*% Ac)/(nrow(Ac) -1)
dim(covA) # it's square
identical(covA, cov(A))

## -----------------------------------------------------------------------------
Acs <- scale(A, center = TRUE, scale = TRUE)
corA <- (t(Acs) %*% Acs)/(nrow(Acs) -1)
dim(corA) # square like covA
identical(corA, cor(A))

## -----------------------------------------------------------------------------
data(mtcars)
mt <- as.matrix(mtcars[seq(1, 20, 2), 1:5]) # select every other row to get 10 samples

## -----------------------------------------------------------------------------
pca <- prcomp(mt, center = TRUE, scale. = TRUE)
prin <- princomp(mt, cor = TRUE)

## -----------------------------------------------------------------------------
pca$x

## -----------------------------------------------------------------------------
prin$scores

## -----------------------------------------------------------------------------
all.equal(abs(prin$scores), abs(pca$x), check.attributes = FALSE)

## ----overlayScores, fig.asp = 1, fig.cap = "Comparison of scores computed by `prcomp` (black points) and `princomp` (pink points). Note that they are related by a 180 degreee rotation about an axis out of paper at the origin (plus a little wiggle due to algorithm differences).", echo = FALSE----
plot(pca$x[,1], pca$x[,2], type = "p", pch = 20, col = "black",
  xlim = c(-3, 3), ylim = c(-3, 3),
  xlab = "PC1", ylab = "PC2")
points(prin$scores[,1], prin$scores[,2], pch = 20, col = "pink")
abline(v = 0, h = 0, lty = 2)

## -----------------------------------------------------------------------------
pca$rotation

## -----------------------------------------------------------------------------
prin_loads <- matrix(prin$loadings, ncol = 5, byrow = FALSE)
prin_loads

## -----------------------------------------------------------------------------
all.equal(abs(pca$rotation), abs(prin_loads), check.attributes = FALSE)

## ----compareLoads, fig.cap = "A comparison of the signs of the first loadings from `prcomp` (black circles) and `princomp` (pink circles). Where the circles coincide, the value of the loading from each method is the same. ", echo = FALSE, fig.height = 3, fig.width = 5----
plot(x = 1:25, type = "n", axes = FALSE,
  xlab = "loadings", ylab = "sign", ylim = c(-1.1, 1.1))
points(sign(c(pca$rotation)))
points(sign(c(prin_loads)), col = "pink", cex = 1.5)
line.pos <- c(5.5, 10.5, 15.5, 20.5)
abline(v = line.pos, col = "gray")
axis(side = 2, labels = c("-1", "1"), at = c(-1, 1), lwd = 0, lwd.ticks = 0)
lab.pos <- c(line.pos, 25.5)
axis(side = 1, labels = c("PC1", "PC2", "PC3", "PC4", "PC5"), at = lab.pos - 2.5, lwd = 0, lwd.ticks = 0)

## ----varExp-------------------------------------------------------------------
summary(pca)
summary(prin)

## -----------------------------------------------------------------------------
MTpca <- PCAtoXhat(pca) # not checking attributes as the internal labeling varies between functions
all.equal(MTpca, mt, check.attributes = FALSE)

## -----------------------------------------------------------------------------
MTprin <- PCAtoXhat(prin)
all.equal(MTprin, mt, check.attributes = FALSE)

## -----------------------------------------------------------------------------
mt_cen_scl <- scale(mt, center = TRUE, scale = TRUE)

## -----------------------------------------------------------------------------
sing <- svd(mt_cen_scl)

## -----------------------------------------------------------------------------
svd_scores <- mt_cen_scl %*% sing$v
all.equal(svd_scores, pca$x, check.attributes = FALSE)

## -----------------------------------------------------------------------------
all.equal(sing$v, pca$rotation, check.attributes = FALSE)

## -----------------------------------------------------------------------------
mt_cen_scl_cor <- cor(mt_cen_scl)
eig <- eigen(mt_cen_scl_cor)

## -----------------------------------------------------------------------------
eig_scores <- mt_cen_scl %*% eig$vectors
all.equal(abs(eig_scores), abs(prin$scores), check.attributes = FALSE)

## -----------------------------------------------------------------------------
all.equal(abs(eig$vectors), abs(prin_loads), check.attributes = FALSE)

## ---- echo = FALSE------------------------------------------------------------
# c1 gives the row names
c1 = c("algorithm", "input dimensions", "user pre-processing", "internal pre-processing", "call", "scores", "loadings", "pct variance explained")
c2 = c("uses svd", "$n \\times p$", "center; scale", "center; scale", "$pca \\leftarrow prcomp(X)$", "$pca\\$x$", "$pca\\$rotation$", "$\\cfrac{100*pca\\$sdev^2}{sum(pca\\$sdev^2)}$")
c3 = c("svd", "$n \\times p$", "center; scale", "none", "$sing \\leftarrow svd(X)$", "$X \\times sing\\$v$", "$sing\\$v$", "$\\cfrac{100*sing\\$d^2}{sum(sing\\$d^2)}$")
c4 <- c("uses eigen", "$n \\times p$", "don't do it!", "center; cov or cor", "$prin \\leftarrow princomp(X)$", "$prin\\$scores$", "$prin\\$loadings$", "$\\cfrac{100*prin\\$sdev^2}{sum(prin\\$sdev^2)}$")
c5 <- c("eigen", "$n \\times n$", "center; scale; cov or cor", "none", "$eig \\leftarrow eigen(X)$", "$X \\times eig\\$vectors$", "$eig\\$vectors$", "$\\cfrac{100*eig\\$values}{sum(eig\\$values)}$")

DF <- data.frame(c1, c2, c3, c4, c5)
colnames(DF) <- c("", "prcomp", "svd", "princomp", "eigen")

## ----SumTable, echo = FALSE, results = "asis"---------------------------------
if (!is_latex_output()) {
  kbl(DF, row.names = FALSE, caption = "A comparison of R functions for PCA.  Input data must be centered in all cases.  Scaling is optional.") %>% kable_styling(c("striped", "bordered"), full_width = FALSE)
}

if (is_latex_output()) {
  bold <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
  tbl <- xtable(DF)
  caption(tbl) <- "A comparison of R functions for PCA.  Input data must be centered in all cases.  Scaling is optional."
  label(tbl) <- "tab:SumTable"
  print(tbl, include.rownames = FALSE,
    hline.after = 0:8,
    sanitize.text.function = function(x) {x},
    sanitize.colnames.function = bold,
    comment = FALSE)
}

## ---- echo = FALSE, results = "asis"------------------------------------------
res <- knitr::knit_child("refer_to_works_consulted.md", quiet = TRUE)
cat(res, sep = '\n')

