## ----SetUp, echo = FALSE, eval = TRUE, results = "hide"-----------------------
# R options & configuration:
set.seed(9)
suppressPackageStartupMessages(library("knitr"))
suppressPackageStartupMessages(library("chemometrics"))
suppressPackageStartupMessages(library("LearnPCA"))
suppressPackageStartupMessages(library("ade4"))
suppressPackageStartupMessages(library("latex2exp"))

# Stuff specifically for knitr:
opts_chunk$set(eval = TRUE, echo = FALSE, results = "hide")

## ----echo = FALSE-------------------------------------------------------------
desc <- packageDescription("LearnPCA")

## ----echo = FALSE, results = "asis"-------------------------------------------
res <- knitr::knit_child("top_matter.md", quiet = TRUE)
cat(res, sep = '\n')

## ----prep-data, echo = TRUE, eval = TRUE, results = "show"--------------------
data(tintoodiel) # activate the data set from package ade4
TO <- tintoodiel$tab # to save typing, rename the element with the data
# select just a few samples (in rows) & variables (in columns)
FeCu <- TO[28:43,c("Fe2O3", "Cu")]
summary(FeCu)

## ----metals-raw-table, results = "asis"---------------------------------------
if (!is_latex_output()) {
  FeCu2 <- FeCu # create a copy to format the column names
  names(FeCu2) <- c("Fe~2~O~3~", "Cu")
  kable(FeCu2, row.names = FALSE, caption = "The FeCu data set. Values for Fe~2~O~3~ are percentages, those for Cu are ppm.")
}

if (is_latex_output()) {
  kable(FeCu, row.names = FALSE, caption = "The FeCu data set. Values for $\\mathrm{Fe_{2}O_{3}}$ are percentages, those for Cu are ppm.")
}



## ----plot-raw-dotplot, eval = TRUE, fig.cap = "The range of the raw data values in `FeCu`."----
stripchart(FeCu, vertical = TRUE, pch = 20, xlim = c(0.5, 2.5),
  ylab = TeX(r"(Values ($Fe_{2}O_{3}$ in percent, Cu in ppm))"),
  xlab = "")

## ----plot-raw-data-2D, eval = TRUE, fig.cap = "The relationship between the raw data values in `FeCu`.", fig.asp = 1----
plot(x = FeCu$Fe2O3, y = FeCu$Cu, pch = 20,
  xlab = TeX(r"($Fe_{2}O_{3}$ (percent))"),
  ylab = "Cu (ppm)")

## ----center-raw-data, echo = TRUE---------------------------------------------
FeCu_centered <- scale(FeCu, scale = FALSE, center = TRUE) # see ?scale for defaults

## ----plot-centered-data, eval = TRUE, fig.cap = "Centered data values in `FeCu`."----
stripchart(as.data.frame(FeCu_centered), vertical = TRUE, pch = 20, xlim = c(0.5, 2.5), ylab = "centered values", xlab = "")

## ----scale-centered-data, echo = TRUE-----------------------------------------
FeCu_centered_scaled <- scale(FeCu_centered, center = FALSE, scale = TRUE) # see ?scale for defaults

## ----show-std-dev, echo = TRUE, results = "show"------------------------------
apply(FeCu_centered_scaled, 2, sd)

## ----plot-centered-scaled-data, fig.cap = "Centered and scaled data."---------
stripchart(as.data.frame(FeCu_centered_scaled), vertical = TRUE, pch = 20, xlim = c(0.5, 2.5), ylab = "centered & scaled values", xlab = "")

## ----prcomp, echo = TRUE, results = "show"------------------------------------
pca_FeCu <- prcomp(FeCu_centered_scaled)
str(pca_FeCu)

## ----plot-scores, fig.cap = "Scores.", fig.asp = 1----------------------------
plot(pca_FeCu$x, pch = 20, xlab = "PC1", ylab = "PC2")

## ----process-all-data, echo = TRUE--------------------------------------------
pca_TO <- prcomp(TO, scale. = TRUE)

## ----plot-all-data, fig.cap = "Score plot using all the data.", fig.asp = 1----
plot(pca_TO$x, pch = 20, xlab = "PC1", ylab = "PC2")

## ----scree, echo = TRUE, fig.cap = "Scree plot."------------------------------
plot(pca_FeCu, main = "")

## ----loading1, echo = TRUE, fig.cap = "Plot of the loadings on PC1."----------
barplot(pca_FeCu$rotation[,1], main = "")

## ----recon, eval = FALSE, echo = TRUE-----------------------------------------
#  Xhat <- pca_FeCu$x[, 1:z] %*% t(pca_FeCu$rotation[, 1:z])

## ----unscale, eval = FALSE, echo = TRUE---------------------------------------
#  Xhat <- scale(Xhat, center = FALSE, scale = 1/pca_FeCu$scale)

## ----uncenter, eval = FALSE, echo = TRUE--------------------------------------
#  Xhat <- scale(Xhat, center =  -pca_FeCu$center, scale = FALSE)

## ----full-recon-2, echo = TRUE, results = "TRUE"------------------------------
TOhat <- pca_TO$x %*% t(pca_TO$rotation)
TOhat <- scale(TOhat, center = FALSE, scale = 1/pca_TO$scale)
TOhat <- scale(TOhat, center = -pca_TO$center, scale = FALSE)

## ----mean-diff, echo = TRUE, results = "show"---------------------------------
mean(TOhat - as.matrix(TO))

## ----reconstruction, results = "show"-----------------------------------------
ntests <- ncol(TO)
rmsd <- rep(NA_real_, ntests)
for (i in 1:ntests) {
  ans <- XtoPCAtoXhat(TO, i, sd)
  error <- ans - TO
  rmsd[i] <- sqrt(sum(error^2)/length(error)) # RMSD
}

## ----rmsd, fig.cap = "Reduction of error as the number of components included in the reconstruction increases."----
plot(rmsd, type = "b", ylim = c(0.0, max(rmsd)),
  xlab = "No. of Components Retained",
  ylab = "Error")
abline(h = 0.0, col = "pink")

## ----echo = FALSE, results = "asis"-------------------------------------------
res <- knitr::knit_child("refer_to_works_consulted.md", quiet = TRUE)
cat(res, sep = '\n')

