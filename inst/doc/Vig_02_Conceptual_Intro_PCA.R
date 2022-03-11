## ----SetUp, echo = FALSE, eval = TRUE, results = "hide"-----------------------
# R options & configuration:
set.seed(9)
suppressPackageStartupMessages(library("knitr"))
suppressPackageStartupMessages(library("kableExtra"))
suppressPackageStartupMessages(library("chemometrics"))
suppressPackageStartupMessages(library("ChemoSpec"))

# Stuff specifically for knitr:
opts_chunk$set(eval = TRUE, echo = FALSE, results = "hide")

## ---- echo = FALSE------------------------------------------------------------
desc <- packageDescription("LearnPCA")

## ---- echo = FALSE, results = "asis"------------------------------------------
res <- knitr::knit_child("top_matter.md", quiet = TRUE)
cat(res, sep = '\n')

## ----dataTaste, results = "asis"----------------------------------------------
data(glass)
DF <- as.data.frame(glass[1:5, 1:8])
kable(DF, caption = "A portion of the archaeological glass data set. Values are percentages.") %>% kable_styling(c("striped", "bordered"))

## ----glassScree, fig.cap = "Scree plot from PCA on the glass data set."-------
pca <- prcomp(glass)
plot(pca, main = "")

## ----glassScores, fig.cap = "Score plot from PCA on the glass data set.", fig.asp = 1----
plot(pca$x[,1], pca$x[,2], type = "p", pch = 20,
  xlab = "Principal Component 1", ylab = "Principal Component 2")

## ----glassLoadings, fig.cap = "Loadings plot for PC 1 from PCA on the glass data set."----
barplot(pca$rotation[,1], cex.names = 0.7, ylab = "Contribution")

## ----elementCor, results = "asis"---------------------------------------------
tab <- cor(glass[,c(1, 4, 9)])

DF <- as.data.frame(tab)
kable(DF, digits = 2, caption = "Correlations among selected element concentrations in `glass` data set.") %>% kable_styling(c("striped", "bordered"), full_width = FALSE)

## ----screeTable, results = "asis"---------------------------------------------
eigensum <- sum(pca$sdev*pca$sdev)
variance <- 100*(pca$sdev*pca$sdev/eigensum)
cumvariance <- cumsum(variance)
labs <- paste("PC", 1:13, sep = " ")
DF <- data.frame(component = labs, variance = variance, cumulative = cumvariance)
kable(DF, digits = 0, caption = "Variance (signal) accounted for by PCs. Values in percent.") %>% kable_styling(c("striped", "bordered"), full_width = FALSE)

## ----glassScores2, fig.cap = "Score plot from PCA on the glass data set, with groups color-coded.", fig.asp = 1----
data(glass.grp)
data(Col7) # 7 colorblind-friendly colors from ChemoSpec
glass.col <- rep(NA_character_, length(glass.grp))
glass.col <- ifelse(glass.grp == 1L, Col7[1], glass.col)
glass.col <- ifelse(glass.grp == 2L, Col7[3], glass.col)
glass.col <- ifelse(glass.grp == 3L, Col7[4], glass.col)
glass.col <- ifelse(glass.grp == 4L, Col7[6], glass.col)
plot(pca$x[,1], pca$x[,2], type = "p", col = glass.col, pch = 20,
     xlab = "Principal Component 1", ylab = "Principal Component 2")

## ----sumState, results = "asis"-----------------------------------------------
x77 <- as.data.frame(apply(state.x77, 2, range))
kable(x77, caption = "The ranges of variables in `state.x77`") %>% kable_styling(c("striped", "bordered"))

## ----statePCA-----------------------------------------------------------------
x77 <- prcomp(state.x77, scale = TRUE)

## ----stateScreePlot, fig.cap = "Scree plot for the `state.x77` data after scaling."----
plot(x77, main = "")

## ----stateScreeTable, results = "asis"----------------------------------------
eigensum <- sum(x77$sdev*x77$sdev)
variance <- 100*(x77$sdev*x77$sdev/eigensum)
cumvariance <- cumsum(variance)
labs <- paste("PC", 1:8, sep = " ")
DF <- data.frame(component = labs, variance = variance, cumulative = cumvariance)
kable(DF, digits = 0, caption = "Variance accounted for by PCs. Values in percent.") %>% kable_styling(c("striped", "bordered"), full_width = FALSE)

## ----stateScores, fig.cap = "Score plot from PCA on the `states.x77` data set, colored by political leanings (ligth blue = democrat, pink = republican, purple = mixed).", fig.asp = 1----
state.col <- c(Col7[4], Col7[4], Col7[4], Col7[4], Col7[3], Col7[3], Col7[3], Col7[3], "purple", Col7[4], Col7[3], Col7[4], Col7[3], Col7[4], "purple", Col7[4], Col7[4], Col7[4], Col7[3], Col7[3], Col7[3], Col7[3], Col7[3], Col7[4], Col7[4], Col7[4], Col7[4], Col7[3], Col7[3], Col7[3], Col7[3], Col7[3], Col7[4], Col7[4], "purple", Col7[4], Col7[3], Col7[3], Col7[3], Col7[4], Col7[4], Col7[4], Col7[4], Col7[4], Col7[3], Col7[3], Col7[3], Col7[4], Col7[3], Col7[4])
plot(x77$x[,1], x77$x[,2], type = "p", pch = 20,
  xlab = "Principal Component 1", ylab = "Principal Component 2", col = state.col)

## ----stateLoadings, fig.cap = "Loadings plot for PC 1 from PCA on the `state.x77` data set."----
barplot(x77$rotation[,1], cex.names = 0.7, ylab = "Contribution")

## ----loadIR-------------------------------------------------------------------
data(SrE.IR)

## ----IRSpectrum, fig.cap = "Spectrum 1 from the IR data set."-----------------
xl <- rev(range(SrE.IR$freq))
plot(SrE.IR$freq, SrE.IR$data[1,], type = "l", xlim = xl,
  xlab = "wavenumbers", ylab = "absorbance")

## ----IRScree, fig.cap = "Scree plot from PCA on the IR data set."-------------
pca <- prcomp(SrE.IR$data)
plot(pca, main = "")

## ----IRScores, fig.cap = "Score plot from PCA on the IR data set.", fig.asp = 1----
plot(pca$x[,1], pca$x[,2], type = "p", pch = 20,
  xlab = "Principal Component 1", ylab = "Principal Component 2")

## ----IRLoadings, fig.cap = "Loadings plot for PC 1 from PCA on the IR data set."----
plot(SrE.IR$freq, pca$rotation[,1], type = "l", xlim = xl,
     xlab = "Wavenumber", ylab = "Contribution")

## ----IRLoadings2, fig.cap = "Loadings plot for PC 1 from PCA on the IR data set, carbonyl region. Reference spectrum shown in red."----
plot(SrE.IR$freq, pca$rotation[,1], type = "l", xlim = c(1800, 1650),
     xlab = "Wavenumber", ylab = "Contribution", ylim = c(-0.3, 0.3))
lines(SrE.IR$freq, SrE.IR$data[1,], col = Col7[4])
abline(v = c(1743, 1708), lty = 2, col = "gray50")

## ----IRLoadings3, fig.cap = "Loadings plot for PC 1 from PCA on the IR data set, carbonyl region, shown as a bar plot."----
plot(SrE.IR$freq, pca$rotation[,1], type = "h", xlim = c(1800, 1650),
     xlab = "Wavelength", ylab = "Contribution")

## ---- echo = FALSE, results = "asis"------------------------------------------
res <- knitr::knit_child("refer_to_works_consulted.md", quiet = TRUE)
cat(res, sep = '\n')

