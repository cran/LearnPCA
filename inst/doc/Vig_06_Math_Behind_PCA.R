## ----SetUp, echo = FALSE, eval = TRUE, results = "hide"-----------------------
# R options & configuration:
set.seed(9)
suppressPackageStartupMessages(library("knitr"))
suppressPackageStartupMessages(library("chemometrics"))

# Stuff specifically for knitr:
opts_chunk$set(eval = TRUE, echo = TRUE)

## ---- echo = FALSE------------------------------------------------------------
desc <- packageDescription("LearnPCA")

## ---- echo = FALSE, results = "asis"------------------------------------------
res <- knitr::knit_child("top_matter.md", quiet = TRUE)
cat(res, sep = '\n')

## ----PCA-Matrices, echo = FALSE, results = "show", fig.cap = "One way to look at the matrix algebra behind PCA. Reconstruction of the data matrix $\\mathbf{X}$ is achieved by multiplying the score matrix ($\\mathbf{S}$) by the transpose of the loadings matrix ($\\mathbf{L}$).  The method of matrix multiplication is symbolized in the red-dotted outlines: Each element of row $i$ of the scores matrix is multiplied by the corresponding element of column $j$ of the transposed loadings.  These results are summed to give a single entry in the original data matrix $\\mathbf{X}_{ij}$.", out.width = "75%", fig.align = "center"----

knitr::include_graphics("Graphics/PCA_Matrices.png")

## ----power1-------------------------------------------------------------------
set.seed(30)
X <- matrix(rnorm(100*50), ncol = 50)
V <- rnorm(50)

## ----power2-------------------------------------------------------------------
X_svd <- svd(X)

## ----power3-------------------------------------------------------------------
for (iter in 1:50) {
  U <- X %*% V # Step 1
  U <- U/sqrt(sum(U^2)) # Step 2
  V <- t(X) %*% U # Step 3
  V <- V/sqrt(sum(V^2)) # Step 4
 if ((iter %% 10) == 0L) { # report every 10 steps; print the correlation between
   cat("\nIteration", iter, "\n") # the current U or V and the actual values from SVD
   cat("\tcor with V:", sprintf("%f", cor(X_svd$v[,1], V)), "\n")
   cat("\tcor with U:", sprintf("%f", cor(X_svd$u[,1], U)), "\n")
   }
}

## ----comp1--------------------------------------------------------------------
mean(abs(V) - abs(X_svd$v[,1]))
mean(abs(U) - abs(X_svd$u[,1]))

## -----------------------------------------------------------------------------
PCA <- prcomp(X)
mean(abs(X %*% V) - abs(PCA$x[,1])) # compare the scores
mean(abs(V)- abs(PCA$rotation[,1])) # compare the loadings

## ----eigen1-------------------------------------------------------------------
set.seed(30)
X <- matrix(rnorm(100*50), ncol = 50)
X <- cor(X)
Q <- rnorm(50)

## ----eigen2-------------------------------------------------------------------
X_eig <- eigen(X)

## ----eigen3-------------------------------------------------------------------
for (iter in 1:50) {
  Q <- X %*% Q # Step 1
  Q <- Q/sqrt(sum(Q^2)) # Step 2

 if ((iter %% 5) == 0L) { # report every 5 steps; print the correlation between
   cat("\nIteration", iter, "\n") # the current Q and the actual values from SVD
   cat("\tcor with Q:", sprintf("%f", cor(X_eig$vectors[,1], Q)), "\n")
   }
}

## -----------------------------------------------------------------------------
mean(abs(Q) - abs(X_eig$vectors[,1])) # check the loadings

## ---- echo = FALSE, results = "asis"------------------------------------------
res <- knitr::knit_child("refer_to_works_consulted.md", quiet = TRUE)
cat(res, sep = '\n')

