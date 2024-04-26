## ----SetUp, echo = FALSE, eval = TRUE, results = "hide"-----------------------
# R options & configuration:
suppressPackageStartupMessages(library("knitr"))
suppressPackageStartupMessages(library("chemometrics"))

# Stuff specifically for knitr:
opts_chunk$set(eval = TRUE, echo = TRUE)

## ----echo = FALSE-------------------------------------------------------------
desc <- packageDescription("LearnPCA")

## ----echo = FALSE, results = "asis"-------------------------------------------
res <- knitr::knit_child("top_matter.md", quiet = TRUE)
cat(res, sep = '\n')

## ----PCA-Matrices, echo = FALSE, results = "show", fig.cap = "One way to look at the matrix algebra behind PCA. Reconstruction of the data matrix $\\mathbf{X}$ is achieved by multiplying the score matrix ($\\mathbf{S}$) by the transpose of the loadings matrix ($\\mathbf{L^T}$).  The method of matrix multiplication is symbolized in the red-dotted outlines: Each element of row $i$ of the scores matrix is multiplied by the corresponding element of column $j$ of the transposed loadings.  These results are summed to give a single entry in the original data matrix $\\mathbf{X}_{ij}$.", out.width = "75%", fig.align = "center"----

knitr::include_graphics("PCA_Matrices.png")

## ----accepted-----------------------------------------------------------------
set.seed(30)
X <- matrix(rnorm(100*50), ncol = 50) # generate random data
X <- scale(X, center = TRUE, scale = FALSE) # always work with centered data
X_svd <- svd(X)
X_pca <- prcomp(X, center = FALSE) # already centered

## ----power3-------------------------------------------------------------------
V <- rnorm(50)
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
mean(abs(X %*% V) - abs(X_pca$x[,1])) # compare the scores
mean(abs(V)- abs(X_pca$rotation[,1])) # compare the loadings

## ----eigen2-------------------------------------------------------------------
X_cor <- cor(X)
X_eig <- eigen(X_cor)

## ----eigen3-------------------------------------------------------------------
Q <- rnorm(50)
for (iter in 1:50) {
  Q <- X_cor %*% Q # Step 1
  Q <- Q/sqrt(sum(Q^2)) # Step 2

 if ((iter %% 5) == 0L) { # report every 5 steps; print the correlation between
   cat("\nIteration", iter, "\n") # the current Q and the actual values from SVD
   cat("\tcor with Q:", sprintf("%f", cor(X_eig$vectors[,1], Q)), "\n")
   }
}

## -----------------------------------------------------------------------------
mean(abs(Q) - abs(X_eig$vectors[,1])) # check the loadings

## -----------------------------------------------------------------------------
R_loadings <- X_pca$rotation[,1] 
std_loadings <- (X_svd$v %*% diag(X_svd$d)/(nrow(X - 1)))[,1]
plot(R_loadings, std_loadings, xlab = "R's notion of loadings", ylab = "Standard notion of loadings")

## -----------------------------------------------------------------------------
simpleNIPALS <- function(X, pcs = 3, tol = 1e-6) {
  S <- NULL # a place to store the scores
  L <- NULL # a place to store the loadings
  for (pc in 1:pcs) { # compute one PC at a time
    s_old <- X[,1] # initial estimate of the score Note 1
    done <- FALSE # flag to track progress
    while (!done) { # Note 2
      l <- t(X) %*% s_old # compute/improve/update loadings estimate
      l <- l/sqrt(sum(l^2)) # scale by l2 norm
      s_new <-X %*% l # compute/improve/update scores estimate
      err <- sqrt(sum((s_new - s_old)^2)) # compute the error of the score estimate
      if (err < tol) {done <- TRUE; next} # check error against tolerance,
                                          # exit if good
      s_old <- s_new # if continuing, use the improved score estimate
                     # next time through while loop
    }
    X <- X - s_new %*% t(l) # "deflate" the data by removing the 
                            # contribution of the results so far Note 3
    S <- cbind(S, s_new) # add the most recent scores to the growing results
    L <- cbind(L, l) # add the most recent loadings to the growing results
  }
  list(Scores = S, Loadings = L) # return the answers
}

## -----------------------------------------------------------------------------
N <- simpleNIPALS(X)
mean(abs(N$Scores[,1]) - abs(X_pca$x[,1])) # compare the scores from prcomp
mean(abs(N$Loadings[,1]) - abs(X_pca$rotation[,1])) # compare the loadings from prcomp
mean(abs(N$Scores[,1]) - abs(X %*% X_svd$v[,1])) # compare the scores from svd
mean(abs(N$Loadings[,1]) - abs(X_svd$v[,1])) # compare the loadings from svd

## ----echo = FALSE, results = "asis"-------------------------------------------
res <- knitr::knit_child("refer_to_works_consulted.md", quiet = TRUE)
cat(res, sep = '\n')

