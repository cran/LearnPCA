## ----SetUp, echo = FALSE, eval = TRUE, results = "hide"-----------------------
# R options & configuration:
set.seed(13)
suppressPackageStartupMessages(library("knitr"))
suppressPackageStartupMessages(library("plotrix"))
suppressPackageStartupMessages(library("latex2exp"))

# source functions
source("scores_loadings_functions.R")

# colors and color names
axis2_col = "#3db7ed"
axis2_colname = "light blue"
axis1_col = "#f748a5"
axis1_colname = "pink"
loadings_col = "#359b73"
loadings_colname = "green"
origdata_col = "#000000"
origdata_colname = "black"

# Stuff specifically for knitr:
opts_chunk$set(eval = TRUE, echo = FALSE, results = "hide")
opts_knit$set(eval.after = "fig.cap")

## ---- echo = FALSE------------------------------------------------------------
desc <- packageDescription("LearnPCA")

## ---- echo = FALSE, results = "asis"------------------------------------------
res <- knitr::knit_child("top_matter.md", quiet = TRUE)
cat(res, sep = '\n')

## ----prepData-----------------------------------------------------------------
# create data sets for use in vignette

data_set_1 = generate_data(size = 10)
rotate0 = rot_axes(angle = 0, file = data_set_1)
rotate15 = rot_axes(angle = 15, file = data_set_1)
rotate20 = rot_axes(angle = 20, file = data_set_1)
rotate30 = rot_axes(angle = 30, file = data_set_1)
rotate40 = rot_axes(angle = 40, file = data_set_1)
rotate45 = rot_axes(angle = 45, file = data_set_1)
rotate60 = rot_axes(angle = 60, file = data_set_1)
rotate75 = rot_axes(angle = 75, file = data_set_1)
rotate80 = rot_axes(angle = 80, file = data_set_1)

## ----fig-origData, fig.align="center", fig.cap="Scatterplot of variable 1 and variable 2 for our eight samples. The dashed lines are the original axes with variable 1 shown in pink and variable 2 shown in blue."----

plot_rot_axes(rotate0, show_rotated = FALSE, show_full_legend = FALSE,
              show_scores = FALSE, show_simple_legend = FALSE,
              show_loadings = FALSE, show_title = FALSE, show_projections = FALSE)
legend(x = "topleft", legend = c("variable 1", "variable 2"),
       lty = 2, lwd = 1, col = c(axis1_col,axis2_col), bty = "n")

## ----table-origData, results = "asis"-----------------------------------------
variable2 = data_set_1$xrot
variable1 = data_set_1$yrot
samples = seq(1,8)
df = data.frame(samples,variable1,variable2)
colnames(df) = c("sample", "variable 1", "variable 2")
kable(df, caption = "Individual values for the variables and samples.", digits = 2)
var2 = var(variable2)
var1 = var(variable1)
total_variance = var1 + var2
percent_var2 = 100*var2/total_variance
percent_var1 = 100*var1/total_variance

## ----rotate20, fig.align="center", fig.cap="Rotating the original axes by 20$^\\circ$ gives the axes shown as solid lines. The small points in pink and in blue are the projections of the original data onto the rotated axes. The gray lines serve as a guide to illustrate the projections for one data point."----
plot_rot_axes(rotate20, show_loadings = FALSE, show_simple_legend = FALSE,
              show_full_legend = FALSE, show_full_axes_legend = TRUE,
              show_scores = TRUE, show_projections = TRUE)

## ----rotate60, fig.align="center", fig.cap="Rotating the original axes by 60$^\\circ$ maximizes the variance along one of the two rotated axes; the line in pink is the first principal component axis and the line in blue is the second principal component axis.  The gray lines serve as a guide to illustrate the projections for one data point."----
plot_rot_axes(rotate60, show_loadings = FALSE, show_simple_legend = FALSE,
              show_full_legend = TRUE, show_full_axes_legend = FALSE,
              show_scores = TRUE, show_projections = TRUE)

## ----table-scores, results = "asis"-------------------------------------------
variable2 = data_set_1$xrot
variable1 = data_set_1$yrot
samples = c(1:8, "% variance")
pc1 = sqrt(rotate60$axis1_x^2 + rotate60$axis1_y^2) * sign(rotate60$axis1_x)
pc2 = sqrt(rotate60$axis2_x^2 + rotate60$axis2_y^2) * sign(rotate60$axis2_x)
# compute variances for a summary row
v1 <- var(variable1)
v2 <- var(variable2)
vpc1 <- var(pc1)
vpc2 <- var(pc2)
# add variances to data
variable1 <- c(variable1, 100*v1/(v1 + v2))
variable2 <- c(variable2, 100*v2/(v1 + v2))
pc1<- c(pc1, 100*vpc1/(vpc1 + vpc2))
pc2<- c(pc2, 100*vpc2/(vpc1 + vpc2))
# put it all together
df = data.frame(samples,variable1,variable2,pc1, pc2)
colnames(df) = c("sample", "variable 1", "variable 2", "PC 1", "PC 2")
kable(df, caption = "Coordinates for each of the eight samples, in the original axis system and in the principal component axis system.", digits = 2)

## ----rotate60-loadings, fig.align="center", fig.cap="Illustration showing the loadings for the first principal component axis."----
plot_rot_axes(rotate60, show_loadings = TRUE, show_simple_legend = FALSE,
              show_full_legend = TRUE, show_full_axes_legend = FALSE,
              show_scores = TRUE, show_projections = TRUE)

## ---- echo = FALSE, results = "asis"------------------------------------------
res <- knitr::knit_child("refer_to_works_consulted.md", quiet = TRUE)
cat(res, sep = '\n')

