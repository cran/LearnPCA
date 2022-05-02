## ----SetUp, echo = FALSE, eval = TRUE, results = "hide"-----------------------
# R options & configuration:
set.seed(13)
suppressPackageStartupMessages(library("knitr"))
suppressPackageStartupMessages(library("plot3D"))
suppressPackageStartupMessages(library("plotly"))

# colors and color names
pcdata_col <- "#3db7ed"
pcdata_colname <- "light blue"
pcproj_col <- "#f748a5"
pcproj_colname <- "pink"
pcaxis_col <- "#d55e00"
pcaxis_colname <- "brown"
xyzaxis_col <- "#000000"
xyzaxis_colname <- "black"

# Stuff specifically for knitr:
opts_chunk$set(eval = TRUE, echo = FALSE, results = "hide")
opts_knit$set(eval.after = "fig.cap")

## ---- echo = FALSE------------------------------------------------------------
desc <- packageDescription("LearnPCA")

## ----top-matter, echo = FALSE, results = "asis"-------------------------------
res <- knitr::knit_child("top_matter.md", quiet = TRUE)
cat(res, sep = "\n")

## ----prepData-----------------------------------------------------------------
# set coordinates for center of ellipsoid
x0 <- 0
y0 <- 0
z0 <- 0

# set dimensions of ellipsoid relative to center; values chosen to
# make x-axis more important than y-axis, which is more important
# than the z-axis; thus pc1 is x-axis, pc2 is y-axis, pc3 = z-axis
xa <- 15
yb <- 9
zc <- 2

# generate set of random points within the ellipsoid's boundaries
# done by first generating random points within rectangular solid that
# encompasses the ellipsoid
set.seed(13)
x <- runif(400, min = -xa, max = xa)
y <- runif(400, min = -yb, max = yb)
z <- runif(400, min = -zc, max = zc)

# determine which points have (x,y,z) values that are inside the
# ellipsoid using equation for ellipsoid; a negative value for
# check means the point is inside of ellipsoid; flag as id
check <- (x - x0)^2 / xa^2 + (y - y0)^2 / yb^2 + (z - z0)^2 / zc^2 - 1
id <- which(check < 0)

# extract sets of (x,y,z) points inside of ellipsoid
xe <- x[id]
ye <- y[id]
ze <- z[id]

# function to rotate data and axes; a, b, and g are angles for rotation
# around the z, y, and x axes; see en.wikipedia.org/wiki/Rotation_matrix
rot <- function(a = 10, b = 10, g = 10, x = xe, y = ye, z = ze) {
  xrot <- cos(a) * cos(b) * x + (cos(a) * sin(b) * sin(g) - sin(a) * cos(g)) * y + (cos(a) * sin(b) * cos(g) + sin(a) * sin(g)) * z
  yrot <- sin(a) * cos(b) * x + (sin(a) * sin(b) * sin(g) + cos(a) * cos(g)) * y + (sin(a) * sin(b) * cos(g) - cos(a) * sin(g)) * z
  zrot <- -sin(b) * x + cos(b) * sin(g) * y + cos(b) * cos(g) * z
  out <- list(
    "xrot" = xrot,
    "yrot" = yrot,
    "zrot" = zrot
  )
}

# original pc axes (same as x,y,z axes)
xpc1 <- c(-xa, xa)
ypc1 <- c(0, 0)
zpc1 <- c(0, 0)
xpc2 <- c(0, 0)
ypc2 <- c(-xa, xa)
zpc2 <- c(0, 0)
xpc3 <- c(0, 0)
ypc3 <- c(0, 0)
zpc3 <- c(-xa, xa)

# rotate the pc axes
pc1 <- rot(x = xpc1, y = ypc1, z = zpc1)
pc2 <- rot(x = xpc2, y = ypc2, z = zpc2)
pc3 <- rot(x = xpc3, y = ypc3, z = zpc3)

# rotate the original data
rotdata <- rot()

# pca results in case they are of interest
pc_results <- prcomp(data.frame(rotdata$xrot, rotdata$yrot, rotdata$zrot))

## ----origData, fig.cap=paste("Figure 1. Three-dimensional plot of data (", pcdata_colname, "points) showing the x, y, and z-axes (",xyzaxis_colname, "lines) that represent the three measured variables."), out.width = "80%", results = "show"----

if (is_latex_output()) {
  # plot rotated data and original pc axes (original x,y,z)
  scatter3D(
    x = rotdata$xrot, y = rotdata$yrot, z = rotdata$zrot,
    pch = 19, col = pcdata_col, bty = "b2", cex = 0.3,
    xlim = c(-xa, xa), ylim = c(-xa, xa), zlim = c(-xa, xa),
    phi = 10, theta = 50, ticktype = "detailed"
  )
  points3D(
    x = xpc1, y = ypc1, z = zpc1, type = "l", col = xyzaxis_col,
    lwd = 2, lty = 1, add = TRUE
  )
  points3D(
    x = xpc2, y = ypc2, z = zpc2, type = "l", col = xyzaxis_col,
    lwd = 2, lty = 1, add = TRUE
  )
  points3D(
    x = xpc3, y = ypc3, z = zpc3, type = "l", col = xyzaxis_col,
    lwd = 2, lty = 1, add = TRUE
  )
}

if (!is_latex_output()) {
  axis_width <- 8L
  rdata <- as.data.frame(rotdata)
  x_axis <- data.frame(xpc1, ypc1, zpc1)
  y_axis <- data.frame(xpc2, ypc2, zpc2)
  z_axis <- data.frame(xpc3, ypc3, zpc3)
  fig <- plot_ly(
    name = "data", rdata, x = ~xrot, y = ~yrot, z = ~zrot,
    marker = list(size = 2.0, color = pcdata_col)
  ) %>%
    add_markers() %>%
    add_trace(name = "x axis", data = x_axis, x = ~xpc1, y = ~ypc1, z = ~zpc1, mode = "lines", type = "scatter3d", inherit = FALSE, line = list(width = axis_width, color = xyzaxis_col)) %>%
    add_trace(name = "y axis", data = y_axis, x = ~xpc2, y = ~ypc2, z = ~zpc2, mode = "lines", type = "scatter3d", inherit = FALSE, line = list(width = axis_width, color = xyzaxis_col)) %>%
    add_trace(name = "z axis", data = z_axis, x = ~xpc3, y = ~ypc3, z = ~zpc3, mode = "lines", type = "scatter3d", inherit = FALSE, line = list(width = axis_width, color = xyzaxis_col)) %>%
    layout(scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "z")
    ))
fig
}

## ----bonus, fig.cap=paste("Bonus Figure (pdf version only). The original data (as", pcdata_colname, "points) and their projection onto the x,y-plane, the y,z-plane, and the x,z-plane (as", pcproj_colname," points).")----

if (is_latex_output()) {
  scatter3D(
    x = rotdata$xrot, y = rotdata$yrot, z = rotdata$zrot,
    pch = 19, col = pcdata_col, bty = "b2", cex = 0.3,
    xlim = c(-xa, xa), ylim = c(-xa, xa), zlim = c(-xa, xa),
    phi = 10, theta = 50, ticktype = "detailed"
  )
  scatter3D(
    x = rotdata$xrot, y = rotdata$yrot, z = rep(-xa, length(id)),
    pch = 19, col = pcproj_col, cex = 0.2, add = TRUE
  )
  scatter3D(
    x = rep(-xa, length(id)), y = rotdata$yrot, z = rotdata$zrot,
    pch = 19, col = pcproj_col, cex = 0.2, add = TRUE
  )
  scatter3D(
    x = rotdata$yrot, y = rep(xa, length(id)), z = rotdata$zrot,
    pch = 19, col = pcproj_col, cex = 0.2, add = TRUE
  )
}

## ----pc1b, fig.cap=paste("Figure 2. The original data (as", pcdata_colname, "points) and the first principal component axis (as a", pcaxis_colname, "line)."), results = "show", out.width = "80%"----

if (is_latex_output()) {
  scatter3D(
    x = rotdata$xrot, y = rotdata$yrot, z = rotdata$zrot,
    pch = 19, col = pcdata_col, bty = "b2", cex = 0.3,
    xlim = c(-xa, xa), ylim = c(-xa, xa), zlim = c(-xa, xa),
    phi = 10, theta = 50, ticktype = "detailed"
  )
  points3D(
    x = pc1$xrot, y = pc1$yrot, z = pc1$zrot, type = "l", col = pcaxis_col,
    lwd = 2, lty = 1, add = TRUE
  )
}

if (!is_latex_output()) {
  fig <- plot_ly(
    name = "data", rdata, x = ~xrot, y = ~yrot, z = ~zrot,
    marker = list(size = 2.0, color = pcdata_col)
  ) %>%
    add_markers() %>%
    add_trace(name = "PC1", data = pc1, x = ~xrot, y = ~yrot, z = ~zrot, mode = "lines", type = "scatter3d", inherit = FALSE, line = list(color = pcaxis_col, width = axis_width)) %>%
    layout(scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "z")
    ))
  fig
}

## ----pc2a, fig.cap=paste("Figure 3. The first principal component (",pcaxis_colname, "line) and the projection of the original data (",pcdata_colname,"points) onto the plane perpendicular to the first principal component (shown with a", pcdata_colname, "boundary)."), results = "show", out.width = "80%"----

if (is_latex_output()) {
  proj <- rot(x = rep(0, length(id)), y = ye, z = ze)
  scatter3D(
    x = proj$xrot, y = proj$yrot, z = proj$zrot,
    pch = 19, col = pcdata_col, cex = 0.2, ticktype = "detailed",
    phi = 10, theta = 50, bty = "b2",
    xlim = c(-xa, xa), ylim = c(-xa, xa), zlim = c(-xa, xa)
  )
  polygon3D(
    x = c(pc3$xrot[2], pc2$xrot[2], pc3$xrot[1], pc2$xrot[1]),
    y = c(pc3$yrot[2], pc2$yrot[2], pc3$yrot[1], pc2$yrot[1]),
    z = c(pc3$zrot[2], pc2$zrot[2], pc3$zrot[1], pc2$zrot[1]),
    col = adjustcolor("white", alpha.f = 0.1), border = pcdata_col,
    add = TRUE
  )
  points3D(
    x = pc1$xrot, y = pc1$yrot, z = pc1$zrot, type = "l", col = pcaxis_col,
    lwd = 2, lty = 1, add = TRUE
  )
}

if (!is_latex_output()) {
  proj <- as.data.frame(rot(x = rep(0, length(id)), y = ye, z = ze))

  # DFs to define 4 lines to draw "surface"
  P1P2 <- data.frame(
    x = c(pc3$xrot[2], pc2$xrot[2]),
    y = c(pc3$yrot[2], pc2$yrot[2]),
    z = c(pc3$zrot[2], pc2$zrot[2])
  )

  P2P3 <- data.frame(
    x = c(pc2$xrot[2], pc3$xrot[1]),
    y = c(pc2$yrot[2], pc3$yrot[1]),
    z = c(pc2$zrot[2], pc3$zrot[1])
  )

  P3P4 <- data.frame(
    x = c(pc3$xrot[1], pc2$xrot[1]),
    y = c(pc3$yrot[1], pc2$yrot[1]),
    z = c(pc3$zrot[1], pc2$zrot[1])
  )

  P4P1 <- data.frame(
    x = c(pc3$xrot[2], pc2$xrot[1]),
    y = c(pc3$yrot[2], pc2$yrot[1]),
    z = c(pc3$zrot[2], pc2$zrot[1])
  )

  fig <- plot_ly(
    name = "data", proj, x = ~xrot, y = ~yrot, z = ~zrot,
    marker = list(size = 2.0, color = pcdata_col)
  ) %>%
    add_markers() %>%
    add_trace(name = "PC1", data = pc1, x = ~xrot, y = ~yrot, z = ~zrot, mode = "lines", type = "scatter3d", inherit = FALSE, line = list(color = pcaxis_col, width = axis_width)) %>%
    # Add PC2 projection plane rectangle as 4 lines; no 3d polygon appears to exist in plotly.  Need 4 DFs to do this!
    add_trace(name = "line1", data = P1P2, x = ~x, y = ~y, z = ~z, mode = "lines", type = "scatter3d", inherit = FALSE, showlegend = FALSE, line = list(color = pcdata_col, width = axis_width / 2)) %>%
    add_trace(name = "line2", data = P2P3, x = ~x, y = ~y, z = ~z, mode = "lines", type = "scatter3d", inherit = FALSE, showlegend = FALSE, line = list(color = pcdata_col, width = axis_width / 2)) %>%
    add_trace(name = "line3", data = P3P4, x = ~x, y = ~y, z = ~z, mode = "lines", type = "scatter3d", inherit = FALSE, showlegend = FALSE, line = list(color = pcdata_col, width = axis_width / 2)) %>%
    add_trace(name = "line4", data = P4P1, x = ~x, y = ~y, z = ~z, mode = "lines", type = "scatter3d", inherit = FALSE, showlegend = FALSE, line = list(color = pcdata_col, width = axis_width / 2)) %>%
    layout(scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "z")
    ))
  fig
}

## ----pc2b, fig.cap=paste("Figure 4. The result of adding the second principal component axis to the previous figure. The first principal component axis is the solid", pcaxis_colname, "line and the second principal component axis is the dashed", pcaxis_colname, "line."), results = "show", out.width = "80%"----

if (is_latex_output()) {
  scatter3D(
    x = proj$xrot, y = proj$yrot, z = proj$zrot,
    pch = 19, col = adjustcolor(pcdata_col, alpha.f = 0.5), cex = 0.2,
    ticktype = "detailed", phi = 10, theta = 50, bty = "b2",
    xlim = c(-xa, xa), ylim = c(-xa, xa), zlim = c(-xa, xa)
  )
  points3D(
    x = pc1$xrot, y = pc1$yrot, z = pc1$zrot, type = "l", col = pcaxis_col,
    lwd = 2, lty = 1, add = TRUE
  )
  points3D(
    x = pc2$xrot, y = pc2$yrot, z = pc2$zrot, type = "l", col = pcaxis_col,
    lwd = 2, lty = 2, add = TRUE
  )
}

if (!is_latex_output()) {
  fig <- plot_ly(
    name = "data", proj, x = ~xrot, y = ~yrot, z = ~zrot,
    marker = list(size = 2.0, color = pcdata_col)
  ) %>%
    add_markers() %>%
    add_trace(name = "PC1", data = pc1, x = ~xrot, y = ~yrot, z = ~zrot, mode = "lines", type = "scatter3d", inherit = FALSE, line = list(color = pcaxis_col, width = axis_width)) %>%
    add_trace(name = "PC2", data = pc2, x = ~xrot, y = ~yrot, z = ~zrot, mode = "lines", type = "scatter3d", inherit = FALSE, line = list(color = pcaxis_col, width = axis_width, dash = "dash")) %>%
    layout(scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "z")
    ))
  fig
}

## ----pc3, fig.cap=paste("Figure 5. The original data (", pcdata_colname, "points) and the three three principal component axes (", pcaxis_colname, "lines). The solid line is the first principal component, the dashed line is the second principal component, and the dotted line is the third principal component."), results = "show", out.width = "80%"----

if (is_latex_output()) {
  scatter3D(
    x = rotdata$xrot, y = rotdata$yrot, z = rotdata$zrot,
    pch = 19, col = pcdata_col, bty = "b2", cex = 0.3,
    xlim = c(-xa, xa), ylim = c(-xa, xa), zlim = c(-xa, xa),
    phi = 10, theta = 50, ticktype = "detailed"
  )
  points3D(
    x = pc1$xrot, y = pc1$yrot, z = pc1$zrot, type = "l", col = pcaxis_col,
    lwd = 2, lty = 1, add = TRUE
  )
  points3D(
    x = pc2$xrot, y = pc2$yrot, z = pc2$zrot, type = "l", col = pcaxis_col,
    lwd = 2, lty = 2, add = TRUE
  )
  points3D(
    x = pc3$xrot, y = pc3$yrot, z = pc3$zrot, type = "l", col = pcaxis_col,
    lwd = 2, lty = 3, add = TRUE
  )
}

if (!is_latex_output()) {
  fig <- plot_ly(
    name = "data", rdata, x = ~xrot, y = ~yrot, z = ~zrot,
    marker = list(size = 2.0, color = pcdata_col)
  ) %>%
    add_markers() %>%
    add_trace(name = "PC1", data = pc1, x = ~xrot, y = ~yrot, z = ~zrot, mode = "lines", type = "scatter3d", inherit = FALSE, line = list(color = pcaxis_col, width = axis_width)) %>%
    add_trace(name = "PC2", data = pc2, x = ~xrot, y = ~yrot, z = ~zrot, mode = "lines", type = "scatter3d", inherit = FALSE, line = list(color = pcaxis_col, width = axis_width, dash = "dash")) %>%
    add_trace(name = "PC3", data = pc3, x = ~xrot, y = ~yrot, z = ~zrot, mode = "lines", type = "scatter3d", inherit = FALSE, line = list(color = pcaxis_col, width = axis_width, dash = "dot")) %>%
    layout(scene = list(
      xaxis = list(title = "x"),
      yaxis = list(title = "y"),
      zaxis = list(title = "z")
    ))
  fig
}

## ----dataClouds, fig.width = 8, fig.asp = 1.0, fig.cap=paste("Figure 6. How the data (in", pcdata_colname, ") changes during PCA: (a) the original data in three dimensions; (b) the data after reducing to two dimensions; (c) the data after reducing to one dimension; (d) close up of (c) making it easier to see the individual data points. The", pcaxis_colname, "lines are the principal component axes at each step in the PCA analysis.")----
old.par <- par(mfrow = c(2, 2))
scatter3D(
  x = rotdata$xrot, y = rotdata$yrot, z = rotdata$zrot,
  pch = 19, col = pcdata_col, bty = "b2", cex = 0.2,
  xlim = c(-xa, xa), ylim = c(-xa, xa), zlim = c(-xa, xa),
  phi = 10, theta = 50, ticktype = "detailed",
  main = "(a) Original Data Cloud"
)
points3D(
  x = pc1$xrot, y = pc1$yrot, z = pc1$zrot, type = "l", col = "red",
  lwd = 0.5, lty = 1, add = TRUE
)
scatter3D(
  x = proj$xrot, y = proj$yrot, z = proj$zrot,
  pch = 19, col = pcdata_col, cex = 0.2, ticktype = "detailed",
  phi = 10, theta = 50, bty = "b2",
  xlim = c(-xa, xa), ylim = c(-xa, xa), zlim = c(-xa, xa),
  main = "(b) After Removing First PC"
)
points3D(
  x = pc1$xrot, y = pc1$yrot, z = pc1$zrot, type = "l", col = "red",
  lwd = 0.5, lty = 1, add = TRUE
)
points3D(
  x = pc2$xrot, y = pc2$yrot, z = pc2$zrot, type = "l", col = "red",
  lwd = 0.5, lty = 1, add = TRUE
)
proj2 <- rot(x = rep(0, length(id)), y = rep(0, length(id)), z = ze)
scatter3D(
  x = proj2$xrot, y = proj2$yrot, z = proj2$zrot,
  pch = 19, col = pcdata_col, cex = 0.2, ticktype = "detailed",
  phi = 10, theta = 50, bty = "b2",
  xlim = c(-xa, xa), ylim = c(-xa, xa), zlim = c(-xa, xa),
  main = "(c) After Removing Second PC"
)
points3D(
  x = pc1$xrot, y = pc1$yrot, z = pc1$zrot, type = "l", col = "red",
  lwd = 0.5, lty = 1, add = TRUE
)
points3D(
  x = pc2$xrot, y = pc2$yrot, z = pc2$zrot, type = "l", col = "red",
  lwd = 0.5, lty = 1, add = TRUE
)
points3D(
  x = pc3$xrot, y = pc3$yrot, z = pc3$zrot, type = "l", col = "red",
  lwd = 0.5, lty = 1, add = TRUE
)
scatter3D(
  x = proj2$xrot, y = proj2$yrot, z = proj2$zrot,
  pch = 19, col = pcdata_col, cex = 0.2, ticktype = "detailed",
  phi = 10, theta = 50, bty = "b2",
  xlim = c(-2, 2), ylim = c(-2, 2), zlim = c(-2, 2),
  main = "(d) Close Up of (c)"
)
par(old.par)

## ----refer-works-consulted, echo = FALSE, results = "asis"--------------------
res <- knitr::knit_child("refer_to_works_consulted.md", quiet = TRUE)
cat(res, sep = "\n")

