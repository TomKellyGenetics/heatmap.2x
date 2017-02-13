library(heatmap.2x)
context("color inputs")

mat <- matrix(rnorm(1000), 50, 20)

#create color bar
colbar <- rainbow(n)[rep(c(1:n),(ncol(mat)/n))]
rowbar <- c("red", "blue")[rep(c(1,2),(nrow(mat)/2))]

#These should behave just as heatmap.2

test_that("color bars as vectors", {
  heatmap.2x(mat, scale="none", trace="none", col=bluered(50), ColSideColors=colbar)
  heatmap.2x(mat, scale="none", trace="none", col=bluered(50), RowSideColors=rowbar)
})

#create another color bar
colbars1 <- c("red", "blue")[rep(c(1,2),(ncol(mat)/2))]
colbars2 <- c("yellow", "green", "cyan")
rowbars1 <- c("red", "blue")[rep(c(1,2),(nrow(mat)/2))]
rowbars2 <- c("yellow", "green", "cyan")

#create color bar matrix
colbarmat <- rbind(colbars1, colbars2)
rowbarmat <- cbind(rowbars1, rowbars2)

test_that("color bars as matrices", {
  heatmap.2x(mat, scale="none", trace="none", col=bluered(50), ColSideColors=colbarmat)
  heatmap.2x(mat, scale="none", trace="none", col=bluered(50), RowSideColors=rowbarmat)
  heatmap.2x(mat, scale="none", trace="none", col=bluered(50), ColSideColors=colbarmat, RowSideColors=rowbarmat)
})