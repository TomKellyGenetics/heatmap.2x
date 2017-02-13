library(heatmap.2x)
context("color inputs")

mat <- matrix(rnorm(1000), 50, 20)

test_that("color accepted as function", {
  n<- 50
  heatmap.2x(mat, scale="none", trace="none", col=function(n) bluered(n))
  heatmap.2x(mat, scale="none", trace="none", col=function(n) colorpanel(n, "green", "white", "magenta"))
})

test_that("string matched to known function", {
  heatmap.2x(mat, scale="none", trace="none", col="bluered")
  heatmap.2x(mat, scale="none", trace="none", col="greenred")
  heatmap.2x(mat, scale="none", trace="none", col="heat.colors")
  heatmap.2x(mat, scale="none", trace="none", col="rainbow")
})

test_that("function call default", {
  heatmap.2x(mat, scale="none", trace="none", col=bluered)
  heatmap.2x(mat, scale="none", trace="none", col=greenred)
  heatmap.2x(mat, scale="none", trace="none", col=heat.colors)
  heatmap.2x(mat, scale="none", trace="none", col=rainbow)
})

test_that("color vector accepted", {
  heatmap.2x(mat, scale="none", trace="none", col=bluered(50))
  heatmap.2x(mat, scale="none", trace="none", col=colorpanel(50, "green", "white", "magenta"))
  heatmap.2x(mat, scale="none", trace="none", col=c("green", "white", "magenta", "red", "blue"))
})