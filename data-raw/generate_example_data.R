#Load package
library("heatmap.2x")
#Generate example data
mat <- matrix(rnorm(1000), 50, 20)
#heatmap with recommended settings
heatmap.2x(mat, scale="none", trace="none", col=bluered(50))

#create color bar
colbar <- c("red", "blue")[rep(c(1,2),(ncol(mat)/2))]
heatmap.2x(mat, scale="none", trace="none", col=bluered(50), ColSideColors=colbar)
n<- 10
colbar <- rainbow(n)[rep(c(1:n),(ncol(mat)/n))]
heatmap.2x(mat, scale="none", trace="none", col=bluered(50), ColSideColors=colbar)
These should behave just as \code{\link[gplots]{heatmap.2}} 

#create a row bar
rowbar <- c("red", "blue")[rep(c(1,2),(nrow(mat)/2))]
heatmap.2x(mat, scale="none", trace="none", col=bluered(50), RowSideColors=rowbar)
n<- 10
rowbar <- rainbow(n)[rep(c(1:n),(nrow(mat)/n))]
heatmap.2x(mat, scale="none", trace="none", col=bluered(50), RowSideColors=rowbar)
These should behave just as \code{\link[gplots]{heatmap.2}} 

#create another color bar
colbars1 <- c("red", "blue")[rep(c(1,2),(ncol(mat)/2))]
colbars2 <- c("yellow", "green", "cyan")
colbarmat <- rbind(colbars1, colbars2)
heatmap.2x(mat, scale="none", trace="none", col=bluered(50), ColSideColors=colbarmat)

#create another color bar for rows
rowbars1 <- c("red", "blue")[rep(c(1,2),(nrow(mat)/2))]
rowbars2 <- c("yellow", "green", "cyan")
rowbarmat <- cbind(rowbars1, rowbars2)
heatmap.2x(mat, scale="none", trace="none", col=bluered(50), RowSideColors=rowbarmat)
heatmap.2x(mat, scale="none", trace="none", col=bluered(50), ColSideColors=colbarmat, RowSideColors=rowbarmat)

