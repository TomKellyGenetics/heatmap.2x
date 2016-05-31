#' Enhanced Heat Map (with annotation matrices)
#'
#' A heat map is a false color image (basically \code{\link[graphics]{image}}(t(x))) with a dendrogram added to the left side and/or to the top. Typically, reordering of the rows and columns according to some set of values (row or column means) within the restrictions imposed by the dendrogram is carried out. This heatmap provides a number of extensions to the standard R \code{\link[stats]{heatmap}} and enhanced \code{\link[gplots]{heatmap.2}} function.
#' @keywords heatmap visualization plot graphics
#' @imports gplots
#' @importFrom gtools invalid
#' @export
#' @examples
#' @examples
#' 
#' #Load package
#' library("heatmap.2x")
#' #Generate example data
#' mat <- matrix(rnorm(1000), 50, 20)
#' #heatmap with recommended settings
#' heatmap.2x(mat, scale="none", trace="none", col=bluered(50))
#' 
#' #create color bar
#' colbar <- c("red", "blue")[rep(c(1,2),(ncol(mat)/2))]
#' heatmap.2x(mat, scale="none", trace="none", col=bluered(50), ColSideColors=colbar)
#' n<- 10
#' colbar <- rainbow(n)[rep(c(1:n),(ncol(mat)/n))]
#' heatmap.2x(mat, scale="none", trace="none", col=bluered(50), ColSideColors=colbar)
#' These should behave just as \code{\link[gplots]{heatmap.2}} 
#' 
#' #create a row bar
#' rowbar <- c("red", "blue")[rep(c(1,2),(nrow(mat)/2))]
#' heatmap.2x(mat, scale="none", trace="none", col=bluered(50), RowSideColors=rowbar)
#' n<- 10
#' rowbar <- rainbow(n)[rep(c(1:n),(nrow(mat)/n))]
#' heatmap.2x(mat, scale="none", trace="none", col=bluered(50), RowSideColors=rowbar)
#' These should behave just as \code{\link[gplots]{heatmap.2}} 
#' 
#' #create another color bar
#' colbars1 <- c("red", "blue")[rep(c(1,2),(ncol(mat)/2))]
#' colbars2 <- c("yellow", "green", "cyan")
#' colbarmat <- rbind(colbars1, colbars2)
#' heatmap.2x(mat, scale="none", trace="none", col=bluered(50), ColSideColors=colbarmat)
#' 
#' #create another color bar for rows
#' rowbars1 <- c("red", "blue")[rep(c(1,2),(nrow(mat)/2))]
#' rowbars2 <- c("yellow", "green", "cyan")
#' rowbarmat <- cbind(rowbars1, rowbars2)
#' heatmap.2x(mat, scale="none", trace="none", col=bluered(50), RowSideColors=rowbarmat)
#' heatmap.2x(mat, scale="none", trace="none", col=bluered(50), ColSideColors=colbarmat, RowSideColors=rowbarmat)
#' 
#' heatmap.2 (x,
#' 
#' # dendrogram control
#' Rowv = TRUE,
#' Colv=if(symm)"Rowv" else TRUE,
#' distfun = dist,
#' hclustfun = hclust,
#' dendrogram = c("both","row","column","none"),
#' reorderfun = function(d, w) reorder(d, w),
#' symm = FALSE,
#' 
#' # data scaling
#' scale = c("none","row", "column"),
#' na.rm=TRUE,
#' 
#' # image plot
#' revC = identical(Colv, "Rowv"),
#' add.expr,
#' 
#' # mapping data to colors
#' breaks,
#' symbreaks=any(x < 0, na.rm=TRUE) || scale!="none",
#' 
#' # colors
#' col="heat.colors",
#' 
#' # block sepration
#' colsep,
#' rowsep,
#' sepcolor="white",
#' sepwidth=c(0.05,0.05),
#' 
#' # cell labeling
#' cellnote,
#' notecex=1.0,
#' notecol="cyan",
#' na.color=par("bg"),
#' 
#' # level trace
#' trace=c("column","row","both","none"),
#' tracecol="cyan",
#' hline=median(breaks),
#' vline=median(breaks),
#' linecol=tracecol,
#' 
#' # Row/Column Labeling
#' margins = c(5, 5),
#' ColSideColors,
#' RowSideColors,
#' cexRow = 0.2 + 1/log10(nr),
#' cexCol = 0.2 + 1/log10(nc),
#' labRow = NULL,
#' labCol = NULL,
#' srtRow = NULL,
#' srtCol = NULL,
#' adjRow = c(0,NA),
#' adjCol = c(NA,0),
#' offsetRow = 0.5,
#' offsetCol = 0.5,
#' colRow = NULL,
#' colCol = NULL,
#' 
#' # color key + density info
#' key = TRUE,
#' keysize = 1.5,
#' density.info=c("histogram","density","none"),
#' denscol=tracecol,
#' symkey = any(x < 0, na.rm=TRUE) || symbreaks,
#' densadj = 0.25,
#' key.title = NULL,
#' key.xlab = NULL,
#' key.ylab = NULL,
#' key.xtickfun = NULL,
#' key.ytickfun = NULL,
#' key.par=list(),
#' 
#' # plot labels
#' main = NULL,
#' xlab = NULL,
#' ylab = NULL,
#' 
#' # plot layout
#' lmat = NULL,
#' lhei = NULL,
#' lwid = NULL,
#' 
#' # extras
#' extrafun=NULL,
#' ...
#' )
#' 
#' @param x numeric matrix of the values to be plotted.
#' @param Rowv determines if and how the row dendrogram should be reordered. By default, it is TRUE, which implies dendrogram is computed and reordered based on row means. If NULL or FALSE, then no dendrogram is computed and no reordering is done. If a \code{\link[stats]{dendrogram}} dendrogram, then it is used "as-is", ie without any reordering. If a vector of integers, then dendrogram is computed and reordered based on the order of the vector.
#' @param Colv determines if and how the column dendrogram should be reordered. Has the options as the Rowv argument above and additionally when x is a square matrix, Colv="Rowv" means that columns should be treated identically to the rows.
#' @param distfun function used to compute the distance (dissimilarity) between both rows and columns. Defaults to \code{\link[stats]{dist}}.
#' @param hclustfun function used to compute the hierarchical clustering when Rowv or Colv are not dendrograms. Defaults to \code{\link[stats]{hclust}}.
#' @param dendrogram character string indicating whether to draw 'none', 'row', 'column' or 'both' dendrograms. Defaults to 'both'. However, if Rowv (or Colv) is FALSE or NULL and dendrogram is 'both', then a warning is issued and Rowv (or Colv) arguments are honoured.
#' @param reorderfun function(d, w) of dendrogram and weights for reordering the row and column dendrograms. The default uses \code{\link[stats]{stats-package}}stats{reorder.dendrogram}
#' @param symm logical indicating if x should be treated symmetrically; can only be true when x is a square matrix.
#' @param scale character indicating if the values should be centered and scaled in either the row direction or the column direction, or none. The default is "none".
#' @param na.rm logical indicating whether NA's should be removed.
#' @param revC logical indicating if the column order should be \code{\link[base]{rev}}ersed for plotting, such that e.g., for the symmetric case, the symmetry axis is as usual.
#' @param add.expr expression that will be evaluated after the call to image. Can be used to add components to the plot.
#' @param breaks (optional) Either a numeric vector indicating the splitting points for binning x into colors, or a integer number of break points to be used, in which case the break points will be spaced equally between min(x) and max(x).
#' @param symbreaks Boolean indicating whether breaks should be made symmetric about 0. Defaults to TRUE if the data includes negative values, and to FALSE otherwise.
#' @param col colors used for the image. Defaults to heat colors (heat.colors). Color functions (in \link[gplots] or any loaded package) taken as a string or a function (generates colours of length breaks + 1). Pre-generated vectors of colours also permitted, creating the appropriate number of breaks.
#' @param colsep,rowsep,sepcolor (optional) vector of integers indicating which columns or rows should be separated from the preceding columns or rows by a narrow space of color sepcolor.
#' @param sepwidth (optional) Vector of length 2 giving the width (colsep) or height (rowsep) the separator box drawn by colsep and rowsep as a function of the width (colsep) or height (rowsep) of a cell. Defaults to c(0.05, 0.05)
#' @param cellnote (optional) matrix of character strings which will be placed within each color cell, e.g. p-value symbols.
#' @param notecex (optional) numeric scaling factor for cellnote items.
#' @param notecol (optional) character string specifying the color for cellnote text. Defaults to "cyan".
#' @param na.color Color to use for missing value (NA). Defaults to the plot background color.
#' @param trace character string indicating whether a solid "trace" line should be drawn across 'row's or down 'column's, 'both' or 'none'. The distance of the line from the center of each color-cell is proportional to the size of the measurement. Defaults to 'column'.
#' @param tracecol character string giving the color for "trace" line. Defaults to "cyan".
#' @param hline,vline,linecol Vector of values within cells where a horizontal or vertical dotted line should be drawn. The color of the line is controlled by linecol. Horizontal lines are only plotted if trace is 'row' or 'both'. Vertical lines are only drawn if trace 'column' or 'both'. hline and vline default to the median of the breaks, linecol defaults to the value of tracecol.
#' @param margins numeric vector of length 2 containing the margins (see \code{\link[graphics]{par}}(mar= *)) for column and row names, respectively.
#' @param ColSideColors (optional) character vector of length ncol(x) containing the color names for a horizontal side bar that may be used to annotate the columns of x. Enabled multiple colorbars combined with rbind: matrix where ncol(ColSideColors)=ncol(x), nrow(ColSideColors) is the number of annotation colour bars, and rownames(ColSideColors) are labels.
#' @param RowSideColors (optional) character vector of length nrow(x) containing the color names for a vertical side bar that may be used to annotate the rows of x. Enabled multiple colorbars combined with cbind: matrix where ncol(RowSideColors)is the number of annotation colour bars, nrow(RowSideColors)=nrow(x), and colnames(RowSideColors) are labels.
#' @param cexRow,cexCol positive numbers, used as cex.axis in for the row or column axis labeling. The defaults currently only use number of rows or columns, respectively.
#' @param labRow,labCol character vectors with row and column labels to use; these default to rownames(x) or colnames(x), respectively.
#' @param cexLab positive numbers, used as cex.axis in for the row or column annotation bar labeling. Relative to size of row or column labels respectively.
#' @param srtRow,srtCol angle of row/column labels, in degrees from horizontal
#' @param adjRow,adjCol 2-element vector giving the (left-right, top-bottom) justification of row/column labels (relative to the text orientation).
#' @param offsetRow,offsetCol Number of character-width spaces to place between row/column labels and the edge of the plotting region.
#' @param colRow,colCol color of row/column labels, either a scalar to set the color of all labels the same, or a vector providing the colors of each label item
#' @param key logical indicating whether a color-key should be shown.
#' @param keysize numeric value indicating the size of the key
#' @param colbarsize numeric value indicating the size of the column bars (relative to default size in \code{\link[gplots]{heatmap.2}})
#' @param density.info character string indicating whether to superimpose a 'histogram', a 'density' plot, or no plot ('none') on the color-key.
#' @param denscol character string giving the color for the density display specified by density.info, defaults to the same value as tracecol.
#' @param symkey Boolean indicating whether the color key should be made symmetric about 0. Defaults to TRUE if the data includes negative values, and to FALSE otherwise.
#' @param densadj Numeric scaling value for tuning the kernel width when a density plot is drawn on the color key. (See the adjust parameter for the density function for details.) Defaults to 0.25.
#' @param key.title main title of the color key. If set to NA no title will be plotted.
#' @param key.xlab x axis label of the color key. If set to NA no label will be plotted.
#' @param key.ylab y axis label of the color key. If set to NA no label will be plotted.
#' @param key.xtickfun function computing tick location and labels for the xaxis of the color key. Returns a named list containing parameters that can be passed to axis. See examples.
#' @param key.ytickfun function computing tick location and labels for the y axis of the color key. Returns a named list containing parameters that can be passed to axis. See examples.
#' @param key.par graphical parameters for the color key. Named list that can be passed to par.
#' @param main,xlab,ylab main, x- and y-axis titles; defaults to none.
#' @param lmat,lhei,lwid visual layout: position matrix, column height, column width. See below for details
#' @param extrafun A function to be called after all other work. See examples.
#' @param ... additional arguments passed on to \code{\link[graphics]{image}}
heatmap.2x<-
function (x,
          Rowv = TRUE, Colv = if (symm) "Rowv" else TRUE,
          distfun = dist,
          hclustfun = hclust,
          dendrogram = c("both", "row", "column", "none"),
          reorderfun = function(d, w) reorder(d, w),
          symm = FALSE,
          scale = c("none", "row", "column"),
          na.rm = TRUE,
          revC = identical(Colv, "Rowv"),
          add.expr,
          breaks,
          symbreaks = any(x < 0, na.rm = TRUE) || scale != "none",
          col = "bluered", colsep, rowsep, sepcolor = "white", sepwidth = c(0.05, 0.05),
          cellnote, notecex = 1, notecol = "cyan",
          na.color = par("bg"),
          trace = c("column", "row", "both", "none"), tracecol = "cyan",
          hline = median(breaks), vline = median(breaks),
          linecol = tracecol,
          margins = c(5, 5),
          ColSideColors, RowSideColors,
          cexRow = 0.2 + 1/log10(nr),
          cexCol = 0.2 + 1/log10(nc),
          labRow = NULL, labCol = NULL,
          srtRow = NULL, srtCol = NULL,
          adjRow = c(0, NA), adjCol = c(NA, 0),
          cexLab = 1,
          offsetRow = 0.5, offsetCol = 0.5,
          colRow = NULL, colCol = NULL,
          key = TRUE, keysize = 1.5, colbarsize = 1,
          density.info = c("histogram", "density", "none"), denscol = tracecol,
          symkey = any(x < 0, na.rm = TRUE) || symbreaks,
          densadj = 0.25,
          key.title = NULL, key.xlab = NULL, key.ylab = NULL, key.xtickfun = NULL, key.ytickfun = NULL, key.par = list(),
          main = NULL, xlab = NULL, ylab = NULL,
          lmat = NULL, lhei = NULL, lwid = NULL, extrafun = NULL,
          ...)
{
    scale01 <- function(x, low = min(x), high = max(x)) {
        x <- (x - low)/(high - low)
        x
    }
    scale <- if (symm && missing(scale)) 
        "none"
    else match.arg(scale)
    dendrogram <- match.arg(dendrogram)
    trace <- match.arg(trace)
    density.info <- match.arg(density.info)
    if (!missing(breaks) && (scale != "none")) 
        warning("Using scale=\"row\" or scale=\"column\" when breaks are", 
            "specified can produce unpredictable results.", "Please consider using only one or the other.")
    if ((Colv == "Rowv") && (!isTRUE(Rowv) || is.null(Rowv))) 
        Colv <- FALSE
    if (length(di <- dim(x)) != 2 || !is.numeric(x)) 
        stop("`x' must be a numeric matrix")
    nr <- di[1]
    nc <- di[2]
    if (nr <= 1 || nc <= 1) 
        stop("`x' must have at least 2 rows and 2 columns")
    if (!is.numeric(margins) || length(margins) != 2) 
        stop("`margins' must be a numeric vector of length 2")
    if (missing(cellnote)) 
        cellnote <- matrix("", ncol = ncol(x), nrow = nrow(x))
    if (!inherits(Rowv, "dendrogram")) {
        if (((!isTRUE(Rowv)) || (is.null(Rowv))) && (dendrogram %in% 
            c("both", "row"))) {
            if (is.logical(Colv) && (Colv)) 
                dendrogram <- "column"
            else dendrogram <- "none"
            warning("Discrepancy: Rowv is FALSE, while dendrogram is `", 
                dendrogram, "'. Omitting row dendogram.")
        }
    }
    if (!inherits(Colv, "dendrogram")) {
        if (((!isTRUE(Colv)) || (is.null(Colv))) && (dendrogram %in% 
            c("both", "column"))) {
            if (is.logical(Rowv) && (Rowv)) 
                dendrogram <- "row"
            else dendrogram <- "none"
            warning("Discrepancy: Colv is FALSE, while dendrogram is `", 
                dendrogram, "'. Omitting column dendogram.")
        }
    }
    if (inherits(Rowv, "dendrogram")) {
        ddr <- Rowv
        rowInd <- order.dendrogram(ddr)
    }
    else if (is.integer(Rowv)) {
        hcr <- hclustfun(distfun(x))
        ddr <- as.dendrogram(hcr)
        ddr <- reorderfun(ddr, Rowv)
        rowInd <- order.dendrogram(ddr)
        if (nr != length(rowInd)) 
            stop("row dendrogram ordering gave index of wrong length")
    }
    else if (isTRUE(Rowv)) {
        Rowv <- rowMeans(x, na.rm = na.rm)
        hcr <- hclustfun(distfun(x))
        ddr <- as.dendrogram(hcr)
        ddr <- reorderfun(ddr, Rowv)
        rowInd <- order.dendrogram(ddr)
        if (nr != length(rowInd)) 
            stop("row dendrogram ordering gave index of wrong length")
    }
    else {
        rowInd <- nr:1
    }
    if (inherits(Colv, "dendrogram")) {
        ddc <- Colv
        colInd <- order.dendrogram(ddc)
    }
    else if (identical(Colv, "Rowv")) {
        if (nr != nc) 
            stop("Colv = \"Rowv\" but nrow(x) != ncol(x)")
        if (exists("ddr")) {
            ddc <- ddr
            colInd <- order.dendrogram(ddc)
        }
        else colInd <- rowInd
    }
    else if (is.integer(Colv)) {
        hcc <- hclustfun(distfun(if (symm) 
            x
        else t(x)))
        ddc <- as.dendrogram(hcc)
        ddc <- reorderfun(ddc, Colv)
        colInd <- order.dendrogram(ddc)
        if (nc != length(colInd)) 
            stop("column dendrogram ordering gave index of wrong length")
    }
    else if (isTRUE(Colv)) {
        Colv <- colMeans(x, na.rm = na.rm)
        hcc <- hclustfun(distfun(if (symm) 
            x
        else t(x)))
        ddc <- as.dendrogram(hcc)
        ddc <- reorderfun(ddc, Colv)
        colInd <- order.dendrogram(ddc)
        if (nc != length(colInd)) 
            stop("column dendrogram ordering gave index of wrong length")
    }
    else {
        colInd <- 1:nc
    }
    x <- x[rowInd, colInd]
    x.unscaled <- x
    cellnote <- cellnote[rowInd, colInd]
    if (is.null(labRow)) 
        labRow <- if (is.null(rownames(x))) 
            (1:nr)[rowInd]
        else rownames(x)
    else labRow <- labRow[rowInd]
    if (is.null(labCol)) 
        labCol <- if (is.null(colnames(x))) 
            (1:nc)[colInd]
        else colnames(x)
    else labCol <- labCol[colInd]
    if (scale == "row") {
        x <- sweep(x, 1, rowMeans(x, na.rm = na.rm))
        sx <- apply(x, 1, sd, na.rm = na.rm)
        x <- sweep(x, 1, sx, "/")
    }
    else if (scale == "column") {
        x <- sweep(x, 2, colMeans(x, na.rm = na.rm))
        sx <- apply(x, 2, sd, na.rm = na.rm)
        x <- sweep(x, 2, sx, "/")
    }
    if (missing(breaks) || is.null(breaks) || length(breaks) < 
        1) 
        if (missing(col) || length(col) == 1) 
            breaks <- 16
        else breaks <- length(col) + 1
    if (length(breaks) == 1) {
      if (!symbreaks){
        breaks <- seq(min(x, na.rm = na.rm), max(x, na.rm = na.rm), length = breaks)
      } else {
        extreme <- max(abs(x), na.rm = TRUE)
        breaks <- seq(-extreme, extreme, length = breaks)
      }
    }
    nbr <- length(breaks)
    ncol <- length(breaks) - 1
    #print(ncol)
    if (length(col) == 1){
      if (class(col) == "function"){
        #print("color accepted as function")
        colx <- function(n) col(n)
        coly <- colx(ncol)
        #} else if (class(get(col)) == "function"){
        #  print("string matched to known function")
        #  colorfunction <- get(col)
        #  coly <- colorfunction(ncol)
      } else {
        #print("function call default")
        coly<- do.call(col, list(ncol))
      }
      col <-coly
    }
    #print(coly)
    #print(length(coly))
    min.breaks <- min(breaks)
    max.breaks <- max(breaks)
    x[] <- ifelse(x < min.breaks, min.breaks, x)
    x[] <- ifelse(x > max.breaks, max.breaks, x)
    lmat <- rbind(4:3, 2:1)
    lhei <- lwid <- c(keysize, 4)
    colbarsize <-  colbarsize / 5
    if (!missing(ColSideColors)) {
        if(is.null(nrow(ColSideColors))){
          if(is.vector(ColSideColors)){
              lmat <- rbind(lmat[1, ] + 1, c(NA, 1), lmat[2, ] + 1)
              lhei <- c(lhei[1], colbarsize, lhei[2])
          } else {
              warning("Note that ColSideColors must be a vector or a Matrix")
          }
        }
        else{
          if(ncol(ColSideColors)==ncol(x)){
              lmat <- rbind(lmat[1, ] + nrow(ColSideColors),
                            t(matrix(as.numeric(unlist(strsplit(paste("NA",1:nrow(ColSideColors))," "))),2,nrow(ColSideColors))),
                            lmat[2, ] + nrow(ColSideColors))
              lhei <- c(lhei[1], rep(colbarsize,nrow(ColSideColors)), lhei[2])
          } else {
              warning("Note that if is a matrix ColSideColors it have the same number of **Columns** as the data matric being plotted")          
          }
        } 
    }
    if (!missing(RowSideColors)) {
        if(is.null(ncol(RowSideColors))){
          if(is.vector(RowSideColors)){
              lmat <- cbind(lmat[, 1] + 1, c(rep(NA, nrow(lmat) - 1), 1), lmat[, 2] + 1)
              lwid <- c(lwid[1], colbarsize, lwid[2])
          } else {
              warning("Note that RowSideColors must be a vector or a Matrix")
          }
        }
        else{
          if(nrow(RowSideColors)==nrow(x)){
              lmat<- lmat+ncol(RowSideColors)
              lmat<-cbind(lmat[,1], rbind(matrix(data=NA, (nrow(lmat)-1), ncol(RowSideColors)), 1:ncol(RowSideColors)),lmat[,2])
              lwid <- c(lwid[1], rep(colbarsize,ncol(RowSideColors)), lwid[2])
          } else {
              warning("Note that if is a matrix RowSideColors it have the same number of **Rows** as the data matric being plotted")          
          }
        } 
    }
    lmat[is.na(lmat)] <- 0
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    layout(lmat, widths = lwid, heights = lhei, respect = FALSE)
    if (!missing(RowSideColors)) {
        if(is.null(nrow(ColSideColors))) {
          par(mar = c(margins[1], 0, 0, 0.5))
          image(rbind(1:nr), col = RowSideColors[rowInd], axes = FALSE)
      }
      else{
        for(kk in 1:ncol(RowSideColors)){
          par(mar = c(margins[1], 0, 0, 0.5))
          image(rbind(1:nr), col = RowSideColors[rowInd,kk], axes = FALSE)
          axis(1, 0, labels = colnames(RowSideColors)[kk], las = 2,
               line = -0.5, tick = 0, cex.axis = cexRow)
        }
      } 
    }
    if (!missing(ColSideColors)) {
      if(is.null(nrow(ColSideColors))) {
        par(mar = c(0.5, 0, 0, margins[2]))
        image(cbind(1:nc), col = ColSideColors[colInd], axes = FALSE)
      }
      else{
        for(kk in 1:nrow(ColSideColors)){
          par(mar = c(0.5, 0, 0, margins[2]))
          image(cbind(1:nc), col = ColSideColors[kk,colInd], axes = FALSE)
          axis(4, 0, labels = rownames(ColSideColors)[kk], las = 2,
               line = -0.5, tick = 0, cex.axis = cexCol)
        }
      }
    }
    par(mar = c(margins[1], 0, 0, margins[2]))
    if (!symm || scale != "none") {
        x <- t(x)
        cellnote <- t(cellnote)
    }
    if (revC) {
        iy <- nr:1
        ddr <- rev(ddr)
        x <- x[, iy]
        cellnote <- cellnote[, iy]
    }
    else iy <- 1:nr
    image(1:nc, 1:nr, x, xlim = 0.5 + c(0, nc), ylim = 0.5 + 
        c(0, nr), axes = FALSE, xlab = "", ylab = "", col = col, 
        breaks = breaks, ...)
    if (!invalid(na.color) & any(is.na(x))) {
        mmat <- ifelse(is.na(x), 1, NA)
        image(1:nc, 1:nr, mmat, axes = FALSE, xlab = "", ylab = "", 
            col = na.color, add = TRUE)
    }
    if (is.null(srtCol) && is.null(colCol)) 
        axis(1, 1:nc, labels = labCol, las = 2, line = -0.5 + 
           offsetCol, tick = 0, cex.axis = cexCol, hadj = adjCol[1], 
         padj = adjCol[2])
      else {
        if (is.null(srtCol) || is.numeric(srtCol)) {
          if (missing(adjCol) || is.null(adjCol)) 
        adjCol = c(1, NA)
          xpd.orig <- par("xpd")
          par(xpd = NA)
          xpos <- axis(1, 1:nc, labels = rep("", nc), las = 2, 
                   tick = 0)
          text(x = xpos, y = par("usr")[3] - (1 + offsetCol) * 
             strheight("M"), labels = labCol, adj = adjCol, 
           cex = cexCol, srt = srtCol, col = colCol)
          print(colCol)
          par(xpd = xpd.orig)
        }
        else warning("Invalid value for srtCol ignored.")
      }
      if (is.null(srtRow) && is.null(colRow)) {
        axis(4, iy, labels = labRow, las = 2, line = -0.5 + offsetRow, 
         tick = 0, cex.axis = cexRow, hadj = adjRow[1], padj = adjRow[2])
      }
      else {
        if (is.null(srtRow) || is.numeric(srtRow)) {
          xpd.orig <- par("xpd")
          par(xpd = NA)
          ypos <- axis(4, iy, labels = rep("", nr), las = 2, 
                   line = -0.5, tick = 0)
          text(x = par("usr")[2] + (1 + offsetRow) * strwidth("M"), 
           y = ypos, labels = labRow, adj = adjRow, cex = cexRow, 
           srt = srtRow, col = colRow)
          par(xpd = xpd.orig)
        }
        else warning("Invalid value for srtRow ignored.")
      }
    if (!is.null(xlab)) 
        mtext(xlab, side = 1, line = margins[1] - 1.25, cex = cexLab)
    if (!is.null(ylab)) 
        mtext(ylab, side = 4, line = margins[2] - 1.25, cex = cexLab)
    if (!missing(add.expr)) 
        eval(substitute(add.expr))
    if (!missing(colsep)) 
        for (csep in colsep) rect(xleft = csep + 0.5, ybottom = rep(0, 
            length(csep)), xright = csep + 0.5 + sepwidth[1], 
            ytop = rep(ncol(x) + 1, csep), lty = 1, lwd = 1, 
            col = sepcolor, border = sepcolor)
    if (!missing(rowsep)) 
        for (rsep in rowsep) rect(xleft = 0, ybottom = (ncol(x) + 
            1 - rsep) - 0.5, xright = ncol(x) + 1, ytop = (ncol(x) + 
            1 - rsep) - 0.5 - sepwidth[2], lty = 1, lwd = 1, 
            col = sepcolor, border = sepcolor)
    min.scale <- min(breaks)
    max.scale <- max(breaks)
    x.scaled <- scale01(t(x), min.scale, max.scale)
    if (trace %in% c("both", "column")) {
        for (i in colInd) {
            if (!is.null(vline)) {
                vline.vals <- scale01(vline, min.scale, max.scale)
                abline(v = i - 0.5 + vline.vals, col = linecol, 
                  lty = 2)
            }
            xv <- rep(i, nrow(x.scaled)) + x.scaled[, i] - 0.5
            xv <- c(xv[1], xv)
            yv <- 1:length(xv) - 0.5
            lines(x = xv, y = yv, lwd = 1, col = tracecol, type = "s")
        }
    }
    if (trace %in% c("both", "row")) {
        for (i in rowInd) {
            if (!is.null(hline)) {
                hline.vals <- scale01(hline, min.scale, max.scale)
                abline(h = i + hline, col = linecol, lty = 2)
            }
            yv <- rep(i, ncol(x.scaled)) + x.scaled[i, ] - 0.5
            yv <- rev(c(yv[1], yv))
            xv <- length(yv):1 - 0.5
            lines(x = xv, y = yv, lwd = 1, col = tracecol, type = "s")
        }
    }
    if (!missing(cellnote)) 
        text(x = c(row(cellnote)), y = c(col(cellnote)), labels = c(cellnote), 
            col = notecol, cex = notecex)
    par(mar = c(margins[1], 0, 0, 0))
    if (dendrogram %in% c("both", "row")) {
        plot(ddr, horiz = TRUE, axes = FALSE, yaxs = "i", leaflab = "none")
    }
    else plot.new()
    par(mar = c(0, 0, if (!is.null(main)) 5 else 0, margins[2]))
    if (dendrogram %in% c("both", "column")) {
        plot(ddc, axes = FALSE, xaxs = "i", leaflab = "none")
    }
    else plot.new()
    if (!is.null(main)) 
        title(main, cex.main = 1.5 * op[["cex.main"]])
    if (key) {
    mar <- c(5, 4, 2, 1)
    if (!is.null(key.xlab) && is.na(key.xlab)) 
      mar[1] <- 2
    if (!is.null(key.ylab) && is.na(key.ylab)) 
      mar[2] <- 2
    if (!is.null(key.title) && is.na(key.title)) 
      mar[3] <- 1
    par(mar = mar, cex = 0.75, mgp = c(2, 1, 0))
    if (length(key.par) > 0) 
      do.call(par, key.par)
    tmpbreaks <- breaks
    if (symkey) {
      max.raw <- max(abs(c(x, breaks)), na.rm = TRUE)
      min.raw <- -max.raw
      tmpbreaks[1] <- -max(abs(x), na.rm = TRUE)
      tmpbreaks[length(tmpbreaks)] <- max(abs(x), na.rm = TRUE)
    }
    else {
      min.raw <- min.breaks
      max.raw <- max.breaks
    }
    z <- seq(min.raw, max.raw, by = min(diff(breaks)/4))
    image(z = matrix(z, ncol = 1), col = col, breaks = tmpbreaks, 
          xaxt = "n", yaxt = "n")
    par(usr = c(0, 1, 0, 1))
    if (is.null(key.xtickfun)) {
      lv <- pretty(breaks)
      xv <- scale01(as.numeric(lv), min.raw, max.raw)
      xargs <- list(at = xv, labels = lv)
    }
    else {
      xargs <- key.xtickfun()
    }
    xargs$side <- 1
    do.call(axis, xargs)
    if (is.null(key.xlab)) {
      if (scale == "row") 
        key.xlab <- "Row Z-Score"
      else if (scale == "column") 
        key.xlab <- "Column Z-Score"
      else key.xlab <- "Value"
    }
    if (!is.na(key.xlab)) {
      mtext(side = 1, key.xlab, line = par("mgp")[1], padj = 0.5, 
            cex = par("cex") * par("cex.lab"))
    }
    if (density.info == "density") {
      dens <- density(x, adjust = densadj, na.rm = TRUE, 
                      from = min.scale, to = max.scale)
      omit <- dens$x < min(breaks) | dens$x > max(breaks)
      dens$x <- dens$x[!omit]
      dens$y <- dens$y[!omit]
      dens$x <- scale01(dens$x, min.raw, max.raw)
      lines(dens$x, dens$y/max(dens$y) * 0.95, col = denscol, 
            lwd = 1)
      if (is.null(key.ytickfun)) {
        yargs <- list(at = pretty(dens$y)/max(dens$y) * 
                        0.95, labels = pretty(dens$y))
      }
      else {
        yargs <- key.ytickfun()
      }
      yargs$side <- 2
      do.call(axis, yargs)
      if (is.null(key.title)) 
        key.title <- "Color Key\nand Density Plot"
      if (!is.na(key.title)) 
        title(key.title)
      par(cex = 0.5)
      if (is.null(key.ylab)) 
        key.ylab <- "Density"
      if (!is.na(key.ylab)) 
        mtext(side = 2, key.ylab, line = par("mgp")[1], 
              padj = 0.5, cex = par("cex") * par("cex.lab"))
    }
    else if (density.info == "histogram") {
      h <- hist(x, plot = FALSE, breaks = breaks)
      hx <- scale01(breaks, min.raw, max.raw)
      hy <- c(h$counts, h$counts[length(h$counts)])
      lines(hx, hy/max(hy) * 0.95, lwd = 1, type = "s", 
            col = denscol)
      if (is.null(key.ytickfun)) {
        yargs <- list(at = pretty(hy)/max(hy) * 0.95, 
                      labels = pretty(hy))
      }
      else {
        yargs <- key.ytickfun()
      }
      yargs$side <- 2
      do.call(axis, yargs)
      if (is.null(key.title)) 
        key.title <- "Color Key\nand Histogram"
      if (!is.na(key.title)) 
        title(key.title)
      par(cex = 0.5)
      if (is.null(key.ylab)) 
        key.ylab <- "Count"
      if (!is.na(key.ylab)) 
        mtext(side = 2, key.ylab, line = par("mgp")[1], 
              padj = 0.5, cex = par("cex") * par("cex.lab"))
    }
    else if (is.null(key.title)) 
      title("Color Key")
    if (trace %in% c("both", "column")) {
      vline.vals <- scale01(vline, min.raw, max.raw)
      if (!is.null(vline)) {
        abline(v = vline.vals, col = linecol, lty = 2)
      }
    }
    if (trace %in% c("both", "row")) {
      hline.vals <- scale01(hline, min.raw, max.raw)
      if (!is.null(hline)) {
        abline(v = hline.vals, col = linecol, lty = 2)
      }
    }
  }
  else plot.new()
    invisible(list(rowInd = rowInd, colInd = colInd))
      if (!is.null(extrafun)){ 
        extrafun()
        }
}
