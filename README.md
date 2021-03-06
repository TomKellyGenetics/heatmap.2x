[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/heatmap.2x)](https://cran.r-project.org/package=heatmap.2x)
[![Travis-CI Build Status](https://travis-ci.org/TomKellyGenetics/heatmap.2x.svg?branch=master)](https://travis-ci.org/TomKellyGenetics/heatmap.2x)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/TomKellyGenetics/heatmap.2x?branch=master&svg=true)](https://ci.appveyor.com/project/TomKellyGenetics/heatmap.2x)
[![Project Status: Inactive – The project has reached a stable, usable state but is no longer being actively developed; support/maintenance will be provided as time allows.](http://www.repostatus.org/badges/latest/inactive.svg)](http://www.repostatus.org/#inactive)
[![codecov](https://codecov.io/gh/TomKellyGenetics/heatmap.2x/branch/master/graph/badge.svg)](https://codecov.io/gh/TomKellyGenetics/heatmap.2x)

Enhanced Heatmap
===================
This is the `master` branch. This branch is recommended for most colour matrix annotation, additional functionality is possible in the `test` version if needed.

Altered version of heatmap.2 by Mik Black and Tom Kelly at the University of Otago

Modifications to the R function `heatmap.2 {gplots}`

Requires R packages `gplots` and `gtools`

* `heatmap.2x`: enables multiple colorbars for _ColSideColors_ with `rbind`, rownames are labels

* `heatmap.2x`: enables multiple colorbars for _RowSideColors_ with `cbind`, colnames are labels

* `master` branch recommended for most column matrix annotation. Functionality comparable to heatmap.3 https://gist.github.com/nachocab/3853004 - note: annotation matrix dimensions differ from `heatmap.3`

* `test` branch restores additional functionality for customisation of axes labels, key labels, reorder functions, extra functions, and symmetric breaks -- these have been merged into the `supr` branch. The `test` branch is recommended for near complete functionality of `heatmap.2` with additional matrix annotation. However many of these changes have not been thoroughly tested and the `test` branch should only be used if issues arise from lack of functionality of the `master` branch.

To get the modified version from github:

```R
# install.packages("devtools")
devtools::install_github("TomKellyGenetics/heatmap.2x", ref="master")
```

To get the modified version from github (with various functions of heatmap.2 restored):

```R
# install.packages("devtools")
devtools::install_github("TomKellyGenetics/heatmap.2x", ref="test")
```

To get the current released version from CRAN:

```R
install.packages("gplots")
```

Citation of original package: Gregory R. Warnes, Ben Bolker, Lodewijk Bonebakker, Robert Gentleman,
  Wolfgang Huber Andy Liaw, Thomas Lumley, Martin Maechler, Arni
  Magnusson, Steffen Moeller, Marc Schwartz and Bill Venables (2015).
  gplots: Various R Programming Tools for Plotting Data. R package
  version 2.17.0. http://CRAN.R-project.org/package=gplots


# Running

See the relevant vignettes for more details:

https://rawgit.com/TomKellyGenetics/heatmap.2x/master/vignettes/heatmap_annotation.html
https://rawgit.com/TomKellyGenetics/heatmap.2x/master/vignettes/heatmap_golub.html

heatmap.2x works in much the same manner as heatmap.2

* `heatmap.2x::heatmap.2x()` generates a heatmap, takes the same arguments as `gplots::heatmap.2()` to plot a heatmap with colour bar annotation (optional) using ColSideCols and RowSideCols vectors.

* `heatmap.2x::heatmap.2x()` also enables multiple colour bar annotation (optional) using ColSideCols and RowSideCols a matrices with ncols(x) columns or nrows(x) rows respectively. ColSideCols is an `ncols(x)*n` matrix for n color bars. RowSideCols is an `m*nrows(x)` matrix for m color bars.

* `heatmap.2x::heatmap.2x()` also takes additional arguments to specify `cexLab` and `colbarsize` to modify the size colours separately the width and labels of the colour bars respectively.

#Stable branch

Note there is a `stable` branch to try the added annotation bars with very few changes to `heatmap.2` code from `gplots`:
```R
# install.packages("devtools")
devtools::install_github("TomKellyGenetics/heatmap.2x", ref="stable")
```

For development history prior to package documentation, see the original repo: https://github.com/TomKellyGenetics/R-Heatmap-Functions/commits?author=TomKellyGenetics
