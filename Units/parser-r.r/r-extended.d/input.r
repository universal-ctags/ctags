# From:
# https://cran.r-project.org/doc/manuals/r-release/R-intro.html#Customizing-the-environment
.First <- function() {
  options(prompt="$ ", continue="+\t")  # $ is the prompt
  options(digits=5, length=999)         # custom numbers and printout
  x11()                                 # for graphics
  par(pch = "+")                        # plotting character
  #
  # In the following function call, "mystuff.R" should be
  # captured. However, it is not easy.
  #
  # source(file.path(Sys.getenv("HOME"), "R", "mystuff.R"))
  #                                       # my personal functions
  library(MASS)                         # attach a package
  source("mystuff.R");
}
