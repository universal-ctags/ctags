# Taken from https://github.com/kklot/eppasm/blob/1988/R/popClass.R
# EPP parameter super class: later improve the generating of fp as well
#' @importFrom methods new
eppFP <- R6::R6Class("eppfp", class=F, cloneable=F, portable=F, lock_objects=F,
    public = list(
        p = NULL,
        initialize = function(fp) {
            p <<- fp[-1]
            list2env(fp$ss, self)
        }
    )
)
