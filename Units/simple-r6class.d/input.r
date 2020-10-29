# Taken from https://www.rdocumentation.org/packages/R6/versions/2.4.1/topics/R6Class

library(R6)

Queue <- R6Class("Queue",
  public = list(
    initialize = function(...) {
      for (item in list(...)) {
        self$add(item)
      }
    },
    add = function(x) {
      private$queue <- c(private$queue, list(x))
      invisible(self)
    },
    remove = function() {
      if (private$length() == 0) return(NULL)
      # Can use private$queue for explicit access
      head <- private$queue[[1]]
      private$queue <- private$queue[-1]
      head
    }
  ),
  private = list(
    queue = list(),
    length = function() base::length(private$queue)
  )
)

# Active bindings -------------------------------------------------
Numbers <- R6Class("Numbers",
  public = list(
    x = 100
  ),
  active = list(
    x2 = function(value) {
      if (missing(value)) return(self$x * 2)
      else self$x <- value/2
    },
    rand = function() rnorm(1)
  )
)

HistoryQueue <- R6Class("HistoryQueue",
  inherit = Queue,
  public = list(
    show = function() {
      cat("Next item is at index", private$head_idx + 1, "\n")
      for (i in seq_along(private$queue)) {
        cat(i, ": ", private$queue[[i]], "\n", sep = "")
      }
    },
    remove = function() {
      if (private$length() - private$head_idx == 0) return(NULL)
      private$head_idx <<- private$head_idx + 1
      # For testing extras: field
      local_noise <- 1
      private$queue[[private$head_idx]]
    }
  ),
  private = list(
    head_idx = 0
  )
)
