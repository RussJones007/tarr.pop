# -------------------------------------------------------------------------------------->
# Script: labels_registry.R
# Description:
#  Creates a registry of labels and their values from the list returned by build_labels()
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: January 4, 2026
# Revised:
# -------------------------------------------------------------------------------------->

flatten_named_list <- function(x, path = character()) {
  stopifnot(is.list(x))
  
  out <- list()
  
  for (nm in names(x)) {
    val <- x[[nm]]
    
    if (is.list(val) && !is.data.frame(val)) {
      sub <- flatten_named_list(val, c(path, nm))
      
      # collision detection
      dup <- intersect(names(out), names(sub))
      if (length(dup)) {
        stop(
          "Duplicate label names after flattening: ",
          paste(dup, collapse = ", "),
          call. = FALSE
        )
      }
      
      out <- c(out, sub)
    } else {
      if (nm %in% names(out)) {
        stop(
          "Duplicate label name: ", nm,
          call. = FALSE
        )
      }
      out[[nm]] <- val
    }
  }
  
  out
}



# Hidden registry for label objects during builds
.labels_env <- new.env(parent = emptyenv())

labels_init <- function(x, envir = .labels_env) {
  stopifnot(is.list(x), !is.null(names(x)), all(nzchar(names(x))))
  
  # clear existing keys to avoid stale labels
  rm(list = ls(envir = envir, all.names = TRUE), envir = envir)
  flat <- flatten_named_list(x)
  # load new labels
  list2env(flat, envir = envir)
  invisible(envir)
}

labels_registry_env <- function() .labels_env
