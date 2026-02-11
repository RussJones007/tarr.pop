# -------------------------------------------------------------------------------------->
# Script: collapse.R
# Description:
#   Implrments the collapse_dim() function.  THe collapse_dim() function is introcued as a generic, the a method is 
#   given to implement for class poparray  - collapse_dim_tarr.pop().  This function was inspired by the original 
#   group_ages() function to collage ages into large groups.  
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: January 11, 2026
# Revised:
# -------------------------------------------------------------------------------------->

#' Collapse a dimension of a poparray cube
#'
#' Groups labels along one dimension and sums population counts within groups.
#' Designed to stay lazy with DelayedArray/HDF5Array backends.
#'
#' @param x A poparray object
#' @param dim Dimension name (character) or index (integer)
#' @param groups Mapping from old labels -> new group labels.
#'   See Details.
#' @param keep_empty Logical; keep groups with zero members?
#' @param name Optional new name for the dimension (defaults to original)
#'
#' @details
#' `groups` can be:
#' - named character vector: names are old labels, values are new labels
#' - list: names are new labels, elements are character vectors of old labels
#' - factor: length == number of old labels; levels are new labels
#'
#' Old labels not present in `groups` are dropped.
#'
#' @return A new poparray with the chosen dimension collapsed by sum.
#' @export
collapse_dim <- function(x, dim, groups, keep_empty = FALSE, name = NULL) {
  UseMethod("collapse_dim")
}

#' @export
collapse_dim.poparray <- function(x, groups, dim, keep_empty = FALSE, name = NULL) {
  
  # ---- 1) Resolve dimension + labels ----
  dn <- dimnames(x)
  dim_names <- names(dn)
  
  k <- if (is.character(dim)) match(dim, dim_names) else as.integer(dim)
  if (is.na(k) || k < 1L || k > length(dim_names)) {
    stop("collapse_dim(): unknown dim '", dim, "'.")
  }
  dim_nm <- dim_names[[k]]
  old_labels <- dn[[dim_nm]]
  
  # ---- 2) Normalize groups -> mapping old -> new ----
  # normalize_groups() should return a character vector same length as old_labels:
  # each element is the new group name, or NA if unmapped.
  map_old_to_new <- normalize_groups(groups, old_labels)
  
  unmapped <- is.na(map_old_to_new)
  if (any(unmapped)) {
    dropped <- old_labels[unmapped]
    preview <- utils::head(dropped, 20L)
    warning(
      "collapse_dim(): dropping ", length(dropped),
      " unmapped label(s) in dim '", dim_nm, "'. Examples: ",
      paste(preview, collapse = ", "),
      if (length(dropped) > 20L) " ..." else "",
      call. = FALSE
    )
    
    rm(dropped, preview)
  }
  
  keep <- !unmapped
  if (!any(keep)) {
    stop("collapse_dim(): no labels mapped for dim '", dim_nm, "'.")
  }
  
  
  new_for_old_keep <- map_old_to_new[keep]
  
  # stable new levels (in appearance order)
  new_levels <- unique(new_for_old_keep)
  if (keep_empty) {
    # If you have a helper to get all defined group names, use it;
    # otherwise just ensure names(groups) included.
    new_levels <- union(new_levels, names(groups))
  }
  
  # group index (1..n_new) for each kept old label
  g <- match(new_for_old_keep, new_levels)
  
  # first cleanup
  rm(dim_names, old_labels, map_old_to_new, unmapped, new_for_old_keep)
  
  # ---- 3) Build mapping matrix M (old_keep -> new) ----
  M <- Matrix::sparseMatrix(
    i = seq_along(g),
    j = g,
    x = 1,
    dims = c(length(g), length(new_levels))
  )
  
  # ---- 4) Permute so target dim is last, and subset to kept labels ----
  a <- x$handle
  nd <- length(dim(a))
  
  perm <- c(setdiff(seq_len(nd), k), k)
  invperm <- match(seq_len(nd), perm)
  
  a_perm <- DelayedArray::aperm(a, perm)
  
  # subset last dim to kept old labels
  idx <- rep(list(TRUE), nd)
  idx[[nd]] <- keep
  a_perm <- do.call(`[`, c(list(a_perm), idx, list(drop = FALSE)))
  
  # ---- 5) Size guardrail before realizing into RAM ----
  d_perm <- dim(a_perm)
  n_old_keep <- d_perm[[nd]]
  n_new <- length(new_levels)
  
  # output dims in permuted order
  d_new_perm <- c(d_perm[-nd], n_new)
  
  # estimate bytes
  type_in <- DelayedArray::type(a_perm)
  bytes_per <- switch(type_in, integer = 4, double = 8, logical = 4, raw = 1, 8)
  est_bytes <- prod(d_perm) * bytes_per   # working set we will realize (a_perm)
  max_bytes <- getOption("poparray.max_bytes", 2e9)  # default ~2GB
  
  if (is.finite(est_bytes) && est_bytes > max_bytes) {
    stop(
      "collapse_dim(): would need to realize ~", format(est_bytes, scientific = TRUE),
      " bytes into memory for this collapse.\n",
      "Either: (1) filter/slice first to reduce size, or (2) implement disk-backed collapse.\n",
      "You can raise the limit via options(poparray.max_bytes=...)."
    )
  }
  
  rm(n_new, type_in, bytes_per, est_bytes, max_bytes)
  
  # ---- 6) Realize the permuted+trimmed array into memory (base array) ----
  # This reads from disk (blockwise internally) but returns a standard R array.
  arr_perm <- as.array(a_perm)
  
  # ---- 7) Collapse via sparse matrix multiply (in memory) ----
  # fold to 2D: (prod(other dims) x n_old_keep)
  n_row <- prod(d_perm[-nd])
  mat_old <- matrix(arr_perm, nrow = n_row, ncol = n_old_keep)
  
  mat_new <- mat_old %*% M  # (n_row x n_new), returns ordinary matrix
  
  # unfold back to N-D in permuted order
  arr_new_perm <- array(mat_new, dim = d_new_perm)
  
  # ---- 8) Invert permutation back to original order ----
  arr_new <- aperm(arr_new_perm, invperm)
  
  # ---- 9) Update dimnames ----
  dn_new <- dn
  dn_new[[dim_nm]] <- new_levels
  if (!is.null(name) && is.character(name) && length(name) == 1L) {
    names(dn_new)[[k]] <- name
  }
  
  # ---- 10) Wrap into a new poparray ----
  out <- new_poparray(
    x = arr_new,
    dimnames_list = dn_new,
    data_col = data_col(x),
    source = get_source(x)
  )
  
  # refresh age_iv if age.char changed (if you store it as attribute)
  if (identical(dim_nm, "age.char") && !is.null(attr(out, "age_iv", exact = TRUE))) {
    attr(out, "age_iv") <- age_to_iv(dn_new[["age.char"]])
  }
  
  out
}  
# ---- helpers ----

normalize_groups <- function(groups, old_labels) {
  if (is.list(groups)) {
    # list("0-4" = c("0","1","2","3","4"), ...)
    new <- rep(NA_character_, length(old_labels))
    for (nm in names(groups)) {
      hits <- old_labels %in% groups[[nm]]
      new[hits] <- nm
    }
    return(stats::setNames(new, old_labels))
  }
  
  if (is.factor(groups)) {
    if (length(groups) != length(old_labels)) {
      stop("If 'groups' is a factor, it must be same length as old labels.")
    }
    return(stats::setNames(as.character(groups), old_labels))
  }
  
  if (is.character(groups)) {
    if (is.null(names(groups))) {
      stop("If 'groups' is character, it must be a *named* vector old->new.")
    }
    new <- rep(NA_character_, length(old_labels))
    m <- match(old_labels, names(groups))
    new[!is.na(m)] <- unname(groups[m[!is.na(m)]])
    return(stats::setNames(new, old_labels))
  }
  
  stop("Unsupported 'groups' type: ", paste(class(groups), collapse = ", "))
}

defined_group_levels <- function(groups) {
  if (is.list(groups)) return(names(groups))
  if (is.factor(groups)) return(levels(groups))
  if (is.character(groups)) return(unique(unname(groups)))
  character()
}


