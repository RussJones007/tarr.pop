# -------------------------------------------------------------------------------------->
# Script: collapse.R
# Description:
#   Implements collapse_dim() generic + poparray method.
#   collapse_dim() groups labels within a selected dimension and sums values by group.
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: January 11, 2026
# Revised: February 16, 2026
# -------------------------------------------------------------------------------------->

#' Collapse a dimension of a poparray cube
#'
#' Groups labels along one dimension and sums population counts within groups.
#' The reduction is executed blockwise against the delayed backend and written to
#' a temporary HDF5-backed result. This avoids realizing the full source cube in
#' memory and does not persist the result as a saved package cube.
#'
#' @param x A poparray object
#' @param dim Dimension name (character) or index (integer)
#' @param groups Mapping from old labels -> new group labels.
#'   See Details.
#' @param keep_empty Logical; keep groups with zero members?
#' @param name Optional new name for the dimension (defaults to original)
#' @param strict Logical; when `TRUE` (default), unsafe grouped reductions are
#'   blocked. When `FALSE`, a warning is emitted and the collapse proceeds.
#' @param allow_overlap Logical; default `FALSE`. Set `TRUE` to explicitly allow
#'   collapsing overlapping categories within a group.
#'
#' @details
#' `groups` can be:
#' - named character vector: names are old labels, values are new labels
#' - list: names are new labels, elements are character vectors of old labels
#' - factor: length == number of old labels; levels are new labels
#'
#' Old labels not present in `groups` are dropped.
#'
#' @return A new HDF5-backed `poparray` with the chosen dimension collapsed by
#'   sum.
#' @export
setGeneric("collapse_dim", function(x,
                                    dim,
                                    groups,
                                    keep_empty = FALSE,
                                    name = NULL,
                                    strict = TRUE,
                                    allow_overlap = FALSE) {
  standardGeneric("collapse_dim")
})

collapse_dim_poparray_impl <- function(x,
                                       dim,
                                       groups,
                                       keep_empty = FALSE,
                                       name = NULL,
                                       strict = TRUE,
                                       allow_overlap = FALSE) {
  
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
    # keep declared levels even when no old label maps to them.
    all_levels <- defined_group_levels(groups)
    all_levels <- all_levels[!is.na(all_levels)]
    new_levels <- union(new_levels, all_levels)
  }
  
  # group index (1..n_new) for each kept old label
  g <- match(new_for_old_keep, new_levels)

  pa_check_collapse_semantics(
    x = x,
    dim_nm = dim_nm,
    old_labels_keep = old_labels[keep],
    new_levels = new_levels,
    group_index = g,
    strict = strict,
    allow_overlap = allow_overlap
  )
  
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
  a <- x
  nd <- length(dim(a))
  
  perm <- c(setdiff(seq_len(nd), k), k)
  invperm <- match(seq_len(nd), perm)
  
  a_perm <- DelayedArray::aperm(a, perm)
  
  # subset last dim to kept old labels
  idx <- rep(list(TRUE), nd)
  idx[[nd]] <- keep
  a_perm <- do.call(`[`, c(list(a_perm), idx, list(drop = FALSE)))
  
  # ---- 5) Blockwise reduction into a temporary HDF5-backed sink ----
  d_perm <- dim(a_perm)
  n_old_keep <- d_perm[[nd]]
  n_new <- length(new_levels)
  
  d_new_perm <- c(d_perm[-nd], n_new)
  type_in <- DelayedArray::type(a_perm)
  bytes_per <- pa_bytes_per_cell(type_in)
  blockdim_perm <- pa_collapse_blockdim(
    dim = d_perm,
    bytes_per = bytes_per,
    target_block_bytes = getOption("poparray.collapse_block_bytes", 64e6)
  )
  block_ranges <- lapply(seq_len(max(0L, nd - 1L)), function(i) {
    pa_make_block_ranges(d_perm[[i]], blockdim_perm[[i]])
  })

  sink <- HDF5Array::HDF5RealizationSink(
    dim = as.integer(d_new_perm),
    dimnames = NULL,
    type = type_in
  )

  for (block_idx in pa_iterate_block_ranges(block_ranges)) {
    src_idx <- c(block_idx, list(seq_len(n_old_keep)))
    block <- do.call(`[`, c(list(a_perm), src_idx, list(drop = FALSE)))
    block_arr <- as.array(block)
    block_dim <- dim(block_arr)
    n_row <- if (length(block_dim) == 1L) 1L else prod(block_dim[-nd])
    mat_old <- matrix(block_arr, nrow = n_row, ncol = n_old_keep)
    mat_new <- mat_old %*% M
    out_block <- array(mat_new, dim = c(block_dim[-nd], n_new))

    if (length(block_idx) == 0L) {
      starts <- integer()
      widths <- integer()
    } else {
      starts <- vapply(block_idx, function(idx) idx[[1L]], integer(1))
      widths <- vapply(block_idx, length, integer(1))
    }
    vp <- S4Arrays::ArrayViewport(
      as.integer(d_new_perm),
      IRanges::IRanges(start = c(starts, 1L), width = c(widths, n_new))
    )
    DelayedArray:::write_block(sink, vp, out_block)
  }

  arr_new_perm <- methods::as(sink, "DelayedArray")
  arr_new <- DelayedArray::aperm(arr_new_perm, invperm)
  
  # ---- 9) Update dimnames ----
  dn_new <- dn
  dn_new[[dim_nm]] <- new_levels
  if (!is.null(name) && is.character(name) && length(name) == 1L) {
    names(dn_new)[[k]] <- name
  }
  
  # Preserve time/area role metadata from the original object, adjusting for rename().
  time_dim_out <- time_role(x)
  area_dim_out <- area_role(x)
  if (!is.null(name) && is.character(name) && length(name) == 1L) {
    if (identical(time_role(x), dim_nm)) time_dim_out <- name
    if (identical(area_role(x), dim_nm)) area_dim_out <- name
  }

  dsem <- dim_semantics(x)
  if (!is.null(name) && is.character(name) && length(name) == 1L && dim_nm %in% names(dsem)) {
    names(dsem)[names(dsem) == dim_nm] <- name
    dsem[[name]] <- pa_update_dim_semantics(dsem[[name]], dim_name = name)
  }
  dsem <- dsem[names(dn_new)]
  
  # ---- 10) Wrap into a new poparray ----
  out <- new_poparray(
    x = arr_new,
    dimnames_list = dn_new,
    data_col = data_col(x),
    source = get_source(x),
    time_dim = time_dim_out,
    area_dim = area_dim_out,
    dim_semantics = dsem
  )
  
  # refresh age_iv if age.char changed (if you store it as attribute)
  if ("age.char" %in% names(dn_new) && !is.null(attr(out, "age_iv", exact = TRUE))) {
    attr(out, "age_iv") <- age_to_iv(dn_new[["age.char"]])
  }
  
  out
}  
# ---- helpers ----

normalize_groups <- function(groups, old_labels) {
  if (is.list(groups)) {
    if (is.null(names(groups)) || any(!nzchar(names(groups)))) {
      stop("If 'groups' is a list, it must be a named list of new group labels.", call. = FALSE)
    }
    olds <- unlist(groups, use.names = FALSE)
    if (anyDuplicated(olds)) {
      stop("An old label cannot be assigned to more than one output group.", call. = FALSE)
    }
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

pa_check_collapse_semantics <- function(x,
                                        dim_nm,
                                        old_labels_keep,
                                        new_levels,
                                        group_index,
                                        strict,
                                        allow_overlap) {
  if (isTRUE(allow_overlap)) {
    return(invisible(TRUE))
  }

  sem <- dim_semantics(x)[[dim_nm]]
  if (is.null(sem) || pa_is_partition(sem)) {
    return(invisible(TRUE))
  }

  unsafe_groups <- vapply(seq_along(new_levels), function(i) {
    labs <- old_labels_keep[group_index == i]
    length(labs) > 1L && pa_dim_has_overlap_risk(sem, labs)
  }, logical(1))

  if (!any(unsafe_groups)) {
    return(invisible(TRUE))
  }

  msg <- c(
    "Unsafe collapse blocked for {.cls poparray}.",
    "i" = "Dimension {.val {dim_nm}} is not known to be a safe partition for grouped reduction.",
    "i" = "Unsafe output group(s): {.val {paste(new_levels[unsafe_groups], collapse = ', ')}}.",
    "i" = "Set {.arg allow_overlap = TRUE} to bypass, or {.arg strict = FALSE} to warn and continue."
  )
  if (isTRUE(strict)) {
    cli::cli_abort(msg)
  }
  cli::cli_warn(msg)
  invisible(TRUE)
}

pa_collapse_blockdim <- function(dim, bytes_per, target_block_bytes = 64e6) {
  dim <- as.integer(dim)
  if (length(dim) == 1L) {
    return(dim)
  }

  target_cells <- max(1L, floor(as.numeric(target_block_bytes) / max(1, as.numeric(bytes_per))))
  out <- dim
  out[-length(out)] <- 1L
  if (prod(out) > target_cells) {
    stop(
      "collapse_dim(): target block size is too small to process the collapsed dimension safely. ",
      "Increase options(poparray.collapse_block_bytes = ...).",
      call. = FALSE
    )
  }

  grow_order <- seq_len(length(out) - 1L)
  grew <- TRUE
  while (isTRUE(grew)) {
    grew <- FALSE
    for (k in grow_order) {
      if (out[[k]] >= dim[[k]]) next
      cand <- out
      cand[[k]] <- min(dim[[k]], as.integer(cand[[k]] * 2L))
      if (prod(cand) <= target_cells) {
        out <- cand
        grew <- TRUE
      }
    }
  }
  out
}

pa_make_block_ranges <- function(n, block_size) {
  starts <- seq.int(1L, as.integer(n), by = max(1L, as.integer(block_size)))
  lapply(starts, function(start) {
    end <- min(as.integer(n), start + as.integer(block_size) - 1L)
    seq.int(start, end)
  })
}

pa_iterate_block_ranges <- function(block_ranges) {
  if (!length(block_ranges)) {
    return(list(list()))
  }
  idx_grid <- expand.grid(lapply(block_ranges, seq_along), KEEP.OUT.ATTRS = FALSE)
  lapply(seq_len(nrow(idx_grid)), function(i) {
    lapply(seq_along(block_ranges), function(k) {
      block_ranges[[k]][[idx_grid[[i, k]]]]
    })
  })
}


