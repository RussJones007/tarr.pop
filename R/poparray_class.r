# -------------------------------------------------------------------------------------->
# Script: poparray_classr
# Description:
#   Constrcotr and methods for generics for the poparray class.  This is a refactoring of the older tarr_pop class
#   with more specific contracted behavior enforing a time and area dimesions allowing for other dimseions
#   to be added to an array as options. 
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: `Feb 3, 2026
# Revised:
# -------------------------------------------------------------------------------------->

#' @export
new_poparray <- function(x,
                         dimnames_list = dimnames(x),
                         data_col = "population",
                         source = NULL,
                         time_dim = NULL,
                         area_dim = "area.name",
                         ...) {
  
  # ---- Laziness: ensure DelayedArray backend ----
  if (!inherits(x, "DelayedArray")) {
    x <- DelayedArray::DelayedArray(x)
  }
  
  if (is.null(dimnames_list) || !is.list(dimnames_list)) {
    stop("`dimnames_list` must be a named list of dimension labels.",
         call. = FALSE)
  }
  
  nms <- names(dimnames_list)
  
  # ---- Resolve time dimension ----
  if (is.null(time_dim)) {
    # Default heuristic: common time names
    time_candidates <- intersect(
      nms,
      c("year", "date", "time", "month")
    )
    if (length(time_candidates) == 0) {
      stop("No time dimension found. Supply `time_dim` explicitly.",
           call. = FALSE)
    }
    time_dim <- time_candidates[[1]]
  }
  
  # ---- Validate required roles ----
  required_roles <- c(time_dim, area_dim)
  missing_roles <- setdiff(required_roles, nms)
  
  if (length(missing_roles) > 0) {
    stop(
      "poparray requires time and area dimensions. Missing: ",
      paste(missing_roles, collapse = ", "),
      call. = FALSE
    )
  }
  
  # ---- Age semantics (optional) ----
  age_iv <- NULL
  if ("age.char" %in% nms) {
    age_iv <- age_to_iv(dimnames_list[["age.char"]])
  }
  
  # ---- Construct object ----
  obj <- list(
    handle = x,
    dimn   = dimnames_list
  )
  
  attr(obj, "data_col") <- data_col
  attr(obj, "time_dim") <- time_dim
  attr(obj, "area_dim") <- area_dim
  
  if (!is.null(age_iv)) attr(obj, "age_iv") <- age_iv
  if (!is.null(source)) attr(obj, "source") <- source
  
  class(obj) <- "poparray"
  obj
}


#' export
is.poparray <- function(x) inherits(x, "poparray")

#' export
is.tarr_pop <- function(x) inherits(x, c("tarr_pop", "poparray"))


# Indexing operator ---------------------------
# poparray method for the index `[` operator
# If drop = TRUE and resulting object no longer has all dims, If the time nd area dimesions are still present, 
# the the returned array is the subsetted poparray, otherwise a subsetted DelayArray.
#' @exportS3Method "[", poparray
`[.poparray` <- function(x, ..., drop = FALSE) {
  dim_names   <- names(x)
  nd  <- length(dim(x))
  ndx <- rep(list(TRUE),nd)  # same length as dimension in x
  
  # handle the dots argument and just in case soemthing is missing
  dots <- as.list(substitute(list(...)))[-1L]
  idx <- lapply(dots, \(e) {if(is_missing_arg(e)) TRUE else eval(e, parent.frame())})
  
  # Set up ndx using dimension name or position
  index_names <- names(idx)
  if(! is.null(index_names)) {
    positions <- match(index_names, dim_names)
    ndx[positions] <- idx
  } else {
    ndx[ seq_len(length(idx)) ] <- idx
  }
  
  #idx <- idx[seq_len(nd)]  # only keep the dims present
  
  h_sub <- do.call(`[`, c(list(x$handle), ndx, list(drop = drop)))
  
  if (drop && length(dim(h_sub)) < nd) return(h_sub)
  
  # rebuilding the dimnamnes attribute 
  dn <- x$dimn
  dim_names <- names(dn)
  
  for (kk in seq_len(nd)) {
    sel <- idx[[kk]]
    if (is.null(sel)) next
    nm <- dim_names[kk]
    this <- dn[[nm]]
    
    if (is.numeric(sel) || is.logical(sel)) {
      dn[[nm]] <- this[sel]
    } else {
      dn[[nm]] <- this[this %in% sel]
    }
  }
  
  new_poparray(
    x             = h_sub,
    dimnames_list = dimnames(h_sub),
    data_col      = data_col(x),
    source        = get_source(x),
    age_iv        = attr(x, "age_iv")
  )
}