# ------------------------------------------------------------------------------------------------------------------->
# Script: filter.R
# Description:
#   Implements the filter() method for class poparray 
# 
# 
# ------------------------------------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: January 5, 2026
# Revised: February 9, 2026 to use poarray instead of tarr_pop
# ------------------------------------------------------------------------------------------------------------------->


#' Filter a `poparray` array/cube by restricting dimension labels 
#'
#' @description `filter.poparray()` restricts a `poparray` object along one or more named dimensions using **label-based
#' predicates**, returning a delayed `poparray` (or view) without materializing the data.
#'
#' This is **not** row filtering. It is **dimension slicing** intended for
#' HDF5/DelayedArray-backed population cubes.
#'
#' @details ## What this method does `filter()` for `poparray` builds or updates a subset plan that restricts dimension
#' labels (e.g., years, counties, ages). The returned object remains delayed; realization happens later via `collect()`,
#' `as.data.frame()`, `as_tibble()`, or similar.
#'
#' ## Supported predicate forms Each filter expression must reference **exactly one** dimension name on the left-hand
#' side.
#'
#' **Categorical dimensions** (e.g. `area.name`, `sex`, `race`, `ethnicity`)
#' support:
#' - Equality: `dim == value`
#' - Membership: `dim %in% values`
#' - Within-dimension boolean combinations: `&` and `|` are allowed only when
#' both sides constrain the *same* dimension.
#'
#' **Ordered dimensions** (e.g. `year`, `age.char`) additionally support:
#' - Comparisons: `<`, `<=`, `>`, `>=`
#' - Ranges: `%between%` (if provided by the package)
#'
#' ## Age interval semantics (`age.char`) Range filtering on `age.char` operates on **age intervals**. Labels are
#' interpreted as half-open intervals, e.g.:
#' - `"37"`  → `[37, 38)`
#' - `"5-9"` → `[5, 10)`
#' - `"85+"` → `[85, Inf)`
#'
#' Range predicates (comparisons and `%between%`) select age labels whose intervals **overlap** the requested age set.
#' Discrete membership with `%in%` remains **exact label matching** (no interval overlap).
#'
#' ## What is not supported
#' - **Named "short form" arguments** such as `filter(x, year = 2020)` are not
#' supported and will warn and then error. Use `year == 2020` or `year %in% 2015:2020`.
#' - Predicates that do not map to a single dimension (e.g. `value > 0`,
#' `year + 1 == 2021`, `paste(area.name) == "Tarrant"`).
#' - Cross-dimension boolean logic within a single expression
#' (e.g. `year == 2020 | area.name == "Tarrant"`).
#'
#' ## Unknown labels and strictness 
#' By default this method uses **strict** label matching for categorical dimensions. If
#' requested labels are not present, an error is thrown (often with suggested close matches). For ordered dimensions,
#' errors are raised if coercion to the comparison scale fails (e.g., non-numeric year labels).
#'
#' @param .data A `poparray` object.
#' @param ... One or more filtering expressions. Each expression must constrain a single dimension name using supported
#'   operators (see Details).
#' @param .strict Logical. If `TRUE` (default), unknown categorical labels are an error. If `FALSE`, unknown categorical
#'   labels are dropped with a warning.
#'
#' @return A delayed `poparray` (or `tarr_pop_view`) with updated dimension restrictions. No materialization occurs.
#' 
#' @exportS3Method filter poparray
filter.poparray <- function(.data, ..., .strict = TRUE) {
  quos <- rlang::enquos(...)
  
  # Reject named "short form" args: filter(x, year = 2020)
  qnms <- names(quos)
  if (length(quos) && any(nzchar(qnms))) {
    warning(
      "filter.poparray() does not support named arguments like year = ...; ",
      "use predicates like year == ... or year %in% ....",
      call. = FALSE
    )
    stop("Named arguments are not supported for filter.poparray().", call. = FALSE)
  }
  
  if (!length(quos)) {
    return(.data)
  }
  
  # Parse all predicates, then apply by dimension (intersection)
  preds <- lapply(quos, tp_parse_predicate, env = rlang::caller_env())
  dims  <- vapply(preds, `[[`, character(1), "dim")
  
  dn <- dimnames(.data)
  dim_order <- names(dn)
  idx <- rep(list(TRUE), length(dim_order))
  names(idx) <- dim_order
  
  for (d in unique(dims)) {
    d_preds <- preds[dims == d]
    idx[[d]] <- tp_eval_dim_preds(.data, dim_name = d, preds = d_preds, .strict = .strict)
  }
  
  
  # Apply lazy subsetting using the existing `[.poparray` method.
  # Do not pass dimension names as argument names to `[.poparray`.
  do.call(`[`, c(list(.data), unname(idx), list(drop = FALSE)))
}

# Internal helpers -----------------------------------------------------------------------------

tp_dim_type <- function(dim_name) {
  if (dim_name %in% c("year", "age.char")) "ordered" else "categorical"
}

tp_allowed_ops <- function(dim_type) {
  if (identical(dim_type, "ordered")) {
    c("==", "%in%", "<", "<=", ">", ">=", "%between%")
  } else {
    c("==", "%in%")
  }
}

tp_parse_predicate <- function(quo, env) {
  expr <- rlang::get_expr(quo)
  qenv <- rlang::get_env(quo)
  tp_parse_expr(expr, env = qenv %||% env)
}

tp_parse_expr <- function(expr, env) {
  # Allow (a & b) / (a | b) within-dimension only
  if (rlang::is_call(expr, "&") || rlang::is_call(expr, "|")) {
    op <- as.character(expr[[1]])
    lhs <- tp_parse_expr(expr[[2]], env = env)
    rhs <- tp_parse_expr(expr[[3]], env = env)
    
    if (!identical(lhs$dim, rhs$dim)) {
      stop(
        "Each filter expression must reference exactly one dimension. ",
        "Use multiple filter() arguments for AND across dimensions.",
        call. = FALSE
      )
    }
    lhs$combine <- op
    lhs$rhs_pred <- rhs
    return(lhs)
  }
  
  # Binary ops: ==, %in%, <, <=, >, >=, %between%
  if (!rlang::is_call(expr)) {
    stop(
      "filter.poparray() predicates must be calls like dim == value or dim %in% values.",
      call. = FALSE
    )
  }
  
  op <- as.character(expr[[1]])
  if (!op %in% c("==", "%in%", "<", "<=", ">", ">=", "%between%")) {
    stop(
      "Unsupported operator in filter.poparray(): ", op, ". ",
      "Supported operators include == and %in% (plus comparisons/ranges for ordered dims).",
      call. = FALSE
    )
  }
  
  # LHS must be a bare symbol naming a dimension
  lhs <- expr[[2]]
  if (!rlang::is_symbol(lhs)) {
    stop(
      "Left-hand side of each predicate must be a bare dimension name (e.g., year == 2020).",
      call. = FALSE
    )
  }
  dim_name <- rlang::as_name(lhs)
  
  # RHS must evaluate to an atomic vector
  rhs_expr <- expr[[3]]
  rhs_val <- rlang::eval_tidy(rhs_expr, data = NULL, env = env)
  if (!is.atomic(rhs_val)) {
    stop(
      "Right-hand side of predicates must be an atomic vector of labels/values.",
      call. = FALSE
    )
  }
  
  list(
    dim = dim_name,
    op  = op,
    rhs = rhs_val,
    env = env
  )
}

tp_eval_dim_preds <- function(x, dim_name, preds, .strict = TRUE) {
  dn <- dimnames(x)
  if (!dim_name %in% names(dn)) {
    stop("Unknown dimension: '", dim_name, "'.", call. = FALSE)
  }
  
  dim_type <- tp_dim_type(dim_name)
  allowed <- tp_allowed_ops(dim_type)
  
  # Validate ops for this dimension type
  ops <- vapply(preds, `[[`, character(1), "op")
  bad_ops <- setdiff(unique(ops), allowed)
  if (length(bad_ops)) {
    stop(
      "Unsupported operator(s) for dimension '", dim_name, "': ",
      paste(bad_ops, collapse = ", "), ".",
      call. = FALSE
    )
  }
  
  labels <- dn[[dim_name]]
  keep <- rep(TRUE, length(labels))
  
  # Apply each predicate (intersection)
  for (p in preds) {
    p_keep <- tp_eval_single_pred(x, dim_name, labels, p, dim_type, .strict)
    
    # Combine within-dimension (& or |) before intersecting with prior constraints.
    if (!is.null(p$combine) && !is.null(p$rhs_pred)) {
      rhs_keep <- tp_eval_single_pred(x, dim_name, labels, p$rhs_pred, dim_type, .strict)
      if (identical(p$combine, "&")) {
        p_keep <- p_keep & rhs_keep
      } else if (identical(p$combine, "|")) {
        p_keep <- p_keep | rhs_keep
      }
    }
    
    keep <- keep & p_keep
  }
  
  which(keep)
}

tp_eval_single_pred <- function(x, dim_name, labels, pred, dim_type, .strict) {
  op <- pred$op
  rhs <- pred$rhs
  
  if (dim_type == "categorical") {
    if (identical(op, "==") || identical(op, "%in%")) {
      rhs_chr <- as.character(rhs)
      tp_check_known_labels(rhs_chr, labels, dim_name, .strict)
      return(labels %in% rhs_chr)
    }
  }
  
  # Ordered dimensions
  if (identical(dim_name, "year")) {
    y <- suppressWarnings(as.integer(labels))
    if (anyNA(y)) {
      stop("Year dimension contains non-numeric labels; cannot apply ordered filters.", call. = FALSE)
    }
    return(tp_eval_ordered_numeric(y, op, rhs))
  }
  
  if (identical(dim_name, "age.char")) {
    # Discrete label selection for == / %in%
    if (identical(op, "==") || identical(op, "%in%")) {
      rhs_chr <- as.character(rhs)
      tp_check_known_labels(rhs_chr, labels, dim_name, .strict)
      return(labels %in% rhs_chr)
    }
    
    # Ordered/range semantics via half-open interval overlap.
    bnd <- tp_age_bounds(as.character(labels))
    return(tp_eval_age_bounds(bnd$start, bnd$end, op, rhs))
  }
  
  stop("Ordered filtering is not configured for dimension '", dim_name, "'.", call. = FALSE)
}

tp_eval_ordered_numeric <- function(vec, op, rhs) {
  rhs_num <- suppressWarnings(as.numeric(rhs))
  if (all(is.na(rhs_num))) {
    stop("Right-hand side is not numeric; cannot apply ordered filter.", call. = FALSE)
  }
  
  if (identical(op, "==")) return(vec == rhs_num[1])
  if (identical(op, "%in%")) return(vec %in% rhs_num)
  if (identical(op, ">"))  return(vec >  rhs_num[1])
  if (identical(op, ">=")) return(vec >= rhs_num[1])
  if (identical(op, "<"))  return(vec <  rhs_num[1])
  if (identical(op, "<=")) return(vec <= rhs_num[1])
  
  if (identical(op, "%between%")) {
    if (length(rhs_num) < 2) {
      stop("%between% requires a length-2 numeric vector like c(2010, 2020).", call. = FALSE)
    }
    lo <- min(rhs_num[1], rhs_num[2])
    hi <- max(rhs_num[1], rhs_num[2])
    return(vec >= lo & vec <= hi)
  }
  
  stop("Unsupported ordered operator: ", op, call. = FALSE)
}

tp_age_bounds <- function(labels) {
  # Parse age labels into half-open intervals [start, end)
  # "37"  -> [37, 38)
  # "5-9" -> [5, 10)
  # "85+" -> [85, Inf)
  # "All" -> [0, Inf)
  stopifnot(is.character(labels))
  
  start <- rep(NA_real_, length(labels))
  end   <- rep(NA_real_, length(labels))
  
  is_all <- stringr::str_detect(labels, stringr::regex("^all$", ignore_case = TRUE))
  is_plus <- stringr::str_detect(labels, "\\+")
  is_range <- stringr::str_detect(labels, "-")
  is_single <- stringr::str_detect(labels, "^\\d+$")
  
  if (any(is_all)) {
    start[is_all] <- 0
    end[is_all] <- Inf
  }
  
  if (any(is_plus)) {
    start[is_plus] <- as.numeric(stringr::str_extract(labels[is_plus], "^\\d+"))
    end[is_plus] <- Inf
  }
  
  if (any(is_range)) {
    a <- as.numeric(stringr::str_extract(labels[is_range], "^\\d+"))
    b <- as.numeric(stringr::str_extract(labels[is_range], "\\d+$"))
    start[is_range] <- a
    end[is_range] <- b + 1
  }
  
  if (any(is_single)) {
    a <- as.numeric(labels[is_single])
    start[is_single] <- a
    end[is_single] <- a + 1
  }
  
  if (anyNA(start) || anyNA(end)) {
    bad <- labels[is.na(start) | is.na(end)]
    stop(
      "Unrecognized age.char label(s): ", paste0("'", unique(bad), "'", collapse = ", "),
      ".", call. = FALSE
    )
  }
  
  list(start = start, end = end)
}

tp_eval_age_bounds <- function(start, end, op, rhs) {
  rhs_num <- suppressWarnings(as.numeric(rhs))
  if (all(is.na(rhs_num))) {
    stop("Right-hand side is not numeric; cannot apply ordered age filter.", call. = FALSE)
  }
  
  overlaps <- function(ts, te) {
    (start < te) & (end > ts)
  }
  
  if (identical(op, ">"))  return(overlaps(rhs_num[1] + 1, Inf))
  if (identical(op, ">=")) return(overlaps(rhs_num[1], Inf))
  if (identical(op, "<"))  return(overlaps(-Inf, rhs_num[1]))
  if (identical(op, "<=")) return(overlaps(-Inf, rhs_num[1] + 1))
  
  if (identical(op, "%between%")) {
    if (length(rhs_num) < 2) {
      stop("%between% requires a length-2 numeric vector like c(25, 44).", call. = FALSE)
    }
    lo <- min(rhs_num[1], rhs_num[2])
    hi <- max(rhs_num[1], rhs_num[2])
    return(overlaps(lo, hi + 1))
  }
  
  stop("Unsupported ordered age operator: ", op, call. = FALSE)
}

tp_check_known_labels <- function(requested, available, dim_name, .strict) {
  requested <- unique(as.character(requested))
  available <- as.character(available)
  bad <- setdiff(requested, available)
  if (!length(bad)) return(invisible(TRUE))
  
  sugg <- tp_suggest_labels(bad, available)
  msg <- paste0(
    "Unknown label(s) for ", dim_name, ": ", paste0("'", bad, "'", collapse = ", "), "."
  )
  if (length(sugg)) {
    msg <- paste0(msg, " Did you mean: ", paste0("'", sugg, "'", collapse = ", "), "?")
  }
  
  if (.strict) stop(msg, call. = FALSE)
  warning(msg, call. = FALSE)
  invisible(FALSE)
}

tp_suggest_labels <- function(bad, available, n = 3) {
  bad <- as.character(bad[1])
  d <- utils::adist(bad, available)
  ord <- order(d)
  head(available[ord], n)
}

#  Between operator  ----------------------------

#' Infix range helper for tidy predicates
#'
#' @description
#' `%between%` is a small infix helper intended for use inside `filter()` calls.
#' It returns a call of the form `x %between% c(lower, upper)` that downstream
#' methods (e.g., `filter.poparray()`) interpret as an inclusive range constraint.
#'
#' For `poparray`:
#' - `year %between% c(2010, 2020)` selects years 2010–2020 (inclusive).
#' - `age.char %between% c(25, 44)` selects age labels whose intervals overlap
#'   ages 25–44 (inclusive; half-open target [25, 45)).
#'
#' @param x A vector or a dimension symbol used inside a filtering expression.
#' @param rng A length-2 vector giving lower and upper bounds. Order does not
#'   matter; `c(44, 25)` is treated the same as `c(25, 44)`.
#'
#' @return A logical vector when evaluated in normal R contexts.
#' When used inside `filter.poparray()`, the expression is captured and interpreted
#' by the method (so it will not be evaluated in the usual way).
#'
#' @examples
#' # Base usage on vectors:
#' 5 %between% c(1, 10)
#' c(1, 5, 11) %between% c(1, 10)
#'
#' # Typical intended usage (captured by filter.poparray()):
#' # dplyr::filter(pop, year %between% c(2015, 2020))
#' # dplyr::filter(pop, age.char %between% c(25, 44))
#'
#' @export
`%between%` <- function(x, rng) {
  if (length(rng) != 2L) {
    stop("`%between%` expects a length-2 vector like c(lower, upper).", call. = FALSE)
  }
  lo <- rng[[1]]
  hi <- rng[[2]]
  if (is.na(lo) || is.na(hi)) {
    return(rep(FALSE, length(x)))
  }
  if (hi < lo) {
    tmp <- lo
    lo <- hi
    hi <- tmp
  }
  x >= lo & x <= hi
}