#' Select age categories overlapping an inclusive age range
#'
#' Returns a logical index for dimension subsetting. Only age labels are
#' inspected; no population values are read. Bounds specify whole-year ages
#' inclusively, so `lower = 25, upper = 44` represents `[25, 45)`.
#' Labels may be single ages (`"37"`), inclusive groups (`"5-9"`), or open-ended
#' groups (`"85+"`). Legacy `"All"` labels are interpreted as `[0, Inf)`.
#' Whole categories are selected whenever they overlap the requested range;
#' categories are not split and partial populations are not estimated.
#' The selection agrees with age `%between%` filtering on valid labels.
#'
#' @param ages Character vector or factor of age category labels.
#' @param lower Non-negative whole-year lower bound. Defaults to zero.
#' @param upper Non-negative whole-year upper bound, or `Inf` for no upper
#'   limit. Defaults to `Inf`. Must be at least `lower`.
#' @return A logical vector with the same length, order, and names as `ages`.
#'   Missing or malformed labels and invalid bounds produce an error.
#' @export
#' @examples
#' ages <- c("0-4", "5-9", "10-14", "85+")
#' age_overlaps(ages, lower = 5, upper = 9)
#' age_overlaps(ages, lower = 8, upper = 12)
#' \dontrun{
#' ages <- dimnames(pop)$age.char
#' pop[age.char = age_overlaps(ages, lower = 25, upper = 44)]
#' pop[age.char = !age_overlaps(ages, lower = 65)]
#' }
age_overlaps <- function(ages, lower = 0, upper = Inf) {
  checkmate::assert_number(lower, lower = 0, finite = TRUE)
  checkmate::assert_number(upper, lower = 0)
  if (lower != floor(lower) || upper != floor(upper)) {
    cli::cli_abort("Age bounds must be whole-year ages, with {.code Inf} allowed for {.arg upper}.")
  }
  if (upper < lower) {
    cli::cli_abort("{.arg upper} must be at least {.arg lower}.")
  }
  if (!is.character(ages) && !is.factor(ages)) {
    cli::cli_abort("{.arg ages} must be a character vector or factor of age labels.")
  }
  original_names <- names(ages)
  labels <- as.character(ages)
  if (anyNA(labels) || any(!grepl("^([0-9]+|[0-9]+-[0-9]+|[0-9]+\\+|[Aa][Ll][Ll])$", labels))) {
    cli::cli_abort("{.arg ages} must contain non-missing labels like {.val 37}, {.val 5-9}, or {.val 85+}.")
  }
  bounds <- tp_age_bounds(labels)
  if (any(!is.finite(bounds$start)) ||
      any(bounds$end <= bounds$start)) {
    cli::cli_abort("Age labels must describe valid, increasing intervals.")
  }
  keep <- tp_eval_age_bounds(bounds$start, bounds$end, "%between%", c(lower, upper))
  names(keep) <- original_names
  keep
}
