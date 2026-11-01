# -------------------------------------------------------------------------------------->
# Script: utils-compat.R
# Description:
#   Compatibility support uses the base %||% operator when available and falls back to rlang only when the
#   base operator is absent.
#
#   Defines the fallback binding for older R installations without replacing an existing base
#   implementation.
#
# -------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: January 16, 2026
# -------------------------------------------------------------------------------------->

if (!exists("%||%", envir = baseenv(), inherits = FALSE)) {
  `%||%` <- rlang::`%||%`
}
