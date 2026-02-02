# ------------------------------------------------------------------------------------------------------------------->
# Script: utils-compat.R
# Description:
#   Ensure %||% exists for older R installations
# 
# ------------------------------------------------------------------------------------------------------------------->
# Author: Russ Jones
# Created: January 16, 2026
# ------------------------------------------------------------------------------------------------------------------->


# Define a %||% operator for R < 4.4.0 using rlang's implementation.
# If base::%||% exists (R >= 4.4.0), do nothing and use the built-in.
# NOTE: do NOT import the operator from rlang in the NAMESPACE file,
#       to avoid masking base::%||% on R >= 4.4.0.
if (!exists("%||%", envir = baseenv(), inherits = FALSE)) {
  `%||%` <- rlang::`%||%`
}
