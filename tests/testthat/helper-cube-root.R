Sys.setenv(TARR_POP_SKIP_CUBE_SETUP = "true")

.test_cube_root <- file.path(tempdir(), "tarr-pop-test-cubes")

reset_test_cube_root <- function() {
  tarr.pop::set_cube_path(.test_cube_root, create = TRUE, persist = FALSE)
  tarr.pop::init_cubes(.test_cube_root, persist = FALSE)
  invisible(.test_cube_root)
}
