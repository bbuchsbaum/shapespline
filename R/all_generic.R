

sample_shape <- function(x, ...) {
  UseMethod("sample_shape", x)
}

jitter_poly <- function(x, ...) {
  UseMethod("jitter_poly", x)
}
