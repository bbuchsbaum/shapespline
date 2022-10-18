
#' @export
sample_shape <- function(x, ...) {
  UseMethod("sample_shape", x)
}

#' @export
jitter_poly <- function(x, ...) {
  UseMethod("jitter_poly", x)
}
