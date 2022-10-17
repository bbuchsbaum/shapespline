
gen_square_space <- function(x1,x2, y1, y2) {
  ret <- list(left=x1, right=x2, top=y1, bottom=y2)
  class(ret) <- "square_space"
  ret
}

make_shape.square_space <- function(x, a=.5,b=.5) {
  p1 <- morph_poly(x$left, x$right, sd=0, prop=a)
  p2 <- morph_poly(x$top, x$bottom, sd=0, prop=b)
  p3 <- morph_poly(p1, p2, sd=0, prop=.5)
}
