



#' @import multivarious
spacegen <- function(nsides=12, nsamples=100000, minrad=.1, maxrad=.5) {

  R <- t(replicate(nsamples, runif(nsides, min=minrad, max=maxrad)))
  pret <- multivarious::pca(t(R), preproc=multivarious::pass())

  ret <- list(
    pcret=pret,
    nsamples=nsamples,
    nsides=nsides
  )

  class(ret) <- "pca_shape_space"
  ret

}

#' @export
sample_shape.pca_shape_space <- function(x, distinctiveness=1, area=.2, rescale=TRUE, shape=.8, ...) {

  # c1 <- median(multivarious::scores(x$pcret)[,1])
  #
  # sc <- multivarious::scores(x$pcret)
  # sc <- sc[,-1]
  #
  # r <- apply(sc, 2, function(x) range(x))
  #
  # nvec <- sapply(1:length(cvec), function(i) {
  #   val <- cvec[i]
  #   delta <- r[2,i] - r[1,i]
  #   p <- val * delta + r[1,i]
  # })


  vec <- sapply(1:ncol(x$pcret$v), function(i) sample(x$pcret$v[,i],1)) * distinctiveness
  #print(radii)
  radii = multivarious::scores(x$pcret) %*% vec
  shape_from_radii(radii[,1], area=area, rescale=rescale, shape=.8)

}



