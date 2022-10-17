morph_poly <- function(x, y, method=c("average", "trait"), sd=.01, prop=.5) {
  method = match.arg(method)
  angle <- (2*pi)/length(x$radii)
  angles <- angle * 1:length(x$radii)

  shifter <- function(x, n = 1) {
    if (n == 0) x else c(tail(x, -n), head(x, n))
  }

  if (method == "average") {
    radii <- (x$radii*(prop) + y$radii*(1-prop)) + rnorm(length(x$radii), sd=sd)
  } else {
    #s1 <- rep(TRUE, length(x$radii))
    #id <- sample(1:length(s1), 1)
    #els <- seq(id, id + length(x$radii)/2)
    #sel <- sort(ifelse(els > length(x$radii), els %% length(x$radii), els))
    #sel <- seq(1, length(x$radii)) %in% sel
    #sel <- rep(c(TRUE, FALSE), each=length(x$radii)/4, length.out=length(x$radii))
    #shift <- sample(1:length(x$radii),1)
    #sel <- shifter(sel, shift)
    #radii <- ifelse(sel, x$radii, y$radii) + rnorm(length(x$radii), sd=sd)
    sel <- runif(1:length(x$radii))
    radii <- ifelse(sel > .5, x$radii, y$radii)
    radii <- radii + rnorm(length(radii), sd=sd)
  }

  shape_from_radii(radii)

}

# s1 <- gen_shape()
# s2 <- gen_shape()
# s3 <- gen_shape()
# s4 <- gen_shape()
#
# r1 <- lapply(seq(0,1, by=.01), function(p) morph_poly(s1,s2,prop=p))
# r2 <- lapply(seq(0,1, by=.01), function(p) morph_poly(s2,s3,prop=p))
# r3 <- lapply(seq(0,1, by=.01), function(p) morph_poly(s3,s4,prop=p))
# r4 <- lapply(seq(0,1, by=.01), function(p) morph_poly(s4,s1,prop=p))
#
# rall <- c(r1,r2,r3,r4)
#
# rmat = do.call(rbind, lapply(rall, "[[", "radii"))
# A <- neighborweights::graph_weights(rmat, k=20)
# ctd = neighborweights::commute_time(A,ncomp=5)
# plot(ctd$cds[,1], ctd$cds[,2])
# plot(ctd$cds[,2], ctd$cds[,3])
# plot(ctd$cds[,3], ctd$cds[,4])
# plot(ctd$cds[,1], ctd$cds[,2])
# plot(ctd$cds[,1], ctd$cds[,3])
# plot(ctd$cds[,1], ctd$cds[,4])

