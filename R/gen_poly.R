

#' create a new shape by jittering the radii of an existing shape.
#'
#' @param x the shape to jitter
#' @param sd the standard deviation of the noise
#' @param rescale whether to rescale the shape to maintain the current area
#' @export
jitter_poly.poly_shape <- function(x, sd=.003, area=.3, rescale=FALSE,...) {
  while (TRUE) {
    radii <- x$radii + rnorm(length(x$radii), sd=sd)
    if (any(radii < .1 | radii > .5) || sum(radii) < 3) {
      next
    } else {
      break
    }
  }

  shape_from_radii(radii,area=area,rescale=rescale)
}


#' @export
random_walk <- function(x, n=10, sd=.01, area=.3, rescale=FALSE) {
  res <- vector(mode="list", n)

  xcur=x
  for (i in 1:n) {
    xcur <- jitter_poly(xcur, sd=sd, area=area,rescale=rescale)
    res[[i]] <- xcur
  }
  res
}

#' @export
rescale_poly <- function(p, area=.3) {
  nsides <- length(p$radii)

  m <- cbind(p$x, p$y)
  m <- rbind(m, m[1,])
  current.area <- 0.5 * (sum(m[1:nsides,1]*m[2:(nsides+1),2]) - sum(m[1:nsides,2]*m[2:(nsides+1),1]))

  p$x <- p$x * sqrt(area/current.area)
  p$y <- p$y * sqrt(area/current.area)

  p
}

#' @export
shape_sim <- function(x,y) {
  proxy::simil(t(as.matrix(x$radii)), t(as.matrix(y$radii)))
}


#' @export
shape_from_radii <- function(radii, area=.3, rescale=TRUE, shape=.8) {
  nsides <- length(radii)
  angle <- (2*pi)/nsides
  angles <- angle * 1:nsides

  points <- list(x=NULL, y=NULL)
  points$x <- cos(angles) * radii
  points$y <- sin(angles) * radii

  m <- matrix(unlist(points), ncol=2)
  m <- rbind(m, m[1,])
  current.area <- 0.5 * (sum(m[1:nsides,1]*m[2:(nsides+1),2]) - sum(m[1:nsides,2]*m[2:(nsides+1),1]))

  if (rescale) {
    points$x <- points$x * sqrt(area/current.area)
    points$y <- points$y * sqrt(area/current.area)
  }

  #points$x^2 + points$y^2
  points$angles = angles
  points$radii = sqrt(points$x^2 + points$y^2)

  grob <- xsplineGrob(points$x, points$y, shape=shape, open=FALSE)
  xpts <- xsplinePoints(grob)
  xp <- convertX(xpts$x, "npc", valueOnly=TRUE)
  yp <- convertX(xpts$y, "npc", valueOnly=TRUE)

  points$xp <- xp
  points$yp <- yp
  points$sradii <- sqrt(xp^2 + yp^2)
  points$shape <- shape
  class(points) <- "poly_shape"
  points
}


#' generate a new shape
#'
#' @param nsides the number of sides
#' @param area the area of the shape
#' @param rescale whether to rescale the shape to a fixed area given by the `area` parameter.
#' @export
#' @import grid
gen_shape <- function(nsides=16, area=.3, rescale=TRUE, shape=.8) {
  div <- 20
  # Find the radius of the circumscribed circle, and the angle of each point if this was a regular polygon
  radius <- sqrt((2*area)/(nsides*sin((2*pi)/nsides)))
  angle <- (2*pi)/nsides

  #Randomize the radii/angles
  #radii <- rnorm(nsides, radius, radius/div)
  radii <- runif(nsides, min=.1, max=.5)
  shape_from_radii(radii, area=area, rescale=rescale, shape=shape)
}


#' @export
#' @param seed the seed shape
#' @param nsteps the number of steps
#' @param njourneys the number of journeys
gen_rw_family <- function(seed, nsteps, njourneys) {
  run_journey <- function() {
    out <- vector(mode="list", nsteps)
    x <- seed
    for (i in 1:nsteps) {
      x <- jitter_poly(x)
      out[[i]] <- x
    }

    out
  }

  out2 <- unlist(lapply(1:njourneys, function(i) {
    run_journey()
  }), recursive=FALSE)

}

#' @export
gen_gaussian_family <- function(seed, nexemplars, sd=.03, rescale=FALSE) {
  ret <- lapply(1:nexemplars, function(i) {
    jitter_poly(seed,sd=sd)
  })

  out <- vector(nexemplars+1, mode="list")
  out[[1]] <- seed
  out[2:(length(out))] <- ret

  out
}


#' @export
plot.poly_shape <- function(x, shape=.8, draw=TRUE, ...) {
  grid.newpage()
  x$x <- x$x - mean(x$x) + .5
  x$y <- x$y - mean(x$y) + .5
  grob <- xsplineGrob(x$x, x$y, shape=shape, gp = gpar(col = "orange", fill = "orange"), open=FALSE)
  #grid.text(label=1:length(p$x), x=p$x, y=p$y, default.units="npc")
  if (draw) {
    grid.draw(grob)
  }

  grob
}





