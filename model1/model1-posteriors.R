post <- function(...) UseMethod("post")

post.class <- function(x, m) {
  nc <- length(m$classes)
  ll <- numeric(nc)

  for (c in seq(nc)) {
    ll[c] <- logl.class(c, m)
    ll[c] <- ll[c] + logl.patient(x, c, m)
  }

  ll <- ll - sumln(ll)
  exp(ll)
}
