sumln <- function(x) {
    m <- max(x)
    r <- sum(exp(x - m))
    m + log(r)
}

logl <- function(...) UseMethod("logl")

logl.class <- function(x, m) {
  log(m$class$p[x])
}

logl.rvsp <- function(x, c, m) {
  mc <- m$classes[[c]]

  if (is.na(x)) {
    ll <- log(m$miss.rvsp$p)
  } else {
    ll <- log(1 - m$miss.rvsp$p)
    ll <- ll + dnorm(x, mc$rvsp$mean, mc$rvsp$sd, log=TRUE)
  }

  ll
}

logl.rhd <- function(x, c, m) {
  mc <- m$classes[[c]]

  if (is.na(x)) {
    ll <- log(m$miss.rhd$p)
  } else {
    ll <- log(1 - m$miss.rhd$p)
    ll <- ll + log(mc$rhd$p[x])
  }

  ll
}

logl.pfvc <- function(x, c, m) {
  mc <- m$classes[[c]]

  if (is.na(x)) {
    ll <- log(m$miss.pfvc$p)
  } else {
    ll <- log(1 - m$miss.pfvc$p)
    ll <- ll + dnorm(x, mc$pfvc$mean, mc$pfvc$sd, log=TRUE)
  }

  ll
}

logl.pdlco <- function(x, c, m) {
  mc <- m$classes[[c]]

  if (is.na(x)) {
    ll <- log(m$miss.pdlco$p)
  } else {
    ll <- log(1 - m$miss.pdlco$p)
    ll <- ll + dnorm(x, mc$pdlco$mean, mc$pfvc$sd, log=TRUE)
  }

  ll
}

logl.visit <- function(x, c, m) {
  ll <- 0

  if (x$has.echo) {
    ll <- ll + logl.rvsp(x$rvsp, c, m)
    ll <- ll + logl.rhd(x$rhd, c, m)
  }

  if (x$has.pft) {
    ll <- ll + logl.pfvc(x$pfvc, c, m)
    ll <- ll + logl.pdlco(x$pdlco, c, m)
  }

  ll
}

logl.patient <- function(x, c, m) {
  ll <- 0

  for (i in seq(nrow(x))) {
    ll <- ll + logl.visit(x[i, ], c, m)
  }

  ll
}

logl.data <- function(x, m) {
  nc <- length(m$classes)

  patient.marginal <- function(x) {
    ll <- numeric(nc)

    for (c in seq(nc)) {
      ll[c] <- logl.class(c, m)
      ll[c] <- ll[c] + logl.patient(x, c, m)
    }

    sumln(ll)
  }

  ll <- daply(x, ~ ptid, patient.marginal)
  sum(ll)
}
