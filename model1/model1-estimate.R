library(plyr)

source("model2-likelihoods.R")
source("model2-posteriors.R")

init.model2 <- function(nc) {
  model <- list()

  model$class <- list(p=numeric(nc))
  model$miss.rvsp <- list(p=0)
  model$miss.rhd <- list(p=0)
  model$miss.pfvc <- list(p=0)
  model$miss.pdlco <- list(p=0)
  model$classes <- list()

  for (c in seq(nc)) {
    model$classes[[c]] <- list()
    model$classes[[c]]$rvsp <- list(mean=0, sd=0)
    model$classes[[c]]$rhd <- list(p=numeric(3))
    model$classes[[c]]$pfvc <- list(mean=0, sd=0)
    model$classes[[c]]$pdlco <- list(mean=0, sd=0)
  }

  model
}

estimate.bern <- function(x, w=1) {
  if (missing(w)) w <- rep(w, length(x))
  
  w <- w / sum(w)
  list(p = sum(w * x))
}

estimate.mult <- function(x, w=1) {
  if (missing(w)) w <- rep(w, length(x))

  w <- w / sum(w)
  list(p = xtabs(w ~ x))
}

estimate.norm <- function(x, w=1) {
  if (missing(w)) w <- rep(w, length(x))

  w <- w / sum(w)
  m <- weighted.mean(x, w)
  v <- sum(w * (x - m)^2)
  list(mean=m, sd=sqrt(v))
}

estimate.model2 <- function(x, nc, ni=5) {
  pid <- unique(x$ptid)
  np <- length(pid)
  z <- matrix(runif(np * nc), np, nc)
  z <- sweep(z, 1, rowSums(z), "/")
  rownames(z) <- pid

  model <- init.model2(nc)

  echo <- x[x$has.echo, ]
  pft <- x[x$has.pft, ]

  i <- !is.na(echo$rvsp)
  rvsp <- structure(echo$rvsp[i], names=echo$ptid[i])

  i <- !is.na(echo$rhd)
  rhd <- structure(echo$rhd[i], names=echo$ptid[i])

  i <- !is.na(pft$pfvc)
  pfvc <- structure(pft$pfvc[i], names=pft$ptid[i])

  i <- !is.na(pft$pdlco)
  pdlco <- structure(pft$pdlco[i], names=pft$ptid[i])

  model$miss.rvsp <- estimate.bern(is.na(echo$rvsp))
  model$miss.rhd <- estimate.bern(is.na(echo$rhd))
  model$miss.pfvc <- estimate.bern(is.na(pft$pfvc))
  model$miss.pdlco <- estimate.bern(is.na(pft$pdlco))

  for (i in seq(ni)) {
    
    model$class$p <- colSums(z) / sum(z)
    
    for (c in seq(nc)) {
      w <- z[, c]
      model$classes[[c]]$rvsp <- estimate.norm(rvsp, w[names(rvsp)])
      model$classes[[c]]$rhd <- estimate.mult(rhd, w[names(rhd)])
      model$classes[[c]]$pfvc <- estimate.norm(pfvc, w[names(pfvc)])
      model$classes[[c]]$pdlco <- estimate.norm(pdlco, w[names(pdlco)])
    }

    message(sprintf("%2d: LL=%10.2f", i, logl.data(x, model)))
    z <- daply(x, ~ ptid, function(df) post.class(df, model), .drop_o=FALSE)
  }

  model
}
