source("model2-estimate.R")
source("data.R")

dat <- get.pft.echo()

nc <- 3
ns <- 4

ptids <- unique(dat$ptid)
splits <- sample(1:ns, length(ptids), replace=TRUE)
names(splits) <- ptids
dat$split <- splits[as.character(dat$ptid)]

res <- data.frame()

for (c in seq(nc)) {
  for (s in seq(ns)) {
    
    message("Starting split ", s, " with ", c, " classes.")

    train <- subset(dat, split != s)
    test <- subset(dat, split == s)
    
    if (c == 1) {
      m <- estimate.model2(train, c, 1)
    } else {
      m <- estimate.model2(train, c, 5)
    }

    ll <- logl.data(test, m)
    message(sprintf("Test LL= %10.2f", ll))

    res <- rbind(res, data.frame(nc=c, s=s, ll=ll))
  }
}

write.csv(res, file="cv-ll.csv", row.names=FALSE)
