library(plyr)

get.pft.echo <- function()
{
  pft <- read.pft.data()
  echo <- read.echo.data()

  pft$has.pft <- TRUE
  echo$has.echo <- TRUE

  dat <- merge(pft, echo, by=c("ptid", "date"), all=TRUE)
  dat$has.pft[is.na(dat$has.pft)] <- FALSE
  dat$has.echo[is.na(dat$has.echo)] <- FALSE
  arrange(dat, ptid, date)
}

read.pft.data <- function()
{
  dat <- read.csv("../data/csv/tPFT.csv", stringsAsFactors=FALSE)
  names(dat) <- tolower(names(dat))
  dat <- dat[, c("ptid", "date", "perc.fvc.of.predicted", "perc.dlco.of.predicted")]
  names(dat) <- c("ptid", "date", "pfvc", "pdlco")
  dat$date <- as.Date(dat$date)
  dat
}

read.echo.data <- function()
{
  dat <- read.csv("../data/csv/tECHO.csv", stringsAsFactors=FALSE)
  names(dat) <- tolower(names(dat))

  vars <- c("ptid", "date.of.echo")
  vars <- c(vars, "ra.dil", "rv.dil")
  vars <- c(vars, "interventricular.septum", "rvsp")
  dat <- dat[, vars]
  dat$date.of.echo <- as.Date(dat$date.of.echo)

  dat$ra.dil <- factor(dat$ra.dil, 0:1)
  dat$rv.dil <- factor(dat$rv.dil, 0:1)
  dat$interventricular.septum <- factor(dat$interventricular.septum, 0:1)

  dat <- arrange(dat, ptid, date.of.echo)

  calc.right.heart.dil <- function(df)
  {
    dil <- c(df$ra.dil, df$rv.dil, df$interventricular.septum)

    if (any(is.na(dil))) {
      dil <- NA
    } else if (dil[3] == 2) {
      dil <- 3
    } else if (2 %in% dil[1:2]) {
      dil <- 2
    } else {
      dil <- 1
    }

    data.frame(rvsp=df$rvsp, rhd=dil)
  }
  
  dat <- ddply(dat, ~ ptid + date.of.echo, calc.right.heart.dil)
  names(dat) <- c("ptid", "date", "rvsp", "rhd")
  dat
}
