## Load the 
load("WeatherGermany.rda")

storms <- list()

ids <- unique(WeatherGermany$id)

k <- 1L

for(i in ids) {
  cat(i, "/", sep = "")

  d <- subset(WeatherGermany, id == i)
  if(!all(is.na(d$Wmax))) {
    d$year <- as.POSIXlt(d$date)$year + 1900
    a <- aggregate(Wmax ~ year, data = d, FUN = function(x) {
      i <- x >= 24.5
      c(
        "counts" = sum(i, na.rm = TRUE),
        "mean" = if(any(i)) mean(x[i], na.rm = TRUE) else NA,
        "max" = if(any(i)) max(x[i], na.rm = TRUE) else NA,
        "sd" = if(any(i)) sd(x[i], na.rm = TRUE) else NA
      )
    })
    a <- as.data.frame(do.call("cbind", a))

    a$id <- d$id[1L]
    a$name <- d$name[1L]
    a$alt <- mean(d$alt, na.rm = TRUE)
    a$lat <- mean(d$lat, na.rm = TRUE)
    a$lon <- mean(d$lon, na.rm = TRUE)

    a <- a[, c("id", "year", "counts", "mean", "max", "sd", "name", "alt", "lon", "lat")]

    a <- a[order(a$year), ]

    storms[[k]] <- a

    k <- k + 1L
  }
}

cat("\n")

storms <- do.call("rbind", storms)

storms$id <- as.factor(storms$id)

save(storms, file = "storms.rda", compress = "xz")

