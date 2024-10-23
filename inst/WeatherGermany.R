## Base url.
url <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/"

## Function to download and process data in a single .zip-file.
download_dwd <- function(file) {
  ## Create a temporary directory,
  ## will be deleted afterwards.
  tdir <- tempfile()
  dir.create(tdir)

  ## Get the current working directory.
  owd <- getwd()

  ## Set working directory to tdir.
  setwd(tdir)

  ## Donwload the .zip-file.
  download.file(paste0(url, "/", file), file)

  ## Extract the data from zip-file.
  unzip(file)

  ## Search for file with "produkt" in the name.
  dfile <- grep("produkt", dir(), ignore.case = TRUE, value = TRUE)

  ## Read in data.
  d <- read.table(dfile, sep = ";", header = TRUE)

  ## Extract only variables of interest.
  vn <- c("STATIONS_ID", "MESS_DATUM", "FX", "RSK", "TXK", "TNK", "SDK", "QN_4")
  d <- d[, vn]

  ## Rename columns.
  names(d) <- c("id", "date", "Wmax", "pre", "Tmax", "Tmin", "sun", "Q4")

  ## Set -999 values to NA values.
  for(j in c("Wmax", "pre", "Tmax", "Tmin", "sun", "Q4"))
    d[[j]][d[[j]] <= -999] <- NA

  ## Transform to real date variable.
  d$date <- as.character(d$date)
  d$date <- as.Date(d$date, format = "%Y%m%d")
  
  ## Read in meta-infomation.
  mfile <- grep("Metadaten_Geographie", dir(), value = TRUE)
  lines <- readLines(mfile, encoding = "latin1")
  lines <- iconv(lines, from = "latin1", to = "UTF-8")
  writeLines(lines, mfile)
  m <- read.table(mfile, sep = ";", header = TRUE, encoding = "UTF-8")

  ## Again, create date variables.
  m$von_datum <- as.character(m$von_datum)
  m$von_datum <- as.Date(m$von_datum, format = "%Y%m%d")
  
  m$bis_datum <- as.character(m$bis_datum)
  m$bis_datum <- as.Date(m$bis_datum, format = "%Y%m%d")
  if(!all(is.na(m$bis_datum))) {
    m$bis_datum[is.na(m$bis_datum)] <- max(m$bis_datum, na.rm = TRUE) + 365*100
  } else {
    m$bis_datum <- Sys.Date() + 100*365
  }
  if(all(is.na(m$von_datum))) {
    m$von_datum <- Sys.Date() - 1000*365
  } else {
    m$von_datum[is.na(m$von_datum)] <- max(m$von_datum, na.rm = TRUE)
  }
  
  ## Pre-initialize variables we want to add from the meta-information.
  d$lon <- d$lat <- d$alt <- d$name <- NA
  
  ## Cycle over the rows of the meta-infomation.
  for(i in 1:nrow(m)) {
    ## Creates an index, use only observations that are in the time span.
    j <- d$date >= m$von_datum[i] & d$date <= m$bis_datum[i]

    ## Add geographic information.
    d$lon[j] <- m$Geogr.Laenge[i]
    d$lat[j] <- m$Geogr.Breite[i]
    d$alt[j] <- m$Stationshoehe[i]
    d$name[j] <- m$Stationsname[i]
  }

  ## Set to old working directory.
  setwd(owd)
  
  ## Remove temporary folder.
  unlink(tdir)

  ## Remove "bad" data.
  d <- subset(d, Q4 == 10)

  ## Remove quality variable, not needed anymore.
  d$Q4 <- NULL

  ## Remove rownames.
  rownames(d) <- NULL
  
  ## Return final data.frame.
  return(d)
}

## Read in all station .zip-file names.
files <- readLines("stations.txt")
files <- files[files != ""]
files <- strsplit(files, " ")
files <- sapply(files, function(x) x[1])

## Create a list, each list entry holds one data set.
df <- list()

## Iterate over all station names.
k <- 1
for(f in files) {
  ## Create data set, store in list.
  df[[k]] <- download_dwd(f)

  ## Increase list iterator.
  k <- k + 1

  ## Sleep a little.
  Sys.sleep(runif(1, 3, 3.2))
}

## Finally, merge to one big data.frame.
library("data.table")
WeatherGermany <- as.data.frame(rbindlist(df))

## Save final data set.
save(WeatherGermany, file = "WeatherGermany.rda", compress = "xz")

