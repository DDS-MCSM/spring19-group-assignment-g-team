#
# # Default parameters
# verbose <- TRUE
# seed <- 666
# scansio.url <- "https://opendata.rapid7.com/sonar.tcp/2019-04-04-1554350684-ftp_21.csv.gz"
# maxmind.url <- "https://geolite.maxmind.com/download/geoip/database/GeoLite2-City-CSV.zip"
# scope <- 500
# output.file <- "geoftps.rds"
#
# # Initial Setup
# log.msg(verbose, "[*] Initial setup")
# tini <- Sys.time()
# set.seed(seed)
# dir.data <- file.path(getwd(), "data")
# create.directory(verbose, dir.data)
#
# # scans.io - Obtener datos en crudo
# log.msg(verbose, "[*] Read RAW data from scans.io")
# scansio.source <- file.path(getwd(), "data","scans.io.tcp21.csv")
# scansio.file.gz <- paste(scansio.source, ".gz", sep = "")
# download.file(url = scansio.url, destfile = scansio.file.gz)
# decompress.data(verbose, scansio.file.gz, "gz", TRUE)
# df.tcp21 <- read.csv(scansio.source, stringsAsFactors = FALSE)
#
# # Maxmind - Obtener datos en crudo (city)
# log.msg(verbose, "[*] Read RAW data from MaxMind")
# maxmind.file <- file.path(getwd(), "data", "maxmind.zip")
# download.file(url = maxmind.url, destfile = maxmind.file)
#
#
#
# zipfiles <- unzip(zipfile = maxmind.file, list = T)
# maxmind.source <- zipfiles$Name[grep(pattern = ".*GeoLite2-City-Blocks-IPv4.csv", x = zipfiles$Name)]
# unzip(zipfile = maxmind.file, exdir = dir.data, files = maxmind.source)
# maxmind.source <- file.path(getwd(), "data", maxmind.source)
# df.maxmind <- read.csv(maxmind.source, stringsAsFactors = FALSE)
# rm(maxmind.file, zipfiles)
#
# # Seleccionamos una muestra de scans
# if (verbose) print("[*] Subseting scans data set")
# df.tcp21$saddr.num <- iptools::ip_to_numeric(df.tcp21$saddr)
# df.tcp21$daddr.num <- iptools::ip_to_numeric(df.tcp21$daddr)
# muestra <- sample(1:nrow(df.tcp21), scope)
# df.scans <- df.tcp21[muestra,]
# rm(muestra)
#
# # Para geolocalizar una IP en un rango comprobaremos si está entre la primera
# # y la ultima ip de cada rango en MaxMind.
#
# # Maxmind elegante
# if (verbose) print("[*] Expanding MaxMind network ranges")
# df.maxmind <- cbind(df.maxmind, iptools::range_boundaries(df.maxmind$network))
# df.maxmind$rowname <- as.integer(row.names(df.maxmind))
#
# # Usamos multiples cpu's para geolocalizar IPs en rangos
# if (verbose) print("[*] Foreach IP (source and destination) identify network range using parallel computing")
# no_cores <- parallel::detectCores() - 1
# cl <- parallel::makeCluster(no_cores)
# parallel::clusterExport(cl, "df.maxmind")
# df.scans$sloc <- sapply(df.scans$saddr.num,
#                         function(ip)
#                           which((ip >= df.maxmind$min_numeric) &
#                                   (ip <= df.maxmind$max_numeric)))
# df.scans$dloc <- sapply(df.scans$daddr.num,
#                         function(ip)
#                           which((ip >= df.maxmind$min_numeric) &
#                                   (ip <= df.maxmind$max_numeric)))
# parallel::stopCluster(cl)
# rm(cl, no_cores)
#
# # Join and tidy data frame (source address)
# if (verbose) print("[*] Joining source IP's with geolocation data")
# df <- dplyr::left_join(df.scans, df.maxmind, by = c("sloc" = "rowname"))
# df <- dplyr::select(df, timestamp_ts, saddr, latitude, longitude, accuracy_radius,
#                     is_anonymous_proxy, is_satellite_provider)
# names(df) <- c("timestamp_ts", "saddr", "slatitude", "slongitude",
#                "accuracy_radius", "is_anonymous_proxy", "is_satellite_provider")
#
# # Join and tidy data frame (destination address)
# if (verbose) print("[*] Joining destination IP's with geolocation data")
# suppressMessages(library(dplyr))
# df.dst <- df.scans %>%
#   left_join(df.maxmind, by = c("dloc" = "rowname")) %>%
#   select(daddr, latitude, longitude)
# names(df.dst) <- c("daddr", "dlatitude", "dlongitude")
# df <- dplyr::bind_cols(df, df.dst)
# rm(df.dst, df.scans)
#
# # Set categoric variables as factors
# if (verbose) print("[*] Tidy data and save it")
# df$is_anonymous_proxy <- as.factor(df$is_anonymous_proxy)
# df$is_satellite_provider <- as.factor(df$is_satellite_provider)
# saveRDS(object = df, file = file.path(getwd(), "data", output.file))
# fini <- Sys.time()
#
# # Summary
# fini - tini
# summary(df)
