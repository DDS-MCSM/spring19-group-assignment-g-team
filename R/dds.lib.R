#' Logs a message to the Log. If a filename is passed as parameter, it will also print the message into the specified log file.
#'
#' @details It's very useful to log messages through a function like this, as it will save you time when developing. You will have to just change the name of the variable 'pVerbose' to log or stop logging messages.
#' @param pVerbose A boolean (TRUE or FALSE).
#' @param pMessage The message you want to show in the console.
#' @return nothing.
#' @export
#'
#' @examples --
log_msg <- function(pVerbose, pMessage, pLogFile) {
  if (pVerbose) {
    print(pMessage)
#    if (nchar(pLogFile) > 0) {
#      ######## Log to file
#    }
  }
}


#' Creates a directory.
#'
#' @details Creates a directory.
#' @param pVerbose A boolean (TRUE or FALSE).
#' @param pDirectory Full path of the directory you want to create.
#' @return nothing.
#' @export
#'
#' @examples --
create_directory <- function(pVerbose, pDirectory) {
  if (!dir.exists(pDirectory)) {
    log_msg(pVerbose, "[*] Create directory", "")
    dir.create(pDirectory)
  }
  else {
    log_msg(pVerbose, "[*] Directory already exists", "")
  }
  log_msg(pVerbose, stringi::stri_paste("[*]   ", pDirectory), "")
}


#' Decompresses a file.
#'
#' @details Decompresses a compressed file.
#' @param pVerbose A boolean (TRUE or FALSE).
#' @param pFile.name filename (including the full path) of the file to be decompressed.
#' @param pCompress.type It indicates the type of compressed file.
#' @param pDelete.file A boolean (TRUE or FALSE) indicating if the compressed file has to be deleted after being decompressed.
#' @return nothing.
#' @export
#'
#' @examples --
decompress_file <- function (pVerbose, pFile.name, pCompress.type, pDelete.file) {
  if (pCompress.type == "gz") {
    log_msg(pVerbose, "[*] Decompressing file: ", "")
    log_msg(pVerbose, paste("[*]   ", pFile.name), "")
    R.utils::gunzip(pFile.name)
    log_msg(pVerbose, paste("[*] File decompressed. Compression type: ", pCompress.type), "")
  }
  if (pDelete.file) {
    log_msg(pVerbose, paste("[*] File ", pFile.name, " removed"), "")
    rm(pFile.name)
  }
}


#' Decompresses a compressed file which contains multiple files
#'
#' @details Decompresses a compressed file which contains multiple files.
#' @param pVerbose A boolean (TRUE or FALSE).
#' @param pFile.name filename (including the full path) of the file to be decompressed.
#' @param pCompress.type It indicates the type of compressed file.
#' @param pDelete.file A boolean (TRUE or FALSE) indicating if the compressed file has to be deleted after being decompressed.
#' @return nothing.
#' @export
#'
#' @examples --
decompress_multifile <- function (pVerbose, pFile.name, pCompress.type, pDelete.file) {
  if (pCompress.type == "zip") {
    log_msg(pVerbose, paste("[*] Decompressing ", pCompress.type, " file: "), "")
    log_msg(pVerbose, paste("[*]   ", pFile.name), "")
    vzipfiles <- unzip(zipfile = pFile.name, list = T)
    log_msg(pVerbose, paste("[*] File decompressed. Compression type: ", pCompress.type), "")
  }
  if (pDelete.file) {
    log_msg(pVerbose, paste("[*] File ", pFile.name, " removed"), "")
    rm(pFile.name)
  }
  return(vzipfiles)
}


#' Download data. Descarreguem un dataset de la web UNSV ADFA amb informació sobre diferents tipus d'atacs
#' El fitxer original, RAW, amb els paquets de xarxa continugts al dataset UNSW-NB 15 ha sigut generat per
#' una eina, IXIA PerfectStorm, en un dels laboratoris del Australian Centre for Cyber Security (ACCS)
#' amb l'objectiu de generar un model híbrid entre les activitats normals en la realitat i els
#' comportaments d'atacs sintètics.
#' L'eina Tcpdump s'ha utilitzat per capturar 100GB de traffic (en Pcap files de Wireshark, p.e.)
#' El dataset conté 9 tipus diferents d'atacs:
#' - Fuzzers
#' - Analysis
#' - Backdoors
#' - DoS
#' - Exploits
#' - Generic
#' - Reconnaissance
#' - Shellcode
#' - Worms.
#' També s'han utilitzat eines com The Argus o Bro-IDS tools o desenvolupat fins a 12 algorismes per generar
#' un total de 49 funcionalitats.
#'
#' Els fitxers que podriem descarregar són:
#' Per a training: 175,341 registres, tan de paquets normals com d'atacs
#' Per a testing: 82,332 registres, tan de paquets normals com d'atacs
#'
#' En aquest projecte, ens centrarem en els atacs per denegació de servei, DoS
#' @details Descarreguem un dataset de UNSV ADFA.
#' @return Retorna una primera versió del dataframe corresponent als atacs.
#' @export
#'
#' @examples --
download_data <- function (pVerbose, pOverwrite.data, pTesting) {
  local.data.folder <- file.path(getwd(), "data")
  local.data.filename <- "dataset.csv"

# Initial Setup
  log_msg(pVerbose, "[*] Initial setup", "")
# En cas que triem el fitxer per a testejar
  if (pTesting) {
    log_msg(pVerbose, "[*]   We're testing", "")
    src.url.fullpath <- "https://www.unsw.adfa.edu.au/unsw-canberra-cyber/cybersecurity/ADFA-NB15-Datasets/a%20part%20of%20training%20and%20testing%20set/UNSW_NB15_testing-set.csv"
  }
# En cas que triem el fitxer per a entrenar
  else {
    log_msg(pVerbose, "[*]   We're NOT testing", "")
    src.url.fullpath <- "https://www.unsw.adfa.edu.au/unsw-canberra-cyber/cybersecurity/ADFA-NB15-Datasets/UNSW-NB15_1.csv"
  }

  tini <- Sys.time()
#log_msg(pVerbose, paste("[*] Initial setup ", tini), "")
  create_directory(pVerbose, local.data.folder)
  local.data.fullpath <- file.path(local.data.folder, local.data.filename)

# Descargar datos en crudo
  if (pOverwrite.data | !file.exists(local.data.fullpath)) {
    log_msg(pVerbose, "[*]   Raw data file has to be downloaded.", "")
    log_msg(pVerbose, "[*]   Downloading RAW data...", "")
    download.file(url = src.url.fullpath, destfile = local.data.fullpath)
  }
  else {
    log_msg(pVerbose, "[*]   Raw data file already exists. It will NOT be downloaded again.", "")
  }

  log_msg(pVerbose, "[*] Loading data on the DataFrame...", "")
  df.attacks <- read.csv(local.data.fullpath, stringsAsFactors = FALSE)

## Corregim el nom de la primera columna perquè es carrega incorrectament del fitxer (???)
  colnames(df.attacks)[colnames(df.attacks) == "ï..srcip"] <- "srcip"

  log_msg(pVerbose, "[*] Data loaded onto the DataFrame.", "")

  return(df.attacks)
}


#' Download maxmind. Descarreguem de la web maxmind.com una base de dades amb la geolocalització de les
#' IPs públiques mundials que té registrades.
#'
#' @details Descarrega un fitxer maxmind de la web de maxmind.
#' @return Obtenim un dataframe amb la posició GPS de les IPs resgistrades.
#' @export
#'
#' @examples --
download_maxmind <- function (pVerbose, pOverwrite.data, pTesting) {
  local.data.folder <- file.path(getwd(), "data")
  local.data.filename <- "maxmind.zip"
  local.data.fullpath <- file.path(local.data.folder, local.data.filename)
  src.url <- "https://geolite.maxmind.com/download/geoip/database/GeoLite2-City-CSV.zip"
  output.file <- "geoftps.rds"

  log_msg(pVerbose, "[*] ***** The parameter pTesting is ignored. It's only kept for future utilization *****")

  if (pOverwrite.data | !file.exists(local.data.fullpath)) {
    log_msg(pVerbose, "[*]   Read RAW data from MaxMind", "")
    local.data.fullpath <- file.path(local.data.folder, local.data.filename)
    download.file(url = src.url, destfile = local.data.fullpath)
  } else {
    log_msg(pVerbose, "[*]   MaxMind file already exists. It will NOT be downloaded again.", "")
  }

  log_msg(pVerbose, "[*]     Decompressing files...", "")
  zipfiles <- decompress_multifile(pVerbose, local.data.fullpath, "zip", TRUE)
  maxmind.source <- zipfiles$Name[grep(pattern = ".*GeoLite2-City-Blocks-IPv4.csv", x = zipfiles$Name)]
  log_msg(pVerbose, "[*]     Unzipping file...", "")
  unzip(zipfile = local.data.fullpath, exdir = local.data.folder, files = maxmind.source)
  maxmind.source <- file.path(getwd(), "data", maxmind.source)

  log_msg(pVerbose, "[*]     Loading file into a dataframe...", "")
  df.maxmind <- read.csv(maxmind.source, stringsAsFactors = FALSE)

  log_msg(pVerbose, "[*]     Adding range boundaries to each network...", "")
  df.maxmind <- cbind(df.maxmind, iptools::range_boundaries(df.maxmind$network))
  df.maxmind$rowname <- as.integer(row.names(df.maxmind))

  log_msg(pVerbose, "[*]     Removing column Range, as it's duplicated...", "")
  df.maxmind$range <- NULL

  log_msg(pVerbose, "[*]     Removing other columns that are not needed...", "")
  df.maxmind$geoname_id <- NULL
  df.maxmind$registered_country_geoname_id <- NULL
  df.maxmind$represented_country_geoname_id <- NULL
  df.maxmind$postal_code <- NULL

  log_msg(pVerbose, "[*]     Cleaning local variables...", "")
  rm(local.data.fullpath, zipfiles)

  log_msg(pVerbose, "[*]     Cleanup finished on MaxMind dataframe", "")
  return(df.maxmind)
}


#' Generem una part del dataframe, reduït a un cert nombre de files. El nombre de files es genera
#' de manera al·leatòria a partir de pData.
#'
#' @details Selects (randomly) a number of rows from pData.
#' @return Obtenim el df que busquem amb un nombre de files indicat per pNumRows provinent de pData.
#' @export
#'
#' @examples --
get_subset_rows <- function (pVerbose, pData, pNumRows) {

  log_msg(pVerbose, paste("[*] Generating a random selection of", pNumRows, "records..."), "")

  muestra <- sample(1:nrow(pData), pNumRows)
  df <- pData[muestra,]

  return(df)
}


#' Afegim columnes per tal de fer la recerca a Maxmind.
#'
#' @details Afegim les columnes necessàries per tal de fer la recerca de les IPs del dataset al dataset de Maxmind.
#' @return Obtenim el DataFrame que busquem amb columnes addicionals.
#' @export
#'
#' @examples --
add_columns_for_lookup <- function (pVerbose, pDataFrame) {

  if (pVerbose) print("[*] Adding columns to dataframe, for looking up with MaxMind...")

  pDataFrame$srcip_num <- iptools::ip_to_numeric(pDataFrame$srcip)
  pDataFrame$dstip_num <- iptools::ip_to_numeric(pDataFrame$dstip)

  return (pDataFrame)
}


#' Lookup to Maxmind to get geolocation (Lat, Lon, Accuracy) of IP_Ori and IP_Dest.
#'
#' @details Adds the geolocation columns into the DataFrame, by looking up to Maxmind through srcip_num and dstip_num.
#' @return DataFrame with additional columns (Lat, Lon, Accuracy) for IP_Ori and IP_Dest
#' @export
#'
#' @examples --
lookup_to_maxmind <- function (pVerbose, pAttacks, pMaxMind) {

  if (pVerbose) print("[*] Adding geolocation columns to dataframe, by looking up to MaxMind...")

  df.attacks$sloc <- sapply(df.attacks$srcip_num,
                          function(ip)
                            which((ip >= df.maxmind$min_numeric) &
                                    (ip <= df.maxmind$max_numeric)))

  return (pAttacks)
}


##################################################
##################################################
#' TBD.
#'
#' @details TBD.
#' @return TBD
#' @export
#'
#' @examples --
addIPgeolocation <- function(ips = "", df.maxmind = data.frame(), boost = FALSE) {
  # Para geolocalizar una IP en un rango comprobaremos si está entre la primera
  # y la ultima ip de cada rango en MaxMind.

  # if (all(iptools::is_ipv4(ips))) {
  #   ips <- iptools::ip_to_numeric(ips)
  # }
  df <- data.frame(ip = as.numeric(ips))

  if (boost) {
    # Usamos multiples cpu's para geolocalizar IPs en rangos
    no_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(no_cores)
    parallel::clusterExport(cl, "df.maxmind", envir = environment())
    df$maxmind.rowname <- sapply(ips,
                                 function(ip)
                                   which((ip >= df.maxmind$min_numeric) &
                                           (ip <= df.maxmind$max_numeric)))
    parallel::stopCluster(cl)
    rm(cl, no_cores)
  } else {
    df$maxmind.rowname <- sapply(ips,
                                 function(ip)
                                   which((ip >= df.maxmind$min_numeric) &
                                           (ip <= df.maxmind$max_numeric)))
  }

  df <- dplyr::left_join(df, df.maxmind, by = c("maxmind.rowname" = "rowname"))

  df <- dplyr::select(df, ip, network, latitude, longitude, accuracy_radius,
                      is_anonymous_proxy, is_satellite_provider)

  return(df)
}


#' TBD.
#'
#' @details TBD.
#' @return TBD
#' @export
#'
#' @examples --
find_geolocation_data <- function(pVerbose, df.attacks, df.maxmind) {

  log_msg(pVerbose, "[*] Adding Geolocation data...", "")
  df <- df.attacks
  scope = nrow(df)
  geo.src <- addIPgeolocation(ips = df$srcip,
                              df.maxmind = df.maxmind,
                              boost = scope > 1000)
  geo.dst <- addIPgeolocation(ips = df$dstip,
                              df.maxmind = df.maxmind,
                              boost = scope > 1000)

  log_msg(pVerbose, "[*]     Tidy dataframe", "")
  names(geo.src) <- paste("src_", names(geo.src), sep = "")
  names(geo.dst) <- paste("dst_", names(geo.dst), sep = "")
  # Preparamos el data frame
  df <- dplyr::bind_cols(df, geo.src, geo.dst)

  log_msg(pVerbose, "[*]     Selecting only the required fields...", "")
  df <- dplyr::select(df, timestamp_ts, ttl,
                      saddr, sport, src_network, src_latitude, src_longitude,
                      src_accuracy_radius, src_is_anonymous_proxy, src_is_satellite_provider,
                      daddr, dport, dst_network, dst_latitude, dst_longitude,
                      dst_accuracy_radius, dst_is_anonymous_proxy, dst_is_satellite_provider)

  log_msg(pVerbose, "[*]     Columns filtered. Returning dataframe.", "")
  return(df)
}

#' Main function, which calls the rest of functions
#'
#' @details Main function, funció principal que crida la resta de funcions en l'rodre següent:
#' - download_data
#'
#'
#' I utilitza, addicionalment, les següents funcions auxiliars:
#' - log_msg. Imprimeix per consola o a fitxer aquells missatges que el programador troba adequats a l'hora d'executar el codi
#' - create_directory. Crea, si és necessari, un directori a l'espai de treball per tal de guardar-hi diferents arxius
#' - decompress_file. Descomprimeix un fitxer que conté un arxiu i elimina el fitxer original
#' - decompress_multifile. Descomprimeix un fitxer que conté més d'un arxiu i elimina el fitxer original
#' @return Retorna un resum del dataframe final
#' @export
#'
#' @examples --
main <- function () {
  verbose <- TRUE
  overwrite.data <- FALSE
  testing <- TRUE
  scope_test <- 1
  scope_prod <- 10

  df.attacks <- download_data(verbose, overwrite.data, testing)

  df.maxmind <- download_maxmind(verbose, overwrite.data, testing)

  if (testing) {
    log_msg(verbose, paste("[*] We are testing. Use a reduced scope of", scope_test, "records"), "")
    scope <- scope_test
  } else {
    log_msg(verbose, paste("[*] This is not a test. Use a scope of", scope_prod, "records"), "")
    scope <- scope_prod
  }
  df.attacks <- get_subset_rows(verbose, df.attacks, scope)

  df.attacks <- add_columns_for_lookup(verbose, df.attacks)

  df <- find_geolocation_data(verbose, df.attacks, df.maxmind)

  summary(df)
}

