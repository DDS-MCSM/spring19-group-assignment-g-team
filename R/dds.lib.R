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


#' Download data
#'
#' @details Downloads data from UNSV ADFA.
#' @return nothing.
#' @export
#'
#' @examples --
download_data <- function (pVerbose, pOverwrite.data, pTesting) {
  local.data.folder <- file.path(getwd(), "data")
  local.data.filename <- "dataset.csv"

  # Initial Setup
  log_msg(pVerbose, "[*] Initial setup", "")
  if (pTesting) {
    log_msg(pVerbose, "[*]   We're testing", "")
    src.url.fullpath <- "https://www.unsw.adfa.edu.au/unsw-canberra-cyber/cybersecurity/ADFA-NB15-Datasets/a%20part%20of%20training%20and%20testing%20set/UNSW_NB15_testing-set.csv"
  } else {
    log_msg(pVerbose, "[*]   We're NOT testing", "")
    src.url.fullpath <- "https://www.unsw.adfa.edu.au/unsw-canberra-cyber/cybersecurity/ADFA-NB15-Datasets/UNSW-NB15_1.csv"
  }

  tini <- Sys.time()
  #log_msg(pVerbose, paste("[*] Initial setup ", tini), "")
  create_directory(pVerbose, local.data.folder)
  local.data.fullpath <- file.path(local.data.folder, local.data.filename)

  # Descargar datos en crudo
  log_msg(pVerbose, "[*] Download RAW data", "")
  if (pOverwrite.data | !file.exists(local.data.fullpath)) {
    log_msg(pVerbose, "[*]   Raw data file has to be downloaded. Starting download.....", "")
    download.file(url = src.url.fullpath, destfile = local.data.fullpath)
  } else {
    log_msg(pVerbose, "[*]   Raw data file already exists. It will NOT be downloaded again.", "")
  }

  log_msg(pVerbose, "[*] Load data on the DataFrame.", "")
  df.attacks <- read.csv(local.data.fullpath, stringsAsFactors = FALSE)

  ## Corregim el nom de la primera columna perquè es carrega incorrectament del fitxer (???)
  colnames(df.attacks)[colnames(df.attacks) == "ï..srcip"] <- "srcip"

  return(df.attacks)
}


#' Download maxmind
#'
#' @details Downloads maxmind file MaxMind site.
#' @return Returns a df with the GPS position of each registered network
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
    log_msg(pVerbose, "[*] Read RAW data from MaxMind", "")
    local.data.fullpath <- file.path(local.data.folder, local.data.filename)
    download.file(url = src.url, destfile = local.data.fullpath)
  } else {
    log_msg(pVerbose, "[*]   MaxMind file already exists. It will NOT be downloaded again.", "")
  }

  log_msg(pVerbose, "[*] Decompressing files...", "")
  zipfiles <- decompress_multifile(pVerbose, local.data.fullpath, "zip", TRUE)
  maxmind.source <- zipfiles$Name[grep(pattern = ".*GeoLite2-City-Blocks-IPv4.csv", x = zipfiles$Name)]
  log_msg(pVerbose, "[*] Unzipping file...", "")
  unzip(zipfile = local.data.fullpath, exdir = local.data.folder, files = maxmind.source)
  maxmind.source <- file.path(getwd(), "data", maxmind.source)

  log_msg(pVerbose, "[*] Loading file into a dataframe...", "")
  df.maxmind <- read.csv(maxmind.source, stringsAsFactors = FALSE)

  log_msg(pVerbose, "[*] Adding range boundaries to each network...", "")
  df.maxmind <- cbind(df.maxmind, iptools::range_boundaries(df.maxmind$network))
  df.maxmind$rowname <- as.integer(row.names(df.maxmind))

  log_msg(pVerbose, "[*] Removing column Range, as it's duplicated...", "")
  df.maxmind$range <- NULL

  rm(local.data.fullpath, zipfiles)

  return(df.maxmind)
}


#' Get subset of rows
#'
#' @details Selects (randomly) a number of rows from pData.
#' @return Returns a df with only the requested pNumRows from pData
#' @export
#'
#' @examples --
get_subset_rows <- function (pData, pNumRows) {

  log_msg(pVerbose, paste("[*] Generating a random selection of", pNumRows, "records..."), "")

  muestra <- sample(1:nrow(pData), pNumRows)
  df <- pData[muestra,]

  return(df)
}


#' Add columns for lookup
#'
#' @details Adds the required columns that will be used for the lookup with Maxmind.
#' @return DataFrame with additional columns
#' @export
#'
#' @examples --
add_columns_for_lookup <- function (pVerbose, pDataFrame) {
  if (pVerbose) print("[*] Adding columns to dataframe, for looking up with MaxMind...")
  colnames(pDataFrame)[colnames(pDataFrame) == "ï..srcip"] <- "srcip"
  pDataFrame$srcip_num <- iptools::ip_to_numeric(pData$srcip)
##  pDataFrame$srcip_num <- iptools::ip_to_numeric(pDataFrame$ï..srcip)
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
lookup_to_maxmind <- function (verbose, pData, pMaxMind) {
  if (verbose) print("[*] Adding geolocation columns to dataframe, by looking up to MaxMind...")


  return (pData)
}


#' Main function, which calls the rest of functions
#'
#' @details Main function, which calls the rest of functions.
#' @return Nothing
#' @export
#'
#' @examples --
main <- function () {
  verbose <- TRUE
  overwrite.data <- TRUE
  testing <- TRUE

  df.attacks <- download_data(verbose, overwrite.data, testing)
  df.maxmind <- download_maxmind(verbose, overwrite.data, testing)
  df.subset <- get_subset_rows(df.attacks, 10)

  # A df.attacks, afegir columnes per poder fer lookup amb maximind (transformar IP origen i destí a format numèric)
  df.subset <- add_columns_for_lookup(TRUE, df.subset)

}


#   pData$sloc <- sapply(pData$saddr.num,
#                         function(ip)
#                           which((ip >= pMaxMind$min_numeric) &
#                                   (ip <= pMaxMind$max_numeric)))
#   pData$dloc <- sapply(pData$daddr.num,
#                         function(ip)
#                           which((ip >= pMaxMind$min_numeric) &
#                                   (ip <= pMaxMind$max_numeric)))
#   return(pData)
# }
