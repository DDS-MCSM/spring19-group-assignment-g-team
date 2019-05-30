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
  local.data.filename <- "mendeley.csv"

  create_directory(pVerbose, local.data.folder)
  local.data.fullpath <- file.path(local.data.folder, paste(local.data.filename))

  # Initial Setup
  log_msg(pVerbose, "[*] Initial setup", "")

  log_msg(pVerbose, "[*]   Loading existing file on the DataFrame...", "")
  df.attacks <- read.csv(local.data.fullpath, stringsAsFactors = FALSE, sep=";")
  log_msg(pVerbose, "[*]   Adding header to DataFrame...", "")
  colnames(df.attacks) <- c("timestamp", "srcip","sport","dstip","dsport","packets","proto")




# local.data.folder <- file.path(getwd(), "data")
# local.data.filename <- "dataset"
#
# create_directory(pVerbose, local.data.folder)
# local.data.fullpath <- file.path(local.data.folder, paste(local.data.filename, "1", ".csv", sep=""))
#
# # Initial Setup
# log_msg(pVerbose, "[*] Initial setup", "")
#
# # Descargar datos en crudo
# if (pOverwrite.data | !file.exists(local.data.fullpath)) {
#   log_msg(pVerbose, "[*]   Raw data file has to be downloaded.", "")
#   src.url.fullpath <- "https://www.unsw.adfa.edu.au/unsw-canberra-cyber/cybersecurity/ADFA-NB15-Datasets/UNSW-NB15_1.csv"
#   log_msg(pVerbose, "[*]   Downloading RAW data...", "")
#   download.file(url = src.url.fullpath, destfile = local.data.fullpath)
#   log_msg(pVerbose, "[*]   Loading fresh data on the DataFrame...", "")
# }
# else {
#   log_msg(pVerbose, "[*]   Raw data file already exists. It will NOT be downloaded again.", "")
#   log_msg(pVerbose, "[*]   Loading existing file on the DataFrame...", "")
# }
#
# df.attacks <- read.csv(local.data.fullpath, stringsAsFactors = FALSE)
# log_msg(pVerbose, "[*]   Adding header to DataFrame...", "")
# colnames(df.attacks) <- c("srcip","sport","dstip","dsport","proto","state","dur","sbytes","dbytes","sttl","dttl","sloss","dloss","service","Sload","Dload","Spkts","Dpkts","swin","dwin","stcpb","dtcpb","smeansz","dmeansz","trans_depth","res_bdy_len","Sjit","Djit","Stime","Ltime","Sintpkt","Dintpkt","tcprtt","synack","ackdat","is_sm_ips_ports","ct_state_ttl","ct_flw_http_mthd","is_ftp_login","ct_ftp_cmd","ct_srv_src","ct_srv_dst","ct_dst_ltm","ct_src_ltm","ct_src_dport_ltm","ct_dst_sport_ltm","ct_dst_src_ltm","attack_cat","Label")
# log_msg(pVerbose, "[*]   Keeping only Dos records...", "")
# df.attacks <- df.attacks[df.attacks$attack_cat == "DoS", ]
#
# if (!pTesting) {
#   local.data.fullpath <- file.path(local.data.folder, paste(local.data.filename, "2", ".csv", sep=""))
#   if (pOverwrite.data | !file.exists(local.data.fullpath)) {
#     log_msg(pVerbose, "[*]   We're NOT testing. So downloading more files...", "")
#     src.url.fullpath <- "https://www.unsw.adfa.edu.au/unsw-canberra-cyber/cybersecurity/ADFA-NB15-Datasets/UNSW-NB15_2.csv"
#     download.file(url = src.url.fullpath, destfile = local.data.fullpath)
#     log_msg(pVerbose, "[*] Loading second set of data on the DataFrame...", "")
#   }
#   else {
#     log_msg(pVerbose, "[*]   Raw data file already exists. It will NOT be downloaded again.", "")
#     log_msg(pVerbose, "[*]   Loading existing file on the DataFrame...", "")
#   }
#   d2 <- read.csv(local.data.fullpath, stringsAsFactors = FALSE)
#   log_msg(pVerbose, "[*]   Adding header to temp DataFrame...", "")
#   colnames(d2) <- c("srcip","sport","dstip","dsport","proto","state","dur","sbytes","dbytes","sttl","dttl","sloss","dloss","service","Sload","Dload","Spkts","Dpkts","swin","dwin","stcpb","dtcpb","smeansz","dmeansz","trans_depth","res_bdy_len","Sjit","Djit","Stime","Ltime","Sintpkt","Dintpkt","tcprtt","synack","ackdat","is_sm_ips_ports","ct_state_ttl","ct_flw_http_mthd","is_ftp_login","ct_ftp_cmd","ct_srv_src","ct_srv_dst","ct_dst_ltm","ct_src_ltm","ct_src_dport_ltm","ct_dst_sport_ltm","ct_dst_src_ltm","attack_cat","Label")
#   log_msg(pVerbose, "[*]   Keeping only Dos records from temp Dataframe...", "")
#   d2 <- d2[d2$attack_cat == "DoS", ]
#   log_msg(pVerbose, "[*]   Appending 2nd set of data on the DataFrame...", "")
#   df.attacks <- rbind(df.attacks, d2)

    # local.data.fullpath <- file.path(local.data.folder, paste(local.data.filename, "3", ".csv", sep=""))
    # if (pOverwrite.data | !file.exists(local.data.fullpath)) {
    #   src.url.fullpath <- "https://www.unsw.adfa.edu.au/unsw-canberra-cyber/cybersecurity/ADFA-NB15-Datasets/UNSW-NB15_3.csv"
    #   download.file(url = src.url.fullpath, destfile = local.data.fullpath)
    #   log_msg(pVerbose, "[*] Loading third set of data on the DataFrame...", "")
    # }
    # else {
    #   log_msg(pVerbose, "[*]   Raw data file already exists. It will NOT be downloaded again.", "")
    #   log_msg(pVerbose, "[*]   Loading existing file on the DataFrame...", "")
    # }
    # d2 <- read.csv(local.data.fullpath, stringsAsFactors = FALSE)
    # log_msg(pVerbose, "[*]   Adding header to temp DataFrame...", "")
    # colnames(d2) <- c("srcip","sport","dstip","dsport","proto","state","dur","sbytes","dbytes","sttl","dttl","sloss","dloss","service","Sload","Dload","Spkts","Dpkts","swin","dwin","stcpb","dtcpb","smeansz","dmeansz","trans_depth","res_bdy_len","Sjit","Djit","Stime","Ltime","Sintpkt","Dintpkt","tcprtt","synack","ackdat","is_sm_ips_ports","ct_state_ttl","ct_flw_http_mthd","is_ftp_login","ct_ftp_cmd","ct_srv_src","ct_srv_dst","ct_dst_ltm","ct_src_ltm","ct_src_dport_ltm","ct_dst_sport_ltm","ct_dst_src_ltm","attack_cat","Label")
    # log_msg(pVerbose, "[*]   Keeping only DoS records from temp Dataframe...", "")
    # d2 <- d2[d2$attack_cat == "DoS", ]
    # log_msg(pVerbose, "[*]   Appending 3rd set of data on the DataFrame...", "")
    # df.attacks <- rbind(df.attacks, d2)
    #
    # local.data.fullpath <- file.path(local.data.folder, paste(local.data.filename, "4", ".csv", sep=""))
    # if (pOverwrite.data | !file.exists(local.data.fullpath)) {
    #   src.url.fullpath <- "https://www.unsw.adfa.edu.au/unsw-canberra-cyber/cybersecurity/ADFA-NB15-Datasets/UNSW-NB15_4.csv"
    #   download.file(url = src.url.fullpath, destfile = local.data.fullpath)
    #   log_msg(pVerbose, "[*] Loading fourth set of data on the DataFrame...", "")
    # }
    # else {
    #   log_msg(pVerbose, "[*]   Raw data file already exists. It will NOT be downloaded again.", "")
    #   log_msg(pVerbose, "[*]   Loading existing file on the DataFrame...", "")
    # }
    # d2 <- read.csv(local.data.fullpath, stringsAsFactors = FALSE)
    # log_msg(pVerbose, "[*]   Adding header to temp DataFrame...", "")
    # colnames(d2) <- c("srcip","sport","dstip","dsport","proto","state","dur","sbytes","dbytes","sttl","dttl","sloss","dloss","service","Sload","Dload","Spkts","Dpkts","swin","dwin","stcpb","dtcpb","smeansz","dmeansz","trans_depth","res_bdy_len","Sjit","Djit","Stime","Ltime","Sintpkt","Dintpkt","tcprtt","synack","ackdat","is_sm_ips_ports","ct_state_ttl","ct_flw_http_mthd","is_ftp_login","ct_ftp_cmd","ct_srv_src","ct_srv_dst","ct_dst_ltm","ct_src_ltm","ct_src_dport_ltm","ct_dst_sport_ltm","ct_dst_src_ltm","attack_cat","Label")
    # log_msg(pVerbose, "[*]   Keeping only DoS records from temp Dataframe...", "")
    # d2 <- d2[d2$attack_cat == "DoS", ]
    # log_msg(pVerbose, "[*]   Appending 4th set of data on the DataFrame...", "")
    # df.attacks <- rbind(df.attacks, d2)
#    rm(d2)
#  }

  # ## Corregim el nom de la primera columna perquè es carrega incorrectament del fitxer (???)
  # colnames(df.attacks)[colnames(df.attacks) == "ï..srcip"] <- "srcip"
  # log_msg(pVerbose, "[*] Data loaded onto the DataFrame.", "")

  saveRDS(object = df.attacks, file = file.path(local.data.folder, "attacks2.rds"))
  log_msg(pVerbose, "[*] Source Data saved in RDS file.", "")

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

  if ((!pOverwrite.data) & file.exists(file.path(local.data.folder, "maxmind.rds"))) {
    log_msg(pVerbose, "[*]   MaxMind RDS file exists. Loading... ", "")
    df.maxmind <- readRDS(file.path(local.data.folder, "maxmind.rds"))
  } else {
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

    #file.remove(maxmind.source)
    unlink(file.path(local.data.folder, "GeoLite2-City-CSV_*"), recursive = T)

    log_msg(pVerbose, "[*]     Adding range boundaries to each network...", "")
    df.maxmind <- cbind(df.maxmind, iptools::range_boundaries(df.maxmind$network))
    df.maxmind$rowname <- as.numeric(row.names(df.maxmind))  #as.integer

    log_msg(pVerbose, "[*]     Removing column Range, as it's duplicated...", "")
    df.maxmind$range <- NULL

    log_msg(pVerbose, "[*]     Removing other columns that are not needed...", "")
    df.maxmind$geoname_id <- NULL
    df.maxmind$registered_country_geoname_id <- NULL
    df.maxmind$represented_country_geoname_id <- NULL
    df.maxmind$postal_code <- NULL
    log_msg(pVerbose, "[*]     Cleanup finished on MaxMind dataframe", "")

    saveRDS(object = df.maxmind, file = file.path(local.data.folder, "maxmind.rds"))
    log_msg(pVerbose, "[*] MaxMind Data saved in RDS file.", "")
  }

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
get_subset_rows <- function (pVerbose, pData, pNumRows, pSeed) {

  set.seed(pSeed)

  log_msg(pVerbose, paste("[*] Generating a random selection of", pNumRows, "records..."), "")
  muestra <- sample(1:nrow(pData), pNumRows)

  log_msg(pVerbose, "[*] Getting the subset of records...", "")
  df <- pData[muestra, ]

  log_msg(pVerbose, "[*] Done. Returning subset.", "")
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



#' Añadir la Geolocalización de las IPs al dataframe
#'
#' @details Añadimos la geolocalización de las IPS al dataframe definitivo a partir del dataset
#'          de maxmind
#' @return Dataframe final
#' @export
#'
#' @examples
#' \dontrun{
#' geoips <- addIPgeolocation(ips = c("8.8.8.8", "147.81.23.1"),
#'                            df.maxmind = download.maxmind())
#' }
addIPgeolocation <- function(ips = "", df.maxmind = data.frame(), boost = FALSE) {
  # Para geolocalizar una IP en un rango comprobaremos si está entre la primera
  # y la ultima ip de cada rango en MaxMind.

  if (all(iptools::is_ipv4(ips))) {
    ips <- iptools::ip_to_numeric(ips)
  }
  df <- data.frame(ip = as.numeric(ips))

  if (boost) {
    # Usamos multiples cpu's para geolocalizar IPs en rangos
    no_cores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(no_cores)
    parallel::clusterExport(cl, "df.maxmind", envir = environment())
    df$maxmind.rowname <- as.numeric(sapply(ips,
                                 function(ip)
                                   which((ip >= df.maxmind$min_numeric) &
                                           (ip <= df.maxmind$max_numeric))))
    parallel::stopCluster(cl)
    rm(cl, no_cores)
  } else {
    df$maxmind.rowname <- as.numeric(sapply(ips,
                                 function(ip)
                                   which((ip >= df.maxmind$min_numeric) &
                                           (ip <= df.maxmind$max_numeric))))
  }

  df <- dplyr::left_join(df, df.maxmind, by = c("maxmind.rowname" = "rowname"))

  df <- dplyr::select(df, ip, network, latitude, longitude, accuracy_radius,
                      is_anonymous_proxy, is_satellite_provider)

  return(df)
}


#' Buscamos la información GPS de cada IP y la añadimos al dataframe
#'
#' @details TBD.
#' @return Dataframe final definitivo con las columnas que nos interesan
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
  df <- dplyr::select(df, timestamp,
                      srcip, sport, src_latitude, src_longitude, src_accuracy_radius,
                      dstip, dsport, dst_latitude, dst_longitude, dst_accuracy_radius)

  df <- df[!is.na(df$src_latitude), ]
  df <- df[!is.na(df$dst_latitude), ]

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
main <- function (verbose = TRUE, overwrite.data = FALSE, testing = FALSE, scope.test = 10) {
  # verbose <- TRUE
  # overwrite.data <- FALSE
  # testing <- FALSE
  # scope.test <- 10
  local.data.folder <- file.path(getwd(), "data")

  df.attacks <- download_data(verbose, overwrite.data, testing)

  df.maxmind <- download_maxmind(verbose, overwrite.data, testing)

  if (testing) {
    log_msg(verbose, paste("[*] We are testing. Use a reduced scope of", scope.test, "records"), "")
    df.attacks <- get_subset_rows(verbose, df.attacks, scope.test, 666)
  } else {
    log_msg(verbose, "[*] This is not a test. Using full scope", "")
  }

  df.attacks <- add_columns_for_lookup(verbose, df.attacks)

  df <- find_geolocation_data(verbose, df.attacks, df.maxmind)

  summary(df)

  saveRDS(object = df, file = file.path(local.data.folder, "results.rds"))
  log_msg(verbose, "[*] Results DataFrame saved in RDS file.", "")

  return(df)
}
#' Estudis varis

#'contaipdesti <- dplyr::count(results, dstip, sort = TRUE)
#'contaiporigen <- dplyr::count(results, srcip, sort = TRUE)
#'contaportdesti <- dplyr::count(results, dsport, sort = TRUE)
                                            
#' pinta.map <- ggplot(results, aes(x = results$src_longitude, y = results$src_latitude))
#' pinta.map + geom_polygon(fill = "white", colour = "red")

#' pinta.map + coord_map("rectangular",0)
