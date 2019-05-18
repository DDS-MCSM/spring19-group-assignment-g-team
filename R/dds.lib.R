#' Logs a message to the Log. If a filename is passed as parameter, it will also print the message into the specified log file.
#'
#' @details It's very useful to log messages through a function like this, as it will save you time when developing. You will have to just change the name of the variable 'pVerbose' to log or stop logging messages.
#' @param pVerbose A boolean (TRUE or FALSE).
#' @param pMessage The message you want to show in the console.
#' @return nothing.
#' @export
#'
#' @examples
log_msg <- function(pVerbose, pMessage, pLogFile) {
  if (pVerbose) {
    print(pMessage)
    if (nchar(pLogFile) > 0) {
      ######## Log to file
    }
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
#' @examples
create_directory <- function(pVerbose, pDirectory) {
  if (!dir.exists(pDirectory)) {
    log_msg(pVerbose, "[*] Create directory")
    dir.create(pDirectory)
  }
  else {
    log_msg(pVerbose, "[*] Directory already exists")
  }
  log_msg(pVerbose, stringi::stri_paste("[*]   ", pDirectory))
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
#' @examples
decompress_data <- function (pVerbose, pFile.name, pCompress.type, pDelete.file) {
  if (pCompress.type == "gz") {
    log_msg(pVerbose, "[*] Decompressing file: ")
    log_msg(pVerbose, paste("[*]   ", pFile.name))
    R.utils::gunzip(pFile.name)
    log_msg(pVerbose, paste("[*] File decompressed. Compression type: ", pCompress.type))
  }
  if (pDelete.file) {
    log_msg(pVerbose, paste("[*] File ", pFile.name, " removed"))
    rm(pFile.name)
  }
}
