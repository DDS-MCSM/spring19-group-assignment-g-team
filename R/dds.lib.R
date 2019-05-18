
# #####################
# Tot això hauria de venir del package, una vegada que sapiguem com es crea.
# Carregaríem el package fent:
#     library(dds.practica1)
# #####################

log.msg <- function(pVerbose, pMsg) {
  if (pVerbose) print(pMsg)
}

create.directory <- function(pVerbose, pDirectory) {
  if (!dir.exists(pDirectory)) {
    log.msg(pVerbose, "[*] Create directory")
    dir.create(pDirectory)
  }
  else {
    log.msg(pVerbose, "[*] Directory already exists")
  }
  log.msg(pVerbose, stringi::stri_paste("[*]   ", pDirectory))
}

decompress.data <- function (pVerbose, pFile.name, pCompress.type, pDelete.file) {
  if (pCompress.type == "gz") {
    log.msg(pVerbose, "[*] Decompressing file: ")
    log.msg(pVerbose, paste("[*]   ", pFile.name))
    R.utils::gunzip(pFile.name)
    log.msg(pVerbose, paste("[*] File decompressed. Compression type: ", pCompress.type))
  }
  if (pDelete.file) {
    log.msg(pVerbose, paste("[*] File ", pFile.name, " removed"))
    rm(pFile.name)
  }
}


# #####################
# Fins aqui
# #####################

