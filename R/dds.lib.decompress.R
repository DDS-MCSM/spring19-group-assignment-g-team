
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

