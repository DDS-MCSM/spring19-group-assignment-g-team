
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
