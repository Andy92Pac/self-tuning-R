#library(rjson)
#library(encryptr)
#library(httr)
#library(R.utils)

#' delegate
#' This function delegate the execution of the passed command to iExec
#'
#' @param cmd a command to execute
#' @param encryption a boolean indicating if encryption is required
#'
#' @return the function returns the id of the task created
#'
#' @importFrom encryptr genkeys encrypt_file
#' @importFrom httr POST GET write_disk content upload_file
#' @importFrom rjson fromJSON
#'
#' @export
deleg <- function(cmd, encryption = T) {

  if(encryption) {
    if(!file.exists('id_rsa_iexec.pub')) {
      url <- "http://localhost:3000/api/key"
      r = GET(url, write_disk("id_rsa_iexec.pub", overwrite=TRUE))
    }

    if(!file.exists('id_rsa.pub') || !file.exists('id_rsa')) {
      if(file.exists('id_rsa')) {
        try(file.remove('id_rsa'))
      }
      if(file.exists('id_rsa.pub')) {
        try(file.remove('id_rsa.pub'))
      }
      genkeys()
    }
  }

  # get command as a string
  str = substitute(cmd)

  # get elements from the string command as a list
  str.list = as.list(str)

  # get rid of unnamed elements from the string
  elem.list = str.list[names(str.list) != '']

  # get rid of string variables
  elem.list = elem.list[lapply(elem.list, is.character) == F]

  # get rid of boolean variables
  elem.list = elem.list[lapply(elem.list, is.logical) == F]

  # get rid of numeric variables
  elem.list = elem.list[lapply(elem.list, is.numeric) == F]

  values.list = lapply(elem.list, function(e) {
    get(deparse(e))
  })

  names(values.list) = elem.list

  currentTimestamp = as.integer(Sys.time())

  filename = paste(currentTimestamp,
                   'data.Rdata',
                   sep = '')

  save(values.list, file = filename)

  if(encryption) {
    encryptedFileName = paste(currentTimestamp,
                              '.data.Rdata',
                              '.encryptr.bin',
                              sep = '')

    encryptResult = encrypt_file(filename,
                                 crypt_file_name = encryptedFileName,
                                 public_key_path = 'id_rsa_iexec.pub')

    file.remove(filename)
    filename = encryptedFileName
  }

  body = list(
    command=paste(deparse(str), collapse = " "),
    rdata=upload_file(filename))

  if(encryption) {
    body[['publicKey']]=upload_file('id_rsa.pub')
  }

  r = POST('localhost:3000/api/jobs',
           body = body
           )

  file.remove(filename)

  c = content(r)

  if(c$status) {
    return(c$data$job$deal$tasks[[1]])
  }
  else {
    return(NULL)
  }
}
