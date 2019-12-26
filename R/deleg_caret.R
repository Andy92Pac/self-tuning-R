#' deleg_caret
#' This function delegate the execution of the passed command to iExec
#'
#' @param cmd a command to execute
#' @param encryption a boolean indicating if encryption is required
#' @param params either a list of dataframe with the different values to have the function to execute
#' @param test_data a dataset to test the created model against to evaluate it
#'
#' @return the function returns the ids of the deal and tasks created
#'
#' @importFrom encryptr genkeys encrypt_file
#' @importFrom httr POST GET write_disk content upload_file add_headers
#' @importFrom rjson fromJSON toJSON
#'
#' @export
deleg_caret <- function(cmd, params = NULL, encryption = T, test_data = NULL) {

  if(!is.null(params)) {
    if(is.list(params)) {
      if(!is.data.frame(params)) {
        params = expand.grid(params)
      }
      params = toJSON(params)
    }
    else {
      print("params is not a list")
      return()
    }
  }

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

  save(values.list, test_data, file = filename)

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

  opts = list(
    encryption=encryption,
    evaluate=ifelse(is.null(test_data), FALSE, TRUE)
  )

  body = list(
    command=paste(deparse(str), collapse = " "),
    rdata=upload_file(filename),
    params=params,
    opts=toJSON(opts))

  if(encryption) {
    body[['publicKey']]=upload_file('id_rsa.pub')
  }

  r = POST('localhost:3000/api/jobs',
           add_headers(api_key=pkg.env$apiKey),
           body = body
  )

  file.remove(filename)

  c = content(r)

  if(r$status_code == 401 || r$status_code == 400 || r$status_code == 500) {
    print(c$message)
    return(NULL)
  }

  if(c$status) {
    return(c$data)
  }
  else {
    return(NULL)
  }
}
