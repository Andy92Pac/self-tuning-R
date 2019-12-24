#' Title
#'
#' @param taskId Id of the task to get the results of
#'
#' @importFrom encryptr decrypt_file
#' @importFrom httr GET
#' @importFrom R.utils gunzip
#' @importFrom rjson fromJSON
#' @importFrom curl curl_download
#'
#' @export
getResults <- function(taskId) {
  resultAvailable = FALSE

  while (resultAvailable == FALSE) {
    r = GET(paste('localhost:3000/api/tasks/', taskId, '/file', sep = ''))
    c = content(r)

    resultAvailable = c$resultsAvailable

    if(resultAvailable == TRUE) {
      curl_download(
        url = paste(
          'https://gateway.pinata.cloud/ipfs/',
          c$data$result$upload$IpfsHash,
          sep = ''),
        destfile = "result.Rdata.encryptr.bin.gz")

      curl_download(
        url = paste(
          'https://gateway.pinata.cloud/ipfs/',
          c$data$output$upload$IpfsHash,
          sep = ''),
        destfile = "output.encryptr.bin.gz")

      gunzip('result.Rdata.encryptr.bin.gz')
      gunzip('output.encryptr.bin.gz')

      decrypt_file('result.Rdata.encryptr.bin',
                   file_name = 'result.Rdata',
                   private_key_path = 'id_rsa')

      decrypt_file('output.encryptr.bin',
                   file_name = 'output',
                   private_key_path = 'id_rsa')

      load('result.Rdata', envir = .GlobalEnv)

      file.remove('result.Rdata',
                  'result.Rdata.encryptr.bin',
                  'output',
                  'output.encryptr.bin')
    }
    else {
      Sys.sleep(time = 20)
    }
  }
}
