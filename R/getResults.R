#' Title
#'
#' @param taskId Id of the task to get the results of
#' @param jobId Id of the job to get the results of
#'
#' @return the function returns the results and outputs of the tasks requested
#'
#' @importFrom encryptr decrypt_file
#' @importFrom httr GET add_headers
#' @importFrom R.utils gunzip
#' @importFrom rjson fromJSON
#' @importFrom curl curl_download
#' @importFrom utils read.delim
#'
#' @export
getResults <- function(taskId = NULL, jobId = NULL) {
  resultAvailable = FALSE

  while (resultAvailable == FALSE) {
    if(!is.null(taskId)) {
      r = GET(
        paste('localhost:3000/api/tasks/', taskId, '/file', sep = ''),
        add_headers(api_key=pkg.env$apiKey))
    } else if(!is.null(jobId)) {
      r = GET(
        paste('localhost:3000/api/jobs/', jobId, '/file', sep = ''),
        add_headers(api_key=pkg.env$apiKey))
    } else {
      print("Missing arguments to the function")
      return()
    }

    c = content(r)

    if(!is.null(taskId)) {
      if(c$taskClaimable == TRUE) {
        print("A problem occured during the execution of the task, open an issue on github if you need more informations")
        return()
      }
    }

    resultAvailable = c$resultsAvailable

    if(resultAvailable == TRUE) {
      if(!is.null(taskId)) {
        curl_download(
          url = paste(
            'https://gateway.pinata.cloud/ipfs/',
            c$data$results$result$upload$IpfsHash,
            sep = ''),
          destfile = "result.Rdata.encryptr.bin.gz")

        curl_download(
          url = paste(
            'https://gateway.pinata.cloud/ipfs/',
            c$data$results$output$upload$IpfsHash,
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

        result = loadRData('result.Rdata')
        output = read.delim(file = 'output')

        results = list(
          result = result,
          output = output)

        file.remove('result.Rdata',
                    'result.Rdata.encryptr.bin',
                    'output',
                    'output.encryptr.bin')
      }

      else {
        results = list()

        for (task in c$data$tasks) {
          if(task$taskClaimable == FALSE) {
            curl_download(
              url = paste(
                'https://gateway.pinata.cloud/ipfs/',
                task$results$result$upload$IpfsHash,
                sep = ''),
              destfile = "result.Rdata.encryptr.bin.gz")

            curl_download(
              url = paste(
                'https://gateway.pinata.cloud/ipfs/',
                task$results$output$upload$IpfsHash,
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

            result = loadRData('result.Rdata')
            output = read.delim(file = 'output')

            results[[task$index]] = list(
              result = result,
              output = output)

            file.remove('result.Rdata',
                        'result.Rdata.encryptr.bin',
                        'output',
                        'output.encryptr.bin')

          }
        }
      }

      return(results)
    }
    else {
      Sys.sleep(time = 20)
    }
  }
}

loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}
