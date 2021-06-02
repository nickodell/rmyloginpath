# Taken from the R package 'ini'
# Modified to accept a connection, and to handle 
# quoted keys.
# Copyright (c) 2016 David Valentim Dias
# MIT Licenced - see LICENSE.txt for more


#' Read and parse .ini file to list
#'
#' @param filepath file to parse
#' @param encoding Encoding of filepath parameter, will default to system
#' encoding if not specifield
#'
#' @details Lines starting with '#' or ';' are comments and will not be parsed
#'
#' @seealso \code{\link{write.ini}}
#'
#' @return List with length equivalent to number of [sections], each section is
#' a new list
#'
#' @examples
#' ## Create a new temp ini for reading
#' iniFile <- tempfile(fileext = '.ini')
#'
#' sink(iniFile)
#' cat("; This line is a comment\n")
#' cat("# This one too!\n")
#' cat("[    Hello World]\n")
#' cat("Foo = Bar          \n")
#' cat("Foo1 = Bar=345 \n")
#' sink()
#'
#' ## Read ini
#' checkini <- read.ini(iniFile)
#'
#' ## Check structure
#' checkini
#' checkini$`Hello World`$Foo
#'
#' @export
#'
read.ini <- function(con, encoding = getOption("encoding")) {

  index <- function(x, rune) {
    equalPosition = numeric(1)
    for(pos in 1:nchar(x)) {
      if (strsplit(x, '')[[1]][pos] == rune) {
        equalPosition = pos
        break
      }
    }
    return(equalPosition)
  }
  # internal helper function to find where a character occur

  sectionREGEXP <- '^\\s*\\[\\s*(.+?)\\s*]'
  # match section and capture section name

  keyValueREGEXP <- '^\\s*[^=]+=.+'
  # match "key = value" pattern

  ignoreREGEXP <- '^\\s*[;#]'
  # match lines with ; or # at start

  trim <- function(x) sub('^\\s*(.*?)\\s*$', '\\1', x)
  # amazing lack of trim at old versions of R

  ini <- list()
  # con <- file(filepath, open = 'r', encoding = encoding)
  # on.exit(close(con))

  while ( TRUE ) {

    line <- readLines(con, n = 1, encoding = encoding, warn = F)
    # print(paste0("Read line ", line))
    if ( length(line) == 0 ) {
      break
    }

    if ( grepl(ignoreREGEXP, line) ) {
      next
    }

    if ( grepl(sectionREGEXP, line) ) {
      matches <- regexec(sectionREGEXP, line)
      lastSection <- regmatches(line, matches)[[1]][2]
    }

    if ( grepl(keyValueREGEXP, line) ) {
      key <- trim(paste0(strsplit(line, '')[[1]][1:(index(line, '=') - 1)], collapse = ''))
      value <- trim(paste0(strsplit(line, '')[[1]][(index(line, '=') + 1):nchar(line)], collapse = ''))
      if( startsWith(value, '"') & endsWith(value, '"') ) {
        # Strip beginning and ending quotes
        value <- substr(value, 2, nchar(value) - 1)
        # Replace escaped quotes with quotes
        value <- sub('\\"', '"', value, fixed=TRUE)
      }

      ini[[ lastSection ]] <- c(ini[[ lastSection ]], list(key = value))
      names(ini[[ lastSection ]])[ match('key', names(ini[[ lastSection ]])) ] <- key
    }

  }

  ini
}
