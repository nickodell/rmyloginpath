# Buffer at the beginning of the login path file.
unused_buffer_length <- 4

# The key stored in the file.
stored_key_length <- 20

# True length of key
key_length <- 16

# Number of bytes used to store the length of ciphertext.
cipher_store_length <- 4



#' @export
login_read <- function(path = NULL) {
  if (missing(path)) {
    path <- get_login_path_file()
  }
  fp <- file(path, "rb")
  login_file <- read_encrypted_file(fp)
  close(fp)
  login_file
}

#' @export
login_parse <- function(path = NULL) {
  if (missing(path)) {
    path <- get_login_path_file()
  }
  login_file <- login_read(path)
  read_mysql_ini(login_file)
}

get_login_path_file <- function() {
  default <- file.path("~", ".mylogin.cnf")
  # TODO: Windows stores its config file in APPDATA
  Sys.getenv("MYSQL_TEST_LOGIN_FILE", unset=default)
}

read_bytes <- function(fp, n) {
  # TODO: Check what happens if there's a short read
  readBin(fp, "raw", n, size = 1, endian = "little", signed = FALSE)
}

read_length <- function(fp) {
  len <- readBin(fp, "integer", n = 1, size = cipher_store_length, endian = "little")
  if (length(len) == 0) {
    # Failed to read length. End of file
    -1
  } else {
    len
  }
}

read_encrypted_file <- function(f) {
  key <- read_key(f)
  cipher <- get_aes_cipher(key)
  plaintext <- ""
  line_length <- read_length(f)
  while (line_length > 0) {
    line <- read_line(f, line_length, cipher)
    plaintext <- paste0(plaintext, line)
    line_length <- read_length(f)
  }
  plaintext
}

read_line <- function(f, line_length, cipher) {
  encrypted <- read_bytes(f, line_length)
  remove_pad(cipher$decrypt(encrypted, raw = TRUE))
}

remove_pad <- function(line) {
  line_len <- length(line)
  if (line_len == 0) {
    # Can't strip padding from empty string
    return("")
  }
  pad_len <- as.integer(line[line_len])
  if (pad_len > line_len) {
    # Padding is longer than entire line?
    return("")
  }
  line <- line[1:(line_len - pad_len)]
  char_vec <- rawToChar(line, multiple = TRUE)
  paste0(char_vec, collapse = "")
}

read_key <- function(f) {
  read_bytes(f, unused_buffer_length)
  # Todo: check return value here for missing data
  create_key(read_bytes(f, stored_key_length))
}

raw_xor <- function(a, b) {
  as.raw(bitwXor(as.integer(a), as.integer(b)))
}

create_key <- function(key) {
  rkey <- raw(key_length)
  for (i in seq_along(key)) {
    # R indexes are 1-indexed, so convert into 0-indexed
    # before modulus, then convert back
    rkey_index <- ((i - 1) %% key_length) + 1
    rkey[rkey_index] <- raw_xor(rkey[rkey_index], key[i])
  }
  rkey
}

get_aes_cipher <- function(key) {
  # ECB mode ignores the IV
  digest::AES(key, mode = "ECB", IV = NULL)
}
