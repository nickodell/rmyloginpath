# Buffer at the beginning of the login path file.
UNUSED_BUFFER_LENGTH = 4

# The key stored in the file.
STORED_KEY_LENGTH = 20

# True length of key
KEY_LENGTH = 16

# Number of bytes used to store the length of ciphertext.
CIPHER_STORE_LENGTH = 4




read <- function() {
	fp <- file("~/.mylogin.cnf", "rb")
	read_encrypted_file(fp)
}

read_bytes <- function(fp, n) {
	# print(n)
	# TODO: Check what happens if there's a short read
	readBin(fp, "raw", n, size=1, endian="little", signed=FALSE)
}

read_length <- function(fp) {
	len <- readBin(fp, "integer", n=1, size=CIPHER_STORE_LENGTH, endian="little")
	if(length(len) == 0) {
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
	while(line_length > 0) {
        line <- read_line(f, line_length, cipher)
        plaintext <- paste0(plaintext, line)
		line_length <- read_length(f)
	}
	plaintext
}

read_line <- function(f, line_length, cipher) {
	encrypted = read_bytes(f, line_length)
	remove_pad(cipher$decrypt(encrypted, raw=TRUE))
}

remove_pad <- function(line) {
	line_len <- length(line)
	if(line_len == 0) {
		# Can't strip padding from empty string
		return("")
	}
	pad_len <- as.integer(line[line_len])
	if(pad_len > line_len) {
		# Padding is longer than entire line?
		return("")
	}
	line <- line[1:(line_len - pad_len)]
	char_vec <- rawToChar(line, multiple=TRUE)
	paste0(char_vec, collapse="")
}

read_key <- function(f) {
	read_bytes(f, UNUSED_BUFFER_LENGTH)
	# Todo: check return value here for missing data
	create_key(read_bytes(f, STORED_KEY_LENGTH))
}

raw_xor <- function(a, b) {
	as.raw(bitwXor(as.integer(a), as.integer(b)))
}

create_key <- function(key) {
	rkey <- raw(KEY_LENGTH)
	for(i in seq_along(key)) {
		# R indexes are 1-indexed, so convert into 0-indexed
		# before modulus, then convert back
		rkey_index <- ((i - 1) %% KEY_LENGTH) + 1
		rkey[rkey_index] <- raw_xor(rkey[rkey_index], key[i])
	}
	rkey
}

get_aes_cipher <- function(key) {
	# ECB mode ignores the IV
	AES(key, mode="ECB", IV=NULL)
}

# library()
# to.read = file("~/.mylogin.cnf", "rb")
# print(readBin(to.read, "raw", 10, endian="little"))
# config <- read()
# con <- textConnection(config, "r")
# print(read.ini(con)$andre)
