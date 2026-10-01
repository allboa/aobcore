## Base64 for raw vectors, vectorised in base R so the core needs no
## encoding package.

b64_alphabet <- c(LETTERS, letters, as.character(0:9), "+", "/")

## Encoded a chunk at a time, through raw bytes rather than one string per
## character: the page's blobs can be hundreds of MB, and a character
## vector of 4/3 n one-letter strings (with the integer matrices beside it)
## took about 40 bytes of R memory per input byte.
b64_encode <- function(x, chunk = 3L * 2^20) {
  stopifnot(is.raw(x), chunk >= 3, chunk %% 3 == 0)
  n <- length(x)
  if (n == 0L) {
    return("")
  }
  starts <- seq(1, n, by = chunk)
  out <- vapply(starts, function(s) b64_chunk(x[s:min(n, s + chunk - 1)]), "")
  paste(out, collapse = "")
}

b64_bytes <- charToRaw(paste(b64_alphabet, collapse = ""))

## One chunk; every chunk but the last is a whole number of 3-byte groups.
b64_chunk <- function(x) {
  n <- length(x)
  pad <- (3L - n %% 3L) %% 3L
  v <- matrix(c(as.integer(x), integer(pad)), nrow = 3L)
  w <- v[1L, ] * 65536L + v[2L, ] * 256L + v[3L, ]
  rm(v)
  idx <- rbind(w %/% 262144L, (w %/% 4096L) %% 64L, (w %/% 64L) %% 64L, w %% 64L)
  rm(w)
  chars <- b64_bytes[idx + 1L]
  if (pad > 0L) {
    chars[(length(chars) - pad + 1L):length(chars)] <- charToRaw("=")
  }
  rawToChar(chars)
}

b64_decode <- function(s) {
  chars <- strsplit(gsub("[^A-Za-z0-9+/=]", "", s), "")[[1L]]
  if (length(chars) == 0L) {
    return(raw())
  }
  if (length(chars) %% 4L != 0L) stop("Base64 text length must be a multiple of 4.", call. = FALSE)
  pad <- sum(chars[length(chars) - 1:0] == "=")
  idx <- match(chars, b64_alphabet) - 1L
  idx[is.na(idx)] <- 0L
  q <- matrix(idx, nrow = 4L)
  w <- q[1L, ] * 262144L + q[2L, ] * 4096L + q[3L, ] * 64L + q[4L, ]
  bytes <- as.vector(rbind(w %/% 65536L, (w %/% 256L) %% 256L, w %% 256L))
  as.raw(bytes[seq_len(length(bytes) - pad)])
}
