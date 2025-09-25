#' Encode smart contract function parameters
#'
#' Convert tidy R values into Ethereum-compatible ABI call data suitable for
#' Hedera contract calls. The helper supports a limited set of static types and
#' returns a hexadecimal string with a `0x` prefix.
#'
#' @param signature Canonical function signature such as
#'   `"transfer(address,uint256)"`. Constructors can omit the function name and
#'   pass only the parameter list (for example `"uint256,address"`).
#' @param args List or atomic vector of argument values corresponding to the
#'   types declared in `signature`.
#' @param include_selector Logical indicating whether the 4-byte function
#'   selector should prefix the encoded payload. Set to `FALSE` when encoding
#'   constructor parameters that are appended to the contract bytecode.
#'
#' @return A length-one character vector containing ABI-encoded call data with a
#'   `0x` prefix.
#'
#' @details When Keccak-256 hashing is unavailable in the host environment the
#'   helper aborts with an informative message so callers can supply pre-encoded
#'   `call_data` to [contract_call()].
#'
#' @examples
#' contract_encode_parameters(
#'   "transfer(address,uint256)",
#'   list("0x00000000000000000000000000000000000000a1", "42")
#' )
#'
#' @export
contract_encode_parameters <- function(signature,
                                       args = list(),
                                       include_selector = TRUE) {
  if (rlang::is_missing(signature) || is.null(signature) || !nzchar(signature)) {
    cli::cli_abort("`signature` must be a non-empty string.")
  }

  signature <- trimws(as.character(signature)[[1]])
  has_paren <- grepl("\\(", signature, fixed = FALSE)

  if (has_paren) {
    name <- sub("\\(.*$", "", signature)
    inner <- sub("^.*?\\(", "", signature)
    inner <- sub("\\)$", "", inner)
  } else {
    name <- signature
    inner <- signature
  }

  type_str <- trimws(inner)
  types <- if (identical(type_str, "")) {
    character()
  } else {
    trimws(strsplit(type_str, ",", fixed = TRUE)[[1]])
  }

  if (include_selector) {
    if (!nzchar(trimws(name))) {
      cli::cli_abort("Function signatures must include a name when `include_selector` is TRUE.")
    }
    canonical <- paste0(trimws(name), "(", paste0(types, collapse = ","), ")")
  }

  if (!is.list(args)) {
    args <- as.list(args)
  }
  if (length(types) != length(args)) {
    cli::cli_abort("`args` must supply one value for each parameter in `signature`.")
  }

  encoded_args <- purrr::map2_chr(types, args, hadeda_abi_encode_type)
  payload <- paste0(encoded_args, collapse = "")

  if (include_selector) {
    selector <- hadeda_compute_function_selector(canonical)
    paste0("0x", selector, payload)
  } else if (nzchar(payload)) {
    paste0("0x", payload)
  } else {
    "0x"
  }
}

#' @keywords internal
hadeda_abi_encode_type <- function(type, value) {
  type <- trimws(type)
  base <- tolower(type)

  if (identical(base, "address")) {
    return(hadeda_abi_encode_address(value))
  }
  if (identical(base, "bool")) {
    return(hadeda_abi_encode_bool(value))
  }
  if (grepl("^uint(\\d+)?$", base)) {
    bits <- hadeda_abi_extract_bits(base, default = 256)
    return(hadeda_abi_encode_uint(value, bits))
  }
  if (grepl("^int(\\d+)?$", base)) {
    bits <- hadeda_abi_extract_bits(base, default = 256)
    return(hadeda_abi_encode_int(value, bits))
  }
  if (identical(base, "bytes32")) {
    return(hadeda_abi_encode_bytes(value, size = 32))
  }

  cli::cli_abort(
    "Unsupported Solidity type `{type}`. Only address, bool, uint<N>, int<N>, and bytes32 are currently supported.",
    class = "hadeda_abi_unsupported_type"
  )
}

#' @keywords internal
hadeda_abi_extract_bits <- function(type, default = 256) {
  digits <- as.integer(sub("^[^0-9]*", "", type))
  if (is.na(digits) || identical(digits, 0L)) {
    return(default)
  }
  digits
}

#' @keywords internal
hadeda_abi_encode_address <- function(value) {
  if (rlang::is_missing(value) || is.null(value) || length(value) == 0) {
    cli::cli_abort("Address parameters must be supplied as hexadecimal strings.")
  }

  hex <- hadeda_contract_normalise_hex(value)
  hex <- substring(hex, 3) # drop 0x
  if (nchar(hex) > 40) {
    cli::cli_abort("Addresses must be at most 20 bytes (40 hex characters).")
  }

  hadeda_abi_left_pad(hex, 64)
}

#' @keywords internal
hadeda_abi_encode_bool <- function(value) {
  scalar <- NA
  if (is.logical(value) && length(value) == 1) {
    scalar <- isTRUE(value)
  } else if (is.numeric(value) && length(value) == 1) {
    scalar <- !identical(value, 0)
  } else if (is.character(value) && length(value) == 1) {
    trimmed <- trimws(tolower(value))
    if (trimmed %in% c("true", "1")) {
      scalar <- TRUE
    } else if (trimmed %in% c("false", "0")) {
      scalar <- FALSE
    }
  }

  if (is.na(scalar)) {
    cli::cli_abort("Boolean parameters must be supplied as logical, numeric, or character scalars.")
  }

  if (scalar) {
    hadeda_abi_left_pad("1", 64)
  } else {
    hadeda_abi_left_pad("0", 64)
  }
}

#' @keywords internal
hadeda_abi_encode_uint <- function(value, bits = 256) {
  hex <- hadeda_abi_normalise_integer(value)
  if (nchar(hex) > bits / 4) {
    cli::cli_abort("Integer value exceeds {bits}-bit capacity.")
  }
  hadeda_abi_left_pad(hex, 64)
}

#' @keywords internal
hadeda_abi_encode_int <- function(value, bits = 256) {
  if ((is.numeric(value) || is.integer(value)) && any(value < 0)) {
    cli::cli_abort("Negative integers are not yet supported by the encoder.")
  }
  if (is.character(value) && startsWith(trimws(value), "-")) {
    cli::cli_abort("Negative integers are not yet supported by the encoder.")
  }
  hadeda_abi_encode_uint(value, bits)
}

#' @keywords internal
hadeda_abi_encode_bytes <- function(value, size = 32) {
  if (inherits(value, "raw")) {
    hex <- paste0(tolower(sprintf("%02x", value)), collapse = "")
  } else if (is.character(value) && length(value) == 1) {
    trimmed <- trimws(value)
    if (startsWith(trimmed, "0x") || startsWith(trimmed, "0X")) {
      hex <- substring(trimmed, 3)
    } else {
      hex <- trimmed
    }
  } else {
    cli::cli_abort("Fixed-length byte parameters must be supplied as raw vectors or hexadecimal strings.")
  }

  if (nchar(hex) > size * 2) {
    cli::cli_abort("Byte parameters cannot exceed {size} bytes.")
  }
  hex <- tolower(hex)
  padding <- size * 2 - nchar(hex)
  hadeda_abi_right_pad(hex, 64, pad = padding)
}

#' @keywords internal
hadeda_abi_normalise_integer <- function(value) {
  if (rlang::is_missing(value) || is.null(value) || length(value) == 0) {
    cli::cli_abort("Integer parameters must be supplied.")
  }

  if (is.numeric(value) && length(value) == 1) {
    if (!is.finite(value) || value < 0 || floor(value) != value) {
      cli::cli_abort("Integer parameters must be non-negative whole numbers.")
    }
    value <- format(value, scientific = FALSE, trim = TRUE)
  } else if (is.integer(value) && length(value) == 1) {
    value <- format(value, scientific = FALSE, trim = TRUE)
  } else if (is.character(value) && length(value) == 1) {
    value <- trimws(value)
  } else {
    cli::cli_abort("Integer parameters must be supplied as scalar numeric or character values.")
  }

  if (startsWith(value, "0x") || startsWith(value, "0X")) {
    hex <- substring(value, 3)
    if (grepl("[^0-9a-fA-F]", hex)) {
      cli::cli_abort("Hexadecimal integers may only contain 0-9 and a-f characters.")
    }
    hex <- gsub("^0+", "", tolower(hex))
    if (identical(hex, "")) {
      hex <- "0"
    }
    return(hex)
  }

  if (!grepl("^[0-9]+$", value)) {
    cli::cli_abort("Integer parameters must be supplied as base-10 or hexadecimal values.")
  }

  hadeda_decimal_to_hex(value)
}

#' @keywords internal
hadeda_decimal_to_hex <- function(value) {
  digits <- as.integer(strsplit(value, "", fixed = TRUE)[[1]])
  if (length(digits) == 0) {
    return("0")
  }

  digits <- digits[!is.na(digits)]
  if (!length(digits)) {
    return("0")
  }

  if (all(digits == 0)) {
    return("0")
  }

  hex_chars <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f")
  result <- character()

  current <- digits
  repeat {
    quotient <- integer()
    remainder <- 0L
    for (digit in current) {
      remainder <- remainder * 10L + digit
      qdigit <- remainder %/% 16L
      remainder <- remainder %% 16L
      if (length(quotient) || qdigit > 0L) {
        quotient <- c(quotient, qdigit)
      }
    }

    result <- c(hex_chars[remainder + 1L], result)
    if (!length(quotient)) {
      break
    }
    current <- quotient
  }

  paste(result, collapse = "")
}

#' @keywords internal
hadeda_abi_left_pad <- function(hex, width = 64) {
  if (nchar(hex) > width) {
    cli::cli_abort("Encoded value exceeds {width} hex characters.")
  }
  padding <- width - nchar(hex)
  paste0(strrep("0", padding), tolower(hex))
}

#' @keywords internal
hadeda_abi_right_pad <- function(hex, width = 64, pad = NULL) {
  if (is.null(pad)) {
    pad <- width - nchar(hex)
  }
  if (pad < 0) {
    cli::cli_abort("Encoded value exceeds {width} hex characters.")
  }
  paste0(tolower(hex), strrep("0", pad))
}

#' @keywords internal
hadeda_contract_normalise_hex <- function(value) {
  if (inherits(value, "raw")) {
    hex <- paste0(tolower(sprintf("%02x", value)), collapse = "")
    return(paste0("0x", hex))
  }

  if (is.character(value) && length(value) == 1) {
    trimmed <- trimws(value)
    hex <- if (startsWith(trimmed, "0x") || startsWith(trimmed, "0X")) substring(trimmed, 3) else trimmed
    if (grepl("[^0-9a-fA-F]", hex)) {
      cli::cli_abort("Hex strings may only contain 0-9 and a-f characters.")
    }
    if (nchar(hex) %% 2 == 1) {
      hex <- paste0("0", hex)
    }
    return(paste0("0x", tolower(hex)))
  }

  cli::cli_abort("Hex values must be provided as raw vectors or length-one character vectors.")
}

#' @keywords internal
hadeda_compute_function_selector <- function(signature) {
  algorithms <- c("keccak", "keccak-256", "keccak256", "sha3-256")
  for (algo in algorithms) {
    hash <- tryCatch(
      digest::digest(signature, algo = algo, serialize = FALSE),
      error = function(...) NULL
    )
    if (!is.null(hash) && is.character(hash) && nzchar(hash)) {
      return(substr(hash, 1, 8))
    }
  }

  cli::cli_abort(
    c(
      "Keccak-256 hashing is not available in this environment.",
      "i" = "Provide `call_data` manually or install a digest backend with keccak support."
    ),
    class = "hadeda_keccak_unavailable"
  )
}
