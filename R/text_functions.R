#' @title Convert PDF File to Text
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#' @param path file path to pdf or file directory of pdf(s)
#' @param ... a character vector to filter out PDF file names
#' @examples
#' \dontrun{
#' pdf_text_files <- get_pdf_text(.dir = "%systemdrive%/Documents and Settings/All Users/Desktop", "words in pdf file name")
#' cat(pdf_text_files)
#' }
#' \dontrun{
#' pdf_text_files <- get_pdf_text(.dir = "%systemdrive%/Documents and Settings/All Users/Desktop", c("Employee", "Engagement"))
#' cat(pdf_text_files)
#' }
#' @import pdftools readr
#' @export
get_pdf_text <- function(path = getwd(), clean = TRUE, ...) {
  stopifnot({
    is.character(path)
    is.logical(clean)
  })
  if(!grepl("\\.pdf$", path)) {
  .path <-
    tryCatch(
      list.files(path, pattern = "\\.pdf$"),
      error = function(err) {
        NA
      }
    )
  if (!length(.path) || is.na(.path)) {
    stop(sprintf("No valid PDF files found in %s", dir))
   }
  .fuzzy <- list(...)
  if (!!length(.fuzzy)) {
    .fuzzy <- paste(.fuzzy, collapse = "|")
    if (any(grepl(.fuzzy, x = .path, ignore.case = TRUE))) {
      .path <- .path[grepl(.fuzzy, x = .path, ignore.case = TRUE)]
    }
   }
  }
  .pdfs <- lapply(.path, pdftools::pdf_text)
  if (clean) {
    .pdfs <- lapply(.pdfs, clean_pdf)
  }
  return(.pdfs)
}
#' @title Identify synonyms using wordnet
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#' @param x character vector of situational adejectives
#' @param POS part-of-speech to be identified
#' @param dictionary a file path wordnet dictionary
#' @param drop if false, will return all synonyms identified
#' @seealso [wordnet::setDict()]
#' @import reshape2 wordnet
#' @export
synonym_match <-
  function(x,
           POS = "ADJECTIVE",
           dictionary = "C:\\Program Files (x86)\\WordNet\\2.1",
           drop = TRUE) {
    .home <- gsub("*\\\\dict", "", dictionary)
    Sys.setenv(WNHOME = .home)
    wordnet::setDict(dictionary)
    .syn <-
      sapply(x, function(x) {
        wordnet::synonyms(word = x, pos = POS)
      })
    .syn <- reshape2::melt(.syn, factorsAsStrings = FALSE)
    names(.syn) <- c("Synonym", "Word")
    .syn[, 1] <- gsub("*\\([^\\)]+\\)$", "", as.character(.syn[, 1]))
    .syn[, "match"] <- ifelse(.syn[, 1] != .syn[, 2], T, F)
    .syn <- .syn[.syn[, "match"], ]
    if (drop) {
      .syn <- .syn[vec_grep(.syn[, 2], .syn[, 1], FALSE), ]
    }
    .syn
  }
#' @title Count common words between two vectors
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#' @param x character vector of words or sentences.
#' @param y character vector of words or sentences.
#' @param stopwords Logical. Remove stop words? Uses [tm::stopwords]
#' @param stem Logical. Stem words? Uses [textstem::stem_word]
#' @import tm textstem
#' @export
count_common_words <-
  function(x,
           y,
           stopwords = TRUE,
           stem = FALSE) {
    stopifnot(is.character(x), is.character(y))
    if (stopwords) {
      x <- .rm_stopwords(x)
      y <- .rm_stopwords(y)
    }
    if (stem) {
      x <- textstem::stem_words(x, "en")
      y <- textstem::stem_words(y, "en")
    }
    l <- sapply(list(unique(x), unique(y)), clean_text)
    l <- sapply(l, function(x) {
      strsplit(x, split = " ")
    })
    res <- sapply(l[[1]], function(x) {
      res <- sapply(l[[2]], function(y) {
        n <- .count_words(x, y)
        n
      })
    })

    res <- as.data.frame(res)
    names(res) <- l[[1]]
    res[, "doc_y"] <- l[[2]]
    res <-
      tidyr::gather_(res, "doc_x", "common_word_count", names(res)[names(res) !=
        "doc_y"], na.rm = T)
    return(res)
  }
#' @title Count words Helper
#' @export
.count_words <- function(x, y) {
  res <- length(intersect(x, y))
  return(res)
}
#' @title Wrap Text Function
#' @export
wrap_text <- function(txt, pattern) {
  if (any(nchar(txt) == 0L)) {
    txt <- txt[!nchar(txt) == 0L]
  }
  lines <- grep(pattern, txt)
  remove <- setdiff(seq(txt), lines)
  d <- diff(lines)
  spread <- unique(d)[order(unique(d), decreasing = TRUE)]
  for (i in seq(spread)) {
    wrap <- lines[d == spread[i]]
    if (spread[i] != 1L && (spread[i] - 1L) %in% spread) {
      txt[wrap] <- paste(txt[wrap], txt[wrap + 1L], sep = " ")
    } else if (spread[i] == 1L) {
      txt[wrap] <- txt[wrap]
    } else {
      txt[wrap + 1L] <- NA
    }
  }
  txt <- txt[-remove]
  return(txt)
}
#' @title Clean text from character vector
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#' @param x character vector of words or sentences.
#' @param rm_nums Logical. Only keep words, hyphens and spaces?
#' @param convert_nums Logical. Update numbers to words?
#' @param convert_contract Logical. Convert contractions to base words?
#' @import qdap
#' @export
clean_text <-
  function(x,
           lowercase = TRUE,
           rm_nums = TRUE,
           convert_nums = FALSE,
           convert_contract = TRUE,
           rm_punct = TRUE,
           rm_whitespace = TRUE,
           filter_punct = list()) {
    stopifnot({
      sapply(
        c(lowercase, rm_nums, convert_nums, rm_punct, rm_whitespace),
        is.logical
      )
    })
    mispell_dict <- function() {
      return(
        list(
          "colour" = "color",
          "centre" = "center",
          "didnt" = "did not",
          "doesnt" = "does not",
          "isnt" = "is not",
          "shouldnt" = "should not",
          "favourite" = "favorite",
          "travelling" = "traveling",
          "counselling" = "counseling",
          "theatre" = "theater",
          "cancelled" = "canceled",
          "labour" = "labor",
          "organisation" = "organization",
          "wwii" = "world war 2",
          "citicise" = "criticize",
          "instagram" =  "social medium",
          "whatsapp" =  "social medium",
          "snapchat" =  "social medium"
        )
      )
    }
    if (typeof(x) != "character") {
      stop("Please define x as a character")
    }

    if (any(grepl(
      "I_WAS_NOT_ASCII",
      iconv(x, "latin1", "ASCII",
        sub = "I_WAS_NOT_ASCII"
      )
    ))) {
      x <- gsub("^(\\s*<U\\+\\w+>\\s*)+.*$", "encoding error", x)
      x <- stringi::stri_trans_general(x, "latin-ascii")
    }

    if (convert_nums) {
      if (any(grepl("[[:digit:]]", x))) {
        x <- qdap::replace_number(x)
        x <- qdap::replace_ordinal(x)
      }
    } else if (rm_nums) {
      x <- gsub("[[:digit:]]", " ", x)
    }

    if (convert_contract) {
      x <- qdap::replace_contraction(x)
      if (any(grepl("['][A-z]", x))) {
        .pat <- c(
          "n't\\b",
          "'re\\b",
          "'d\\b",
          "'ll\\b",
          "'t\\b",
          "'ve\\b",
          "'m\\b"
        )
        .expand <- c(
          " not", " are", " would",
          " will", " not", " have", " am"
        )
        for (i in seq_along(.pat)) {
          x <- gsub(.pat[i], .expand[i], x)
        }
      }
    }

    if (rm_punct) {
      if (length(filter_punct) == 0L) {
        .pat <- "[^[:alnum:]\\s]"
      } else {
        .pat <-
          paste0("[^[:alnum:]\\s", paste0(unlist(filter_punct), collapse = ""), "]")
      }
      x <- gsub(.pat, " ", x)
    }

    if (any(grepl("^\\s*$", x))) {
      x[grep("^\\s*$", x)] <- "NA"
    }

    if (rm_whitespace) {
      x <- gsub("\\s+", " ", x)
      x <- gsub("^\\s+|\\s+$", "", x)
      x <- x[x != ""]
    }

    if (lowercase) {
      x <- tolower(x)
    }

    return(x)
  }
#' @title Capture text between two characters
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#' @param x character vector of words or sentences.
#' @param between character vector of length 2 containing boundary characters
extract_text_between <-
  function(x,
           between = c(".*", ".*"),
           simplify = TRUE,
           ...) {
    .pattern <- paste0("(?<=", between[1], ").*?(?=", between[2], ")")
    .x <- regmatches(x, gregexpr(.pattern, x, perl = TRUE, ...))
    if (simplify) {
      .x <- unlist(.x)
    }
    return(.x)
  }
#' @title Parse PDF article
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#' @param pdf_path file path to article as a pdf
#' @import tabulizer
#' @export
parse_pdf <- function(pdf_path) {
  if (!file.exists(pdf_path)) {
    stop("File path invalid")
  }
  x <- tryCatch(
    {
      tabulizer::extract_text(pdf_path, encoding = "UTF-8")
    },
    warning = function(w) {
      print(paste("warning:", w))
    },
    error = function(e) {
      print(paste("error:", e))
    }
  )
  if (any(grepl("\r\n", x))) {
    x <- unlist(strsplit(x, "\r\n"))
  }
  x <- clean_text(x)
  x <- x[x != ""]
  if (length(x) == 0L) {
    return()
  } else {
    return(x)
  }
}
#' @title Find top words in a text document
#'
#' @param x Character. A vector of words from a text document.
#' @param stopwords Logical. Remove stop words? Uses [tm::stopwords]
#' @param stem Logical. Stem words? Uses [textstem::stem_word]
#' @param ... Additional arguments to be passed to [qdap::freq_terms]
#' @return
#' @export
find_top_words <- function(x,
                           stopwords = TRUE,
                           stem = FALSE,
                           ...) {
  if (stopwords) {
    x <- .rm_stopwords(x)
  }
  if (stem) {
    x <- textstem::stem_words(x, "en")
  }
  if (length(list(...)) != 0L) {
    x <- qdap::freq_terms(text.var = x, ...)
  } else {
    x <-
      qdap::freq_terms(
        text.var = x,
        20,
        at.least = 3,
        stopwords = qdapDictionaries::Top200Words
      )
  }
  return(x)
}
#' @title Attempt to calculate number of english words in a string
#' @param x Character. A vector of words from a text document.
#' @param ... Additional words to be passed to be checked against
#' @seealso [qdapDictionaries::GradyAugmented]
#' @export
get_english_words_ratio <- function(x, ...) {
  if (length(list(...)) != 0L) {
    .dict <- qdapDictionaries::GradyAugmented
  } else {
    .dict <- c(qdapDictionaries::GradyAugmented, paste(...))
  }
  x <- sum(x %in% .dict) / length(x)
  if (is.nan(x)) {
    x <- 1
  }
  return(x)
}
#' @title Check Spelling
#' @param x Character. A vector of words from a text document.
#' @param return_misspell Logical. Return misspelled words? Otherwise will remove.
#' @param ... additional arguments to be passed to \code{\link[hunspell]{hunspell_check}}
#' @seealso [hunspell::suggest]
#' @export
check_spelling <- function(x, return_misspell = TRUE, ...) {
  stopifnot({
    is.character(x)
    !any(grepl("\\s+", x))
    is.logical(return_misspell)
  })
  .ms <- hunspell::hunspell_check(x, ...)
  if (return_misspell) {
    x <- x[!.ms]
  } else {
    x <- x[.ms]
  }
  return(x)
}
#' @title Remove Stopwords from a string
#' @param x Character. A vector of words from a text document.
#' @seealso [tm::stopwords]
#' @export
.rm_stopwords <- function(x) {
  sw <- paste(tm::stopwords("en"), collapse = "\\b|\\b")
  x <- gsub(sw, "", x)
  x <- gsub("\\s+", " ", x)
  x <- gsub("^\\s+|\\s+$", "", x)
  x <- x[x != ""]
  return(x)
}
#' @title Count words in vector of strings
#' @param x Character. A vector of words from a text document.
#' @return a numeric vector of lengths
#' @export
count_string_words <- function(x) {
  if (!is.character(x)) {
    stop("x not a character vector")
  }
  x <- sapply(gregexpr("[[:alpha:]]+", x), function(x) {
    sum(x > 0)
  })
  return(x)
}
#' @title Find rows in data.frame columns with certain words
#' @param ... Required. Character. A list of words to find.
#' @param data Required. Data.frame. A data.frame containing character columns
#' @param partial Logical/Boolean. Include partial matches?
#' @param type Character. Either 'and' or 'or' suggesting how matching words in \code{...}
#' should be treated
#' @return logical vector with \code{length} equal to \code{nrow(data)}
#' @export
has_words <- function(...,
                      data,
                      partial = FALSE,
                      type = "and") {
  stopifnot({
    inherits(data, "data.frame")
    is.logical(partial)
    is.character(type)
  })

  .char <- sapply(data, is.character)

  if (sum(.char) == 0) {
    stop("data contains no valid character columns")
  }

  .words <- c(...)
  if (is.recursive(.words)) {
    .words <- unlist(.words)
  }

  if (partial) {
    .words <- paste0("\\b.*", .words, ".*\\b")
  } else {
    .words <- paste0("\\b", .words, "\\b")
  }

  if (tolower(type) == "and") {
    .out <- list()
    for (w in seq_along(.words)) {
      .out[[w]] <- apply(data[, .char], 1, function(x) {
        x <- grepl(.words[w], x)
        return(any(x))
      })
    }

    .out <- Reduce("&", .out)
  } else if (tolower(type) == "or") {
    .words <- paste0(.words, collapse = "|")
    .out <- apply(data[, .char], 1, function(x) {
      x <- grepl(.words, x)
      return(any(x))
    })
  } else {
    stop("type must be one of the follower: 'and' 'or'")
  }
  return(.out)
}

find_replace_words <-
  function(docs,
           find_wrds,
           replace_wrds,
           fixed = TRUE,
           ignore.case = TRUE) {
    if (length(find_wrds) != length(replace_wrds) ||
      any(vapply(list(find_wrds, replace_wrds), is.recursive, FUN.VALUE = logical(1)))) {
      stop("find_wrds and replace_wrds atomic vectors of the same length.")
    }
    if (!is.character(docs)) {
      stop("docs must be a vector of strings returning TRUE for is.character()")
    }
    if (ignore.case) {
      find_wrds <- tolower(find_wrds)
      docs <- tolower(docs)
    }
    if (fixed) {
      find_wrds <- paste0("\\b", find_wrds, "\\b")
    }
    names(replace_wrds) <- find_wrds
    .res <- stringr::str_replace_all(docs, pattern = replace_wrds)
    return(.res)
  }

tokenize_docs <-
  function(x,
           sep = NULL,
           fixed = TRUE,
           strip = TRUE,
           simplify = FALSE,
           lower_case = FALSE) {
    stopifnot({
      is.null(sep) || length(sep) == 1L
      is.character(x) && length(x) != 0L
    })
    doc_id <- seq_along(x)
    if (lower_case) {
      x <- tolower(x)
      sep <- tolower(sep)
    }
    if (is.null(sep)) {
      .x <- stringi::stri_split_boundaries(x,
        type = "word",
        skip_word_none = strip
      )
    } 
    if (fixed) {
      .x <-
        stringi::stri_split_fixed(x, pattern = sep, omit_empty = strip)
    } else {
      .x <-
        stringi::stri_split_regex(x, pattern = sep, omit_empty = strip)
    }
    .x <- lapply(.x, trimws)
    if (simplify) {
      doc_id <- rep(doc_id, sapply(.x, length))
      .x <- unlist(.x)
    } else {
      .x <- .x[sapply(.x, function(x) {
        length(x) != 0
      })]
    }
    names(.x) <- doc_id
    return(.x)
  }
normalize_sentences <-
  function(txt,
           sent_chars = c(".", "?", "!"),
           normalize_to = "[SEP]", clean = FALSE,
           ...) {
    .sent_str <- paste0("[", paste0(sent_chars, collapse = ""), "]")
    if (!any(grepl(.sent_str, txt))) {
      warning("no sentence ending characters found")
      return(logical(0))
    } else {
      txt <- gsub(paste0("(", .sent_str,")\\1+"), 
           "\\1", txt)
      txt <-
        gsub(paste0("(?<=", .sent_str, ")(?=[\\w{2,}])"), " ", txt, perl = T)
      txt <-
        gsub(paste0("(", .sent_str, ")\\s+"), "\\1 ", txt, perl = T)
      txt <-
        gsub(paste0("\\s+(?=", .sent_str, ")|(?<=", .sent_str, ")\\s+$"),
          "",
          txt,
          perl = T
        )
    }
    txt <-
      gsub(paste0("(?<=", .sent_str, ")(?=[\\w{1}])"), "", txt, perl = T)
    txt <- stringr::str_replace_all(txt, .sent_str, normalize_to)
    if (clean) {
      txt <- clean_text(txt, filter_punct = normalize_to, ...)
    }
    return(txt)
  }
