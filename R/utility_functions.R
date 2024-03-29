#' @title Covert birthdate to age
#'
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#'
#' @description Calculates a vector of ages based on a vector of birthdays where \code{is.Date(dob)}.
#'
#' @param dob A date vector of birthdays.
#' @param enddate A date vector used as reference to calculate age.
#' @param units A character vector that specifies time-units of return vector.
#'
#' @usage age.calc(dob, enddate=Sys.Date(), units='years')
#'
#' @return A numeric vector of ages.
#'
#' @family Utilities
#' @export
age_calc <- function(dob,
                     enddate = Sys.Date(),
                     units = "years") {
    if (!inherits(dob, "Date") | !inherits(enddate, "Date")) {
        stop("Both dob and enddate must be Date class objects")
    }
    start <- as.POSIXlt(dob)
    end <- as.POSIXlt(enddate)

    years <- end$year - start$year
    if (units == "years") {
        result <-
            ifelse((end$mon < start$mon) |
                       ((end$mon == start$mon) & (end$mday <
                                                      start$mday)), years - 1, years)
    } else if (units == "months") {
        months <- (years - 1) * 12
        result <- months + start$mon
    } else if (units == "days") {
        result <- difftime(end, start, units = "days")
    } else {
        stop("Unrecognized units. Please choose years, months, or days.")
    }
    return(result)
}
#' @title Fuzzy Match a vector and with an index vector
#'
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#'
#' @description Using \code{\link[stringdist]{stringdistmatrix}}, match \code{x} to index \code{y}.
#'
#'
#' @param x A character vector.
#' @param y A character vector. Should be unique.
#' @param threshold A numeric value specifying distance threshold to report results.
#' A smaller value represents a closer match.
#' @param ... Optional. Used to change matching weights or method in \code{\link[stringdist]{stringdistmatrix}}.
#'
#' @usage fuzzy.match(x, y, threshold = 2.5, ...)
#'
#' @return A data.frame of \code{x} values, matched values from \code{y}, and distance
#'
#' @seealso \code{\link[stringdist]{stringdistmatrix}} for '...' arguments
#'
#' @family Utilities
#' @export
fuzzy_match <- function(x,
                        y = NULL,
                        threshold = 2.5,
                        ...) {
    if (missing(x) || !is.character(x)) {
        stop("Please define x as a character vector")
    }
    if (!is.null(y) & !is.character(y)) {
        stop("Please define y as a character vector")
    }
    if (anyNA(x)) {
        x <- na.omit(x)
    }
    x <- gsub("[[:punct:]]", "", x)
    if (object.size(x) > 500 * 1000) {
        stop("x is too large, please subsection the vector")
    }
    if (is.null(y)) {
        y <- unique(x)
    } else {
        y <- gsub("[[:punct:]]", "", y)
        if (any(duplicated(y))) {
            warning("y is not unique, converting to unique vector")
            if (anyNA(y)) {
                y <- na.omit(y)
            }
            if (object.size(y) > 500 * 1000) {
                stop("y is too large, please subsection the vector")
            }
            y <- unique(y)
        }
    }
    x <- tolower(x)
    y <- tolower(y)
    if (length(setdiff(x, y)) == 0) {
        dist <-
            as.matrix(
                stringdist::stringdistmatrix(
                    x,
                    method = "osa",
                    weight = c(1,
                               1, 1, 1),
                    useNames = "string"
                )
            )
    } else {
        xm <- x[!x %in% y]
        if (any(nchar(xm) < 5)) {
            xms <- xm[nchar(xm) < 5]
            xml <- xm[nchar(xm) >= 5]
            if (length(list(...))) {
                dists <- as.matrix(stringdist::stringdistmatrix(unique(xms), y,
                                                                useNames = "string",
                                                                ...))
                distl <-
                    as.matrix(stringdist::stringdistmatrix(unique(xml), y,
                                                           useNames = "string",
                                                           ...))
            } else {
                dists <- as.matrix(
                    stringdist::stringdistmatrix(
                        unique(xms),
                        y,
                        method = "osa",
                        weight = c(1, 0.1, 1, 1),
                        useNames = "string"
                    )
                )
                distl <-
                    as.matrix(
                        stringdist::stringdistmatrix(
                            unique(xml),
                            y,
                            method = "osa",
                            weight = c(1, 1, 1, 1),
                            useNames = "string"
                        )
                    )
            }
            dist <- rbind(dists, distl)
        } else {
            dist <- as.matrix(
                stringdist::stringdistmatrix(
                    unique(xm),
                    y,
                    method = "osa",
                    weight = c(1, 1, 1, 1),
                    useNames = "string"
                )
            )
        }
    }
    dist <- reshape2::melt(dist)
    names(dist) <- c("original_value", "matched_value", "distance")
    dist$original_value <- as.character(dist$original_value)
    dist$matched_value <- as.character(dist$matched_value)
    dist <- dist[dist$distance <= threshold & dist$distance != 0,]
    row.names(dist) <- NULL
    return(dist)
}
#' @title Convert data.frames to excel workbook
#'
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#'
#' @description Using \code{\link[xlsx]{write.xlsx}}, creates a dynamic workbook based on objects passed.
#'
#'
#' @param file the path to the output file.
#' @param ... objects to be converted to worksheets in excel workbook.
#'
#' @usage save.xlsx(getwd(), foo, bar, data)
#'
#' @return An excel workbook saved by \code{\link[xlsx]{write.xlsx}}.
#'
#' @seealso \code{\link[xlsx]{write.xlsx}}
#'
#' @family Utilities
#' @export
save_xlsx <- function(file = getwd(), ...) {
    if (missing(file)) {
        file <- getwd()
    }
    objects <- list(...)
    fargs <- as.list(match.call(expand.dots = TRUE))
    objnames <- as.character(fargs)[-c(1, 2)]
    nobjects <- length(objects)
    for (i in 1:nobjects) {
        if (i == 1) {
            xlsx::write.xlsx(
                as.data.frame(objects[[i]]),
                file,
                sheetName = objnames[i],
                row.names = FALSE
            )
        } else {
            xlsx::write.xlsx(
                as.data.frame(objects[[i]]),
                file,
                sheetName = objnames[i],
                row.names = FALSE,
                append = TRUE
            )
        }
    }
    print(paste("Workbook", file, "has", nobjects, "worksheets."))
}
#' @title Standard rounding that always rounds 5 up
#'
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#'
#' @description Reworked \code{\link[base]{round}} to always round 5 up.
#'
#' @param x a numeric vector.
#' @param digits integer indicating the number of decimal places to be used.
#'
#' @usage round2(x, digits = 5)
#'
#' @return a numeric vector.
#'
#' @seealso \code{\link[base]{round}}
#'
#' @family Utilities
#' @export
round2 <- function(x, digits = 5L) {
    if (missing(x) || !is.numeric(x)) {
        stop("Please define x as a number")
    }
    if (!is.integer(digits)) {
        stop("Please define digits as an integer")
    }
    posneg <- sign(x)
    z <- abs(x) * 10 ^ digits
    z <- z + 0.5
    z <- trunc(z)
    z <- z / 10 ^ digits
    z * posneg
}
#' @title Trim trailing and leading whitespace from a character vector
#'
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#'
#' @description Simple function that trims white-space from a character vector.
#'
#' @param x a character vector.
#'
#' @usage trim(x)
#'
#' @return a character vector.
#'
#' @seealso \code{\link[base]{gsub}}
#'
#' @family Utilities
#' @export
trim <- function(x) {
    if (missing(x) || !is.character(x)) {
        stop("Please define x as a character vector")
    }
    x <- gsub("^\\s+|\\s+$", "", x)
    return(x)
}

#' @title Working item that will allow the pasting of strings
#' TODO(shea.fyffe)
paste_assign <- function() {
    cat("Paste search string and hit enter twice")
    x <- scan(what = "")
    xa <- gsub("\\\\", "/", x)
    writeClipboard(paste(xa, collapse = " "))
    cat("Here's your de-windowsified path. (It's also on the clipboard.)\n",
        xa,
        "\n")
}

#' @title Download github files from GitItSheaGitIt
#'
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#' @family Utilities
#' @export
download_code <- function(git_file) {
    tmp <- tempfile()
    .git_file <- sprintf(
        "https://raw.githubusercontent.com/Shea-Fyffe/GitItSheaGitIt/master/%s",
        git_file
    )
    .git_file <-
        tryCatch(
            RCurl::getURL(.git_file, ssl.verifypeer = FALSE),
            error = function(err) {
                NA
            }
        )
    if (is.na(.git_file)) {
        stop(sprintf(
            "Error pulling file verify that %s is a valid file name",
            git_file
        ))
    }
    con <- file(tmp, "w")
    writeLines(.git_file, con = con)
    close(con)
    source(tmp)
}
#' @title Find Common value indicies
#' @param x an atomic vector of values
#' @param val a value to find (Default is NULL)
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#' @family Utilities
#' @export
find_runs <- function(x, val = NULL) {
    stopifnot({
        is.atomic(x)
    })
    .ri <- x[-1L] == x[-length(x)]
    .ri <- c(which(.ri), length(x))
    if (!is.null(val)) {
        .vi <- which(x == val)
        .ri <- intersect(.ri, .vi)
    }
    return(.ri)
}
#' @title Find duplicate values
#' @details Adds to \code{duplicated} by creating boolean return as opposed to count.
#' @param x an atomic vector of values
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#' @family Utilities
#' @export
flag_duplicates <- function(x) {
    if (!is.atomic(x)) {
        stop("x is not an atomic vector")
    }
    .x <- as.numeric(x %in% x[duplicated(x)])
    return(.x)
}
#' @title Randomize Group Selection
#' @author Shea Fyffe, \email{shea.fyffe@@gmail.com}
#' @details Return vector of group assignment
#' @param n Required. Numeric value representing sample size.
#' @param groups Required. Numeric value representing desired number of groups.
#' @family Utilities
#' @export
randomize_groups <- function(n, groups) {
    if (length(n) != 1L || length(groups) != 1L) {
        stop("arguments must be a single value")
    }
    if (!is.numeric(n) || !is.numeric(groups)) {
        stop("arguments must be numeric")
    }
    .div <- floor(n / groups)
    .rem <- n %% groups
    .pool <- rep(seq(groups), .div)
    if (.rem == 0L) {
        return(sample(.pool, n, replace = FALSE))
    } else {
        return(sample(c(.pool, seq(.rem)), n, replace = FALSE))
    }
}
#' @title Flatten Recursive List
#' TODO(shea.fyffe) unit test, roxygen code
flatten <- function(x) {
    if (!inherits(x, "list")) {
        return(list(x))
    } else {
        return(unlist(c(lapply(x, flatten)), recursive = FALSE))
    }
}
#' @title Coalesce two data frame
#' TODO(shea.fyffe) unit test, roxygen code
coalesce_data <-
    function(x,
             y,
             id_col,
             filter_pattern = NULL,
             reference.x = TRUE) {
        stopifnot({
            identical(names(x), names(y))
            inherits(x, "data.frame") && inherits(y, "data.frame")
            is.character(id_col)
        })
        if (reference.x) {
            .ref <- x
            .change <- y
        } else {
            .ref <- y
            .change <- x
        }
        .rowindex <- match(.ref[, id_col], .change[, id_col])
        .colindex <- match(names(.ref), names(.change))
        if (anyNA(.rowindex)) {
            warning("some index values not found, rows will be dropped")
            .rowindex <- .rowindex[!is.na(.rowindex)]
        }
        .change <- .change[.rowindex,]
        .change <- .change[, .colindex]
        if (!is.null(filter_pattern)) {
            .keep <- grep(paste0(c(id_col, filter_pattern),
                                 collapse = "|"), names(.ref))
            .ref <- .ref[.keep]
            .change <- .change[.keep]
        }
        .fill <- apply(.ref[!names(.ref) %in% id_col], 1, function(x) {
            x <- all(is.na(x))
        })
        if (length(unique(.fill)) == 1L) {
            return(.ref)
            stop(
                "x is doesn't have completely missing rows.
           Please make sure filter pattern is correct"
            )
        }

        .ref <- rbind(.ref[!.fill,], .change[.fill,])
        return(.ref)
    }
#' @title Expand a dataframe to long
#' TODO(shea.fyffe) unit test, roxygen code
expand_data <- function(x, rep_rows) {
    .x <- x[rep(seq_len(nrow(x)), rep_rows),]
    row.names(.x) <- gsub("\\.", "_", row.names(.x))
    return(.x)
}
#' @title Reorder rows in a Dataframe
#' TODO(shea.fyffe) unit test, roxygen code
rearrange_rows <- function(x, ind_arr, order_index = NULL) {
    if (!all(dim(x) == dim(ind_arr))) {
        stop("x and ind_arr must have the same number of columns and rows")
    } else {
        N <- nrow(x)
    }

    if (inherits(ind_arr, "data.frame")) {
        possible_indx <- unique(unlist(ind_arr))
    } else {
        possible_indx <- unique(ind_arr)
    }

    if (!is.null(order_index)) {
        .indx <- order_index
    } else {
        .indx <- sort(possible_indx)
    }
    .res <- list()
    for (i in seq(N)) {
        .res[[i]] <- unlist(x[i , ])[match(ind_arr[i, ], .indx)]
    }
    .res <- as.data.frame(do.call(rbind, .res))
    if (is.numeric(.indx)) {
        .indx <- paste0("V", .indx)
    }
    names(.res) <- .indx
    return(.res)
}
#' @title Sequence days between two dates
#' TODO(shea.fyffe) unit test, roxygen code
#' @details Provides dates and days of the week between two dates. Will also
#'   filter by day of week if argument \code{subset_day} is specified.
sequence_days <-
    function(from,
             to,
             subset_day = NULL,
             date_format = "%m-%d-%Y") {
        .is_date <- function(.arg_date, date.format = date_format) {
            tryCatch(
                !is.na(as.Date(.arg_date, date.format)),
                error = function(err) {
                    FALSE
                }
            )
        }
        .seq_dates <- function(from_date, to_date,
                               .format) {
            .out <- seq(as.Date(from_date, format = .format),
                        as.Date(to_date, format = .format),
                        by = "days")
            .out
        }
        .check_dates <-
            is.character(c(from, to)) && length(c(from, to)) == 2L
        if (!.check_dates) {
            stop("to and from must be single character strings.")
        }
        if (!all(.is_date(c(from, to)))) {
            stop(
                sprintf(
                    "to and from must strings in the date format %s\n
                     change the date format to match to and from",
                    date_format
                )
            )
        }
        .res_dates <- .seq_dates(from, to, date_format)
        .days_of_week <- weekdays(.res_dates)
        .res <-
            data.frame(date = .res_dates, day_of_week = .days_of_week)
        if (!is.null(subset_day)) {
            .days_of_week_indx <- tolower(unique(.days_of_week))
            .days_of_week_found <-
                grepl(paste0(paste0("^", tolower(subset_day), ".*"), collapse = "|"),
                      .days_of_week_indx)
            if (any(.days_of_week_found)) {
                .res <-
                    .res[tolower(.res$day_of_week) %in% .days_of_week_indx[.days_of_week_found],]
            } else {
                warning(
                    sprintf(
                        "day of week to subset not a valid value\n
                            please specify as one of the following %s\n.
                            Returning raw data...",
                        paste0(.days_of_week_indx, collapse = " ,")
                    )
                )
            }
        }
        return(.res)
    }
#' @title Push Columns to Front of dataframe
#' TODO(shea.fyffe) unit test, roxygen code
#' @details takes column names in a dataframe and pushes them to the front
`%<<%` <- function(x, push_cols = NULL) {
    stopifnot({
        inherits(x, "data.frame")
        is.null(push_cols) || is.character(push_cols)
    })
    if (is.null(push_cols)) {
        return(x)
    }
    .found_names <- setdiff(names(x), push_cols)
    if (length(.found_names) == length(names(x)) ||
        length(.found_names) == 0L) {
        stop("push cols invalid column names in x")
    }
    return(x[, c(push_cols, setdiff(names(x), push_cols))])
}
#' @title Digitize PDF file
#' TODO(shea.fyffe) unit test, roxygen code
#' @details takes in pdf file path and attempts to convert it to digital text using OCR
digitize_pdf <- function(pdf_path, convert_pages = NULL, image_dpi = 600, is_landscape = FALSE, ...) {
    stopifnot({
        is.character(pdf_path) && grepl("\\.pdf$", pdf_path)
    })

    if(!file.exists(pdf_path)) {
        stop(sprintf("pdf file not found: %s", pdf_path))
    }
    .tmp_img_file <- pdftools::pdf_convert(pdf_path, pages = convert_pages, dpi = image_dpi)

    if (is_landscape) {
        for(i in seq_along(.tmp_img_file)) {
            .up_img_file <- magick::image_rotate(magick::image_read(.tmp_img_file[i]), 90)
            magick::image_write(.up_img_file, .tmp_img_file[i])
        }
    }

    .res <- lapply(.tmp_img_file, \(.x) try(tesseract::ocr(.x, ...)))

    if(inherits(.res, "try-error")) {
        warning("unable to convert image file to text via ocr please see `?tesseract::ocr` for more options.
                Returning image file")
    }
    return(.res)
}
