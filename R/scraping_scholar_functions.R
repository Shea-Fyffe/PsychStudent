#' @title Scraping Google Scholar
#' @author Shea Fyffe sfyffe@@gmail.com
#' @description Wrapper function that collect data from a Google Scholar Search
#' @seealso \link{https://scholar.google.com/robots.txt} For preventing scraper from being caught.
#' @param ... String to be passed to Google Scholar search.
#'  Use \code{min =} and \code{max = } to restirict number of articles returned.
#'  Use \code{year = } as a single-element vector to pull articles newer than the given year.
#'  Use \code{year = } as a double-element vector to pull articles between two dates.
#'  \code{year[1]} sets signifies 'after date' and \code{year[2]} 'before date'.
#'  Use \code{article =} to search within a particular article. *note* this should be the article
#'  given by Google (e.g., usually a string of digits), define this as a character.
#' @details \code{max = } will default to 200 if note explicitly defined.
#'  \code{article = } will use search terms within the article defined.
#' @examples
#' \dontrun{
#' ## Searches for 'Engagement' AND 'organizational behavior' and limits return to 300
#' res <- scrape_scholar("Engagement", "organizational behavior", max = 300)
#' }
#'
#' \dontrun{
#' ## Searches for articles newer that 2010 with the words 'Job satistfaction' and limits return to 200 starts at 100
#' res <- scrape_schola("Job satisfaction", year = 2010, min = 100, max = 200)
#' }
#'
#' \dontrun{
#' ## Searches for articles newer that 2010 but earlier than 2015 with the words 'Job satistfaction' and limits return to 200 starts at 100
#' res <- scrape_scholar("Job satisfaction", year = c(2010, 2015), min = 100, max = 200)
#' }
#'
#' \dontrun{
#' ## Searches for articles WITHIN article 10572144307387769361 newer that 2010 but earlier than 2015 with the words 'Job satistfaction'
#' ## and limits return to 200 starts at 100
#' res <- build_search("Job satisfaction", article = "10572144307387769361", year = c(2010, 2015), min = 100, max = 200)
#' }
#' @export
#' TODO(2021-10-07) clean up arguments passed to ...,
#' make into generic function)
scrape_scholar <- function(..., base_wait = 10, user_agent = NULL, verbose = FALSE) {
    .tmp <- paste(c(...))
    if (any(sapply(.tmp, function(x) {
        grepl("^https://", x)
    }))) {
        .url <- .tmp
    } else {
        .url <- build_search(...)
    }

    if (is.null(user_agent)) {
        user_agent <- sample(.use_age(), 1)
    }
    mproxy <- .get_proxy()
    .mpr <- sample(seq(nrow(mproxy)), 1L)
    # set a random user_agent and a timeout
    .start_session <-
        rvest::session(.url[[1L]], httr::user_agent(user_agent), httr::timeout(3L),
                       httr::use_proxy(url = mproxy[.mpr, 1], port = mproxy[.mpr, 2])
        )
    my_cookies_table <- httr::cookies(.start_session) # get the cookies as a table
    .cookies <- my_cookies_table$value
    names(.cookies) <- my_cookies_table$names
    # drop links if they are beyond results
    .url <- .url[.index_crawl(.start_session, .url)]
    .out <- list()
    for (i in seq_along(.url)) {
        if (i > 1) {
            .out[[i]] <- try(scrape_list(.url[i],
                                         base_wait = base_wait, verbose = verbose, session = .start_session
            ))
        } else {
            .out[[i]] <- try(scrape_list(.url[i],
                                         base_wait = base_wait, verbose = verbose
            ))
        }

        if (inherits(.out[[i]], "try-error")) {
            if (i == 1L) {
                stop(
                    sprintf(
                        "ERROR: %s
                   open:%s and check for CAPTCHA and/or mispellings",
                   .out[[i]],
                   .url[i]
                    )
                )
            } else {
                .out <- .out[seq(.i - 1L)]
                warning("Returning a partial dataset. Google is onto you, go play outside...")
                break
            }
        }
    }
    return(.out)
}
#' @title Scrape Child Function
#' @description This function actually does the scraping of Google Scholar
#' @export
#' @import curl rvest xml2 httr
scrape_list <- function(.page, base_wait = 10, verbose = FALSE, session = NULL, ...) {
    if (!is.null(session)) {
        .page <- rvest::jump_to(session, .page)
    }

    t0 <- Sys.time()

    my_page <-
        tryCatch(
            xml2::read_html(.page),
            error = function(err) {
                stop(print(err))
            }
        )

    t1 <- Sys.time()

    .pause(as.numeric(t1 - t0), base_wait)

    .cap <-
        rvest::html_text(rvest::html_nodes(xml2::xml_child(my_page, 2), "script")[1])
    if (length(.cap) != 0L) {
        if (grepl("gs_captcha_cb()", .cap)) {
            stop(
                "Oh no! Google thinks you're a robot...
         Take a break, go make human friends, and try again later."
            )
        }
    }
    if (verbose) {
        return(xml2::html_structure(my_page))
    }

    .out <- parse_page(my_page)
    on.exit(closeAllConnections())
    return(.out)
}
scrape_depreciated <- function(url, base_wait = 10,
                   user_agent = NULL,
                   verbose = FALSE) {
    if (is.null(user_agent)) {
        user_agent <- sample(.use_age(), 1)
    }
    mproxy <- .get_proxy()
    .mpr <- sample(seq(nrow(mproxy)), 1L)
    # set a random user_agent and a timeout
    .cfg <- list(
        httr::user_agent(user_agent),
        httr::timeout(3L),
        httr::use_proxy(
            url = mproxy[.mpr, 1],
            port = mproxy[.mpr, 2]
        )
    )

    t0 <- Sys.time()

    my_page <-
        tryCatch(
            xml2::read_html(httr::content(httr::GET(url, .cfg), as = "text")),
            error = function(err) {
                stop(print(err))
            }
        )

    t1 <- Sys.time()

    .cap <-
        rvest::html_text(rvest::html_nodes(xml2::xml_child(my_page, 2), "script")[1])
    if (length(.cap) != 0L) {
        if (grepl("gs_captcha_cb()", .cap)) {
            stop(
                "Oh no! Google thinks you're a robot...
         Take a break, go make human friends, and try again later."
            )
        }
    }
    if (verbose) {
        return(xml2::html_structure(my_page))
    } else if (is.na(my_page)) {
        on.exit(closeAllConnections())
        return(invisible(my_page))
    } else {
        .out <- parse_page(my_page)
    }

    .pause(as.numeric(t1 - t0), base_wait)

    on.exit(closeAllConnections())
    return(.out)
}
#' @title Build Search
#' @author Shea Fyffe sfyffe@@gmail.com
#' @description Helper function for scraping Google Scholar
#' @param ... String to be passed to Google Scholar search.
#'  Use \code{min =} and \code{max = } to restirict number of articles returned.
#'  Use \code{year = } as a single-element vector to pull articles newer than the given year.
#'  Use \code{year = } as a double-element vector to pull articles between two dates.
#'  \code{year[1]} sets signifies 'after date' and \code{year[2]} 'before date'.
#'  Use \code{article =} to search within a particular article. *note* this should be the article
#'  given by Google (e.g., usually a string of digits), define this as a character.
#' @details \code{max = } will default to 200 if note explicitly defined.
#'  \code{article = } will use search terms within the article defined.
#' @examples
#' \dontrun{
#' ## Searches for 'Engagement' AND 'organizational behavior' and limits return to 300
#' res <- build_search("Engagement", "organizational behavior", max = 300)
#' }
#'
#' \dontrun{
#' ## Searches for articles newer that 2010 with the words 'Job satistfaction' and limits return to 200 starts at 100
#' res <- build_search("Job satisfaction", year = 2010, min = 100, max = 200)
#' }
#'
#' \dontrun{
#' ## Searches for articles newer that 2010 but earlier than 2015 with the words 'Job satistfaction' and limits return to 200 starts at 100
#' res <- build_search("Job satisfaction", year = c(2010, 2015), min = 100, max = 200)
#' }
#'
#' \dontrun{
#' ## Searches for articles WITHIN article 10572144307387769361 newer that 2010 but earlier than 2015 with the words 'Job satistfaction'
#' ## and limits return to 200 starts at 100
#' res <- build_search("Job satisfaction", article = "10572144307387769361", year = c(2010, 2015), min = 100, max = 200)
#' }
#' @seealso \code{scrape_scholar}
#' @export
build_search <- function(...) {
    .search <- list(...)

    if ("year" %in% names(.search)) {
        .year <- .search[["year"]]
        .search[["year"]] <- NULL
    }
    if ("max" %in% names(.search)) {
        .max <- .search[["max"]]
        if (as.numeric(.max) %% 20 != 0) {
            stop("Please verify max is divisible by 20")
        }
        .search[["max"]] <- NULL
    }
    if ("min" %in% names(.search)) {
        .min <- .search[["min"]]
        if (as.numeric(.min) %% 20 != 0) {
            stop("Please verify min is divisible by 20")
        }
        .search[["min"]] <- NULL
    }
    if ("article" %in% names(.search)) {
        .cites <- .search[["article"]]
        .search[["article"]] <- NULL
    }
    if ("title" %in% names(.search)) {
        .title <- TRUE
    } else {
        .title <- FALSE
    }

    if (exists(".cites")) {
        .base <- .get_base(.cites)
    } else {
        .base <- .get_base(NULL)
    }

    .args <- c(exists(".year"), exists(".max"), exists(".min"))

    if (is.recursive(.search)) {
        .search <- unlist(.search)
    }

    if (.title) {
        .search <- c("allintitle:", .search)
    }

    .search <- vapply(.search,
                      FUN = utils::URLencode, FUN.VALUE = character(1),
                      reserved = TRUE
    )
    .search <- paste(.search, collapse = "+")

    if (nchar(.search) > 255L) {
        stop("search string too many characters, please shorten")
    }

    if (all(.args)) {
        .max <- seq(as.numeric(.min), as.numeric(.max), by = 20)
        if (length(.year) == 2L) {
            .add <-
                sprintf(
                    "start=%d&as_ylo=%s&as_yhi=%s&q=%s&btnG=",
                    .max,
                    .year[1],
                    .year[2],
                    .search
                )
        } else {
            .add <-
                sprintf("start=%d&as_ylo=%s&q=%s&btnG=", .max, .year, .search)
        }
    } else if (.args[1] & !.args[2] & !.args[3]) {
        .max <- seq(0, 200, by = 20)
        if (length(.year) == 2L) {
            .add <-
                sprintf(
                    "start=%d&as_ylo=%s&as_yhi=%s&q=%s&btnG=",
                    .max,
                    .year[1],
                    .year[2],
                    .search
                )
        } else {
            .add <-
                sprintf("start=%d&as_ylo=%s&q=%s&btnG=", .max, .year, .search)
        }
    } else if (!.args[1] & (.args[2] & .args[3])) {
        .max <- seq(as.numeric(.min), as.numeric(.max), by = 20)
        .add <- sprintf("start=%d&q=%s&btnG=", .max, .search)
    } else if (!.args[1] & .args[2] & !.args[3]) {
        .max <- seq(0, as.numeric(.max), by = 20)
        .add <- sprintf("start=%d&q=%s&btnG=", .max, .search)
    } else if (!.args[1] & !.args[2] & .args[3]) {
        .max <- seq(as.numeric(.min), as.numeric(.min) + 220, by = 20)
        .add <- sprintf("start=%d&q=%s&btnG=", .max, .search)
    } else if (.args[1] & .args[2] & !.args[3]) {
        .max <- seq(0, as.numeric(.max), by = 20)
        if (length(.year) == 2L) {
            .add <-
                sprintf(
                    "start=%d&as_ylo=%s&as_yhi=%s&q=%s&btnG=",
                    .max,
                    .year[1],
                    .year[2],
                    .search
                )
        } else {
            .add <-
                sprintf("start=%d&as_ylo=%s&q=%s&btnG=", .max, .year, .search)
        }
    } else if (.args[1] & !.args[2] & .args[3]) {
        .max <- seq(as.numeric(.min), as.numeric(.min) + 200, by = 20)
        if (length(.year) == 2L) {
            .add <-
                sprintf(
                    "start=%d&as_ylo=%s&as_yhi=%s&q=%s&btnG=",
                    .max,
                    .year[1],
                    .year[2],
                    .search
                )
        } else {
            .add <-
                sprintf(
                    "start=%d&hl=en&as_vis=1?&as_sdt=0,47&as_ylo=%s&q=%s&btnG=",
                    .max,
                    .year,
                    .search
                )
        }
    } else {
        .max <- seq(0, 200, by = 20)
        .add <- sprintf("start=%d&q=%s&btnG=", .max, .search)
    }

    .base <- paste0(.base, .add)

    return(.base)
}
#' @title Parse web page
#' @details Helper function that finds if articles are missing nodes (i.e., Abstracts, links, etc.).
#'  Also cleans data.
#' @export
#' @import rvest xml2
parse_page <- function(.html) {
    .html <- rvest::html_nodes(.html, "div.gs_ri")
    if (is.null(.html)) {
        return(NA)
    }
    .html <- do.call(rbind, lapply(.html, function(x) {
        Title <-
            tryCatch(
                xml2::xml_text(rvest::html_element(x, css = ".gs_rt")),
                error = function(err) {
                    NA
                }
            )
        Meta <-
            tryCatch(
                xml2::xml_text(rvest::html_element(x, css = ".gs_a")),
                error = function(err) {
                    NA
                }
            )
        Abstract <-
            tryCatch(
                xml2::xml_text(rvest::html_element(x, css = ".gs_rs")),
                error = function(err) {
                    NA
                }
            )
        Link <-
            tryCatch(
                rvest::html_attr(rvest::html_element(x, css = "h3 a"), "href"),
                error = function(err) {
                    NA
                }
            )
        x <-
            data.frame(Title, Meta, Abstract, Link, stringsAsFactors = FALSE)
        return(x)
    }))

    .html <- .parsing_helper(.html)
    .html <-
        .html[c("Title", "Author", "Journal", "Year", "Link", "Abstract")]
    return(.html)

}

.parsing_helper <- function(.df) {
    ..clean_all <- function(.vec) {
        .vec <- gsub("[\u2026]+", "<ect>", .vec, perl = TRUE)
        .vec <- stringi::stri_trans_general(.vec, "latin-ascii")
        .vec <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", .vec, perl = TRUE)
        .vec <- tolower(.vec)
        return(.vec)
    }
    .meta <- gsub("[\u00A0]+", " ", .df$Meta, perl = TRUE)
    .meta <- gsub(",\\s+(?=\\d{4}( \\-|$))", " - ", .meta, perl = T)
    .meta <- gsub("\\s\\-\\s", "[SEP]", .meta, perl = T)
    .meta <- strsplit(.meta, "[SEP]", fixed = TRUE)
    .meta <- lapply(.meta, .sort_meta)
    .meta <- do.call("rbind", .meta)
    .df$Title <- gsub("\\[[^\\]]*\\]", "", .df$Title, perl = TRUE)
    .df$Abstract <-
        gsub("[^[:alnum:][:blank:]+?&/\\-]", " ", .df$Abstract, perl = TRUE)
    .df$Abstract <- gsub("^Page\\s\\d+.|\\s?\\-\\s", "", .df$Abstract)
    .df <- data.frame(.df, .meta)
    .df[] <- lapply(.df, ..clean_all)
    return(.df)
}
#' @title Extract Author, Journal, and Year
#' @details Helper function that extracts Author, Journal, and Year from a single node
.sort_meta <- function(.vec) {
    `%||%` <- function(.x, .y) {
        if(is.null(.x)||is.na(.x)||length(.x)==0L) {
            .y
        } else {
            .x
        }
    }
    .name_list <- c("Author", "Journal", "Year", "Source")
    .auth_indx <- 1L
    .year_indx <- grep("^\\d{4}$", .vec)
    .src_indx <- ifelse(!length(.year_indx), length(.vec), .year_indx + 1L)
    .journ_indx <- setdiff(seq(4L), c(.auth_indx, .year_indx, .src_indx))
    .res <- lapply(c(.auth_indx, .journ_indx, .year_indx, .src_indx), \(x) {
        .vec[x] %||% NA
    })
    .res <- data.frame(.res, stringsAsFactors = FALSE)
    names(.res) <- .name_list
    return(.res)
}
#' @title Randomize User-Agent Helper
#' @details Function that randomly selects user-agents to make the bot seem 'human'
#' @export
.use_age <- function() {
    .out <-
        c(
            # Chrome
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36",
            "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36",
            "Mozilla/5.0 (Windows NT 5.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36",
            "Mozilla/5.0 (Windows NT 6.2; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36",
            "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/44.0.2403.157 Safari/537.36",
            "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36",
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36",
            "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/57.0.2987.133 Safari/537.36",
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.87 Safari/537.36",
            "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/55.0.2883.87 Safari/537.36",
            # Firefox
            "Mozilla/4.0 (compatible; MSIE 9.0; Windows NT 6.1)",
            "Mozilla/5.0 (Windows NT 6.1; WOW64; Trident/7.0; rv:11.0) like Gecko",
            "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; WOW64; Trident/5.0)",
            "Mozilla/5.0 (Windows NT 6.1; Trident/7.0; rv:11.0) like Gecko",
            "Mozilla/5.0 (Windows NT 6.2; WOW64; Trident/7.0; rv:11.0) like Gecko",
            "Mozilla/5.0 (Windows NT 10.0; WOW64; Trident/7.0; rv:11.0) like Gecko",
            "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.0; Trident/5.0)",
            "Mozilla/5.0 (Windows NT 6.3; WOW64; Trident/7.0; rv:11.0) like Gecko",
            "Mozilla/5.0 (compatible; MSIE 9.0; Windows NT 6.1; Trident/5.0)",
            "Mozilla/5.0 (Windows NT 6.1; Win64; x64; Trident/7.0; rv:11.0) like Gecko",
            "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; WOW64; Trident/6.0)",
            "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; Trident/6.0)",
            "Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0; .NET CLR 2.0.50727; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729)"
        )
    return(.out)
}
#' @title Pause Helper
#' @details Function that randomly pauses after scraping a web page to make the bot seem 'human'.
#'  to increase scraping speed, lower sampling time.
.pause <- function(x, .base_wait) {
    .wait <- sample(seq(1, 2, .25), 1) * (1 / (1 / 2.5))
    .wait <- .base_wait * (.wait * x)
    message(sprintf("waiting for %.3f seconds...to prevent being blocked", .wait))

    Sys.sleep(.wait) # pause to let connection work

    closeAllConnections()
    gc()
}
#' @title Base Url Helper
#' @details Builds a base-url and determines if search should be global (i.e., across all articles)
#'  or local (i.e., within article cited by)
.get_base <- function(.cites = NULL) {
    if (!is.null(.cites)) {
        .base <-
            sprintf(
                "https://scholar.google.com/scholar?&hl=en&num=20&as_vis=1?&as_sdt=1,47&cites=%s&scipsc=1&",
                .cites
            )
    } else {
        .base <-
            "https://scholar.google.com/scholar?&hl=en&num=20&as_vis=1?&as_sdt=1,47&"
    }
    return(.base)
}
#' @title Randomize Proxy Helper
#' @details Function that randomly selects proxy string to make the bot seem 'human'
#'
#' @return A dataframe of proxy information
#' @export
#'
.get_proxy <- function() {
    if ("mproxy" %in% ls(envir = parent.frame())) {
        return(get("mproxy", envir = parent.frame()))
    } else {
        my_page <-
            tryCatch(
                xml2::read_html(x = "https://free-proxy-list.net/"),
                error = function(err) {
                    stop(print(err))
                }
            )
        .proxy <- rvest::html_table(my_page)[[1L]]
        .proxy <- .proxy[.proxy[, 7] == "yes", ]
        if (nrow(.proxy[.proxy[, 3] == "US", ]) > 1) {
            .proxy <- .proxy[.proxy[, 3] == "US", c(1, 2)]
        } else {
            .proxy <- .proxy[, c(1, 2)]
        }
        return(as.data.frame(.proxy))
    }
}
#' @title Count Number of pages before crawling
#' @details just a helper to get the number of results from a search
.index_crawl <- function(.page, url_strings) {
    if(is.character(.page)) {
        .page <- xml2::read_html(x = .page)
    }
    .res <- rvest::html_elements(.page, ".gs_ab_mdw")
    .res <- rvest::html_text(.res)[2]
    .res <-
        as.numeric(gsub(",", "", sub("About (.*?) results.*", "\\1", .res)))
    if(is.na(.res)) {
        .res <- 1L
    } else {
        .obs_indx <- stringr::str_extract(url_strings, "(?<=start\\=)\\d+")
        .res <- seq_along(url_strings)[as.numeric(.obs_indx) < .res]
    }
    return(.res)
}
