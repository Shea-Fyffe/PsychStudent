#' Title
#'
#' @param n1
#' @param n2
#' @param es
#' @param out_vg
#'
#' @return
#' @export
#'
#' @examples
compute_g <- function(n1, n2, es, out_vg = TRUE) {
  .df <- 4 * (n1 + n2 - 2)
  .j <- 1 - 3/(.df - 1)
  .res <-  .j * es
  if (out_vg) {
    .vg <- .j^2 * compute_v(n1, n2, es)
    .res <- list(g = .res, vg = .vg)
  }
  return(.res)
}
#' Title
#'
#' @param n1
#' @param n2
#' @param es
#' @param inverse
#'
#' @return
#' @export
#'
#' @examples
compute_v <- function(n1, n2, es, inverse = FALSE) {
  .res <- (n1 + n2) / (n1 + n2)
  .res <- es^2 / (2 * (n1 + n2))
  if (inverse) {
    .res <- 1 / .res
  }
  return(.res)
}

#' Title
#'
#' @param x1
#' @param x2
#' @param n1
#' @param n2
#' @param sd1
#' @param sd2
#'
#' @return
#' @export
#'
#' @examples
compute_d <- function(x1, x2, n1, n2, sd1, sd2) {
  .md <- x1 - x2

  .sswn <- (n1 - 1)*sd1^2 + (n2 - 1)*sd2^2
  .sswd <- (n1 + n2 - 2)

  .es <- .md / sqrt(.sswn / .sswd)
  return(.es)
}
#' Title
#'
#' @param x1
#' @param x2
#' @param n1
#' @param n2
#' @param sd1
#' @param sd2
#'
#' @return
#' @export
#'
#' @examples
compare_effect_size <- function(es_x1, es_x2, r_x1_x2, N) {
  .vals <- as.list(match.call()[-1])
  num_test <- all(vapply(.vals,
    \(x) is.numeric(x) && length(x) == 1L,
    FUN.VALUE = logical(1)
  ))
  if (!num_test) {
    stop("all arguments must be numeric vectors with a length of 1")
  }
  if (N < 4) {
    stop("number of subjects must be greater than 3")
  }
  .vals <- unlist(.vals, use.names = FALSE)
  .rbar <- sum(.vals[1:2]) / 2L
  .rdet <- Reduce(\(x,y) {
    x - y^2
  }, init = 1, x = .vals[-4L]) + 2 * prod(.vals[-4L])
  .n1 <- .vals[4L] - 1L
  .b <- 2*(.n1 / (.n1 - 2L)) * .rdet + prod(.rbar^2, (1 - .vals[3])^3)
  .est <- -1L*diff(.vals[1:2]) * sqrt((.n1 * (1 + .vals[3])) / .b)
  .p <- 2*pt(abs(.est), N - 3L, lower.tail = FALSE)
  return(list(estimate = .est, df = N - 3L,
              p_value = .p,
              statement = sprintf("t(%d) = %.5f, p = %0.5f", N - 3L, .est, .p)))
}

