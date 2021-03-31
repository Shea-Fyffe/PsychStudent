#' @title Get Group Rank Labels
#' @author Shea Fyffe email: shea.fyffe@@gmail.com
#' @param x Required. Data.frame containing variables to index
#' @param pattern Required. Pattern of variable names to select columns. May use Regex.
#' @param rank
#' @param verbose
#' @family qualtrics
#' @export
get_group_rank_labels <-
  function(x,
           pattern,
           rank = FALSE,
           verbose = FALSE) {
    .pat <- sprintf("%s\\d+", pattern)
    if (verbose) {
      .pat <- sprintf("(^Q\\d+)(%s)(.*)", .pat)
      .out <- names(x)[grepl(.pat, names(x))]
      .out <- gsub(.pat, "\\2", .out)
      return(unique(.out))
    }
    if (rank) {
      .pat <- sprintf("%s_rank$", .pat)
    } else {
      .pat <- sprintf("^Q\\d+%s", .pat)
    }
    .out <- grep(.pat, names(x),
                 value = T, ignore.case = T)
    return(.out)
  }
#' @title combine multiple columns into one
#' @author Shea Fyffe email: shea.fyffe@@gmail.com
#' @param x Required. Data.frame containing variables to index
#' @param labels Required. A character vector of column names.
colesce_columns <- function(x, labels) {
  .out <- apply(x[,labels], 1, function(cols){
    cols <- which(cols == 1L)
  })
  return(.out)
}
#' @title Format Likert-type Items for Qualtrics
#' @author Shea Fyffe email: shea.fyffe@@gmail.com
#' @param stem Required. Character vector of item stems
#' @param ids Optional. Character vector of item IDs to use
#' @param scale Optional. Chacter vector of standard response options.
#' If miss will default 5-point Likert
#' @param items_per_page Optional. Integer number of items to appear on each page.
#' If missing will default to 1.
#' @return A string to be uploaded to qualtrics.
#' @family qualtrics
#' @export
build_qualtrics_likert <- function(stem, ids = NULL, scale = c("Strongly Disagree","Disagree","Neither Agree Nor Disagree","Agree","Strongly Agree"), items_per_page = 1L) {
  if (!is.character(scale)) {
    stop("Please define scale_type as a vector of mode character")
  }
  if (!is.null(ids)) {
    if(length(unique(ids)) != length(stem)) {
      warning("invalid ids using generic ids")
      ids <- paste0("Q_", seq_along(stem))
    }
    .head <- sprintf("[[Question:MC:SingleAnswer:Horizontal]]\n[[ID:%s]]", ids)
  } else {
    .head <- "[[Question:MC:SingleAnswer:Horizontal]]"
  }
  .stem <- sprintf("%s\n", stem)
  .choice <- paste(sprintf("%s\n", scale), collapse = "")
  if(items_per_page == 1) {
   .ipp <- "[[PageBreak]]"
  } else {
    .ipp <- rep("\n", length(.stem))
    .ipp[seq(1, length(.stem), items_per_page)[-1]] <- "[[PageBreak]]"
  }
  .item <- paste(.head, "\n", .stem, "[[Choices]]", "\n",
                 .choice, "\n\n", .ipp, "\n\n", sep = "")
  return(.item)
}
#' @title Format MFC Item Blocks for Qualtrics
#' @author Shea Fyffe email: shea.fyffe@@gmail.com
#' @param statements Required. Character vector of item statements to 
#'  placed within item blocks
#' @param stem Optional. Character vector of item block stems. If defined will over-ride rank_labels.
#' If missing will default to 'For each block of statements, rank the statements from ...'
#' @param ids Optional. Character vector of item IDs to use
#' @param rank_labels Optional. Chacter vector of standard response prompt.
#' If missing will default to most like me (1) to least like me (3)
#' @param n_options Optional. Integer number of statements to appear in item block.
#' If missing will default to 3 (triplet).
#' @param items_per_page Optional. Integer number of items to appear on each page.
#' If missing will default to 1.
#' @return A string to be uploaded to qualtrics.
#' @family qualtrics
#' @export
build_qualtrics_MFC <- function(statements, stem = NULL, ids = NULL, rank_labels = c("most like me", "least like me"), n_options = 3, items_per_page = 1L) {
  if(is.numeric(n_options)&&length(n_options)==1&&n_options>0) {
    n_options <- seq(n_options)
  } else {
    stop("invalid n_options argument should be a single positive number")
  }
  if(length(statements) %% max(n_options) != 0L) {
    stop("statements not divisible by number of statements per block")
  } else {
    .b_indx <- seq(1, length(statements), max(n_options))
  }

  if(is.null(stem)) {
    if(!is.character(rank_labels)|| length(rank_labels) != 2) {
      stop("rank_labels must be a character vector of length 2")
    }
    .stem_add <- paste0(rank_labels[1], " (", min(n_options), ") ", "to ", rank_labels[2], " (", max(n_options), ")")
    .stem <- rep(paste0('For each block of statements, rank the statements from', " ",.stem_add, "\n"), length(.b_indx))
  } else {
    .stem <- gsub("^\\s+|\\s+$", "", stem)
    if(length(.stem) != length(.b_indx)) {
      stop("number of statements per block not divisible by stems")
    }
  }
  
  if (!is.null(ids)) {
    if(length(unique(ids)) != length(.stem)) {
      warning("invalid ids using generic ids")
      ids <- paste0("Q_", seq_along(.stem))
    }
    .head <- sprintf("[[Question:RO]]\n[[ID:%s]]", ids)
  } else {
    .head <- "[[Question:RO]]"
  }
  
  .statements <- split(statements, rep(seq(length(statements)/max(n_options)), 
                                       each = max(n_options)))
  
  .statements <- lapply(.statements, function(x) {
          x <- paste(sprintf("%s\n", x), collapse = "")
  })
  
  if(items_per_page == 1) {
    .ipp <- "[[PageBreak]]"
  } else {
    .ipp <- rep("\n", length(.stem))
    .ipp[seq(1, length(.stem), items_per_page)[-1]] <- "[[PageBreak]]"
  }
  .item <- paste(.head, "\n", .stem, "\n" , "[[Choices]]", "\n",
                 unlist(.statements), "\n\n", .ipp, "\n\n", sep = "")
  
  return(.item)
}
#' @title Format Matrix Style items for Qualtrics
#' @author Shea Fyffe email: shea.fyffe@@gmail.com
#' @param choices Required. Character vector of item statements to 
#'  placed in rows of matrix.
#' @param mat_ids Optional. Character vector of repeated ideas that will map choices to a matrix
#' @param stem Optional. Character vector of matrix stems.
#' If missing will default to 'For each matrix, select the most relevant option'
#' @param answers Optional. Chacter vector of options that will appear in column of matrix item.
#' @param items_per_page Optional. Integer number of items to appear on each page.
#' @param recode_answers_to Optional. Chacter vector to recode \code{answers}
#' @param choices_per_mat Optional. Integer number of choices to appear in each matrix
#' If missing will default to 10.
#' @return A string to be uploaded to qualtrics.
#' @family qualtrics
#' @export
build_qualtrics_matrix <- function(choices, mat_ids = NULL, stem = NULL,
                                   answers = c("Strongly Disagree","Disagree","Neither Agree Nor Disagree",
                                               "Agree","Strongly Agree"),
                                   recode_answers_to = NULL, choices_per_mat = 10, items_per_page = 1L) {
  if (!is.numeric(choices_per_mat) ||
      length(choices_per_mat) != 1 || choices_per_mat <= 0) {
    stop("invalid choices_per_mat argument should be a single positive number")
  }
  if (length(choices) %% choices_per_mat != 0L && is.null(mat_ids)) {
    warning("statements not divisible by number of statements per matrix..adding items
            to the last matrix")
    .b_indx_int <- length(choices) %/% choices_per_mat
    .mat_indx <- c(
      rep(seq(.b_indx_int), each = choices_per_mat),
      rep(.b_indx_int, length(choices) %% .b_indx_int)
    )
    .mat_labs <- paste0("Q_", seq_along(.mat_indx))
    .head <- sprintf("[[Question:Matrix]]\n[[ID:%s]]", .mat_labs)
  } else {
    if (anyDuplicated(mat_ids) == 0 || !is.character(mat_ids)) {
      stop("invalid matrix ids")
    } else {
      .mat_labs <- unique(mat_ids)
      .mat_indx <- as.numeric(factor(mat_ids, levels = .mat_labs))
      
      .head <- sprintf("[[Question:Matrix]]\n[[ID:%s]]", .mat_labs)
    }
  }
  
  if (is.null(stem)) {
    .stem <- rep("For each matrix, select the most relevant option", max(.mat_indx))
  } else {
    .stem <- gsub("^\\s+|\\s+$", "", stem)
  }
  
  if (!is.null(recode_answers_to)) {
    .answers <- sprintf("[[Answers:%s]]\n%s\n", answers, recode_answers_to)
    .answers <- paste0(.answers, collapse = "")
    .answers <- paste0("[[AdvancedAnswers]]", "\n", .answers)
  } else {
    .answers <- sprintf("%s\n", answers)
    .answers <- paste0(.answers, collapse = "")
    .answers <- paste0("[[Answers]]", "\n", .answers)
  }
  
  
  
  .choices <- split(choices, .mat_indx)
  .choices <- lapply(.choices, function(x) {
    x <- paste(sprintf("%s\n", x), collapse = "")
  })
  
  
  
  if (items_per_page == 1) {
    .ipp <- "[[PageBreak]]"
  } else {
    .ipp <- rep("\n", length(.stem))
    .ipp[seq(1, length(.stem), items_per_page)[-1]] <- "[[PageBreak]]"
  }
  .item <- paste(.head, "\n", .stem, "\n", "[[Choices]]", "\n",
                 unlist(.choices), "\n", .answers, .ipp, "\n\n",
                 sep = ""
  )
  
  return(.item)
}
#' @title Enumerate Policy Capturing Items for Qualtrics
#'
#' @param template Required. An item template that will be passed to \code{\link[base]{sprintf}} 
#' @param ... Required. A list of variables to enumerate.
#'
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'  # make a template
#'  temp <- "Compensation: %s
#'     Promotion: %s
#'     Layoff Policy: %s
#'     Environmental Score: %s"
#'     
#'  variables <- list(c(50000, 60000, 70000), 
#'                    c("High", "Average", "Low"), 
#'                    c("Seniority", "Performance"),
#'                    seq(5))
#'  pols <- build_policy_capture(temp, variables)
#' }
build_policy_capture <- function(template, ...) {
  stopifnot({
    is.character(template)
  })
  .vars <- eval(...)
  if(is.atomic(.vars)) {
    .vars <- list(.vars)
  }
  .check_base <- length(gregexpr("[%]+", template)[[1L]])
  if(.check_base != length(.vars)) {
    stop("base needs to be an appropriate string to hold variables see '?sprintf' fmt argument")
  }
  .vars <- expand.grid(.vars, stringsAsFactors = F)
  .out <- do.call(sprintf, c(fmt = template, .vars))
  return(.out)
}
#' @title Package text blocks into Qualtrics survey
#'
#' @param blocks 
#' @param out_dir 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
package_survey <- function(blocks = list(), out_dir = getwd(), emb_data = list()){
  if(!is.list(blocks)||length(blocks) == 0) {
    stop("please define blocks of items to import. See build_qualtrics_ functions")
  }
  if(length(emb_data) == 0) {
    HEAD <- "[[AdvancedFormat]]"
  } else {
    
    if(is.null(names(emb_data))) {
      stop("... should be a named list of embedded date names and values")
    } else {
      emb_labs <- paste0("[[ED:", names(emb_data))
      emb_data <- ifelse(is.null(unlist(emb_data))||is.na(unlist(emb_data)),
                         paste0(emb_labs, "]]"), paste0(emb_labs,":", unlist(emb_data), "]]"))
    }
    
    HEAD <- paste0("[[AdvancedFormat]]", "\n\n", emb_data)
  
  }
  blocks <- lapply(blocks, function(X) X <- c("[[Block]]\n",X))
  setwd(out_dir)
  sink("Qualtrics Import.txt")
  cat(HEAD)
  cat("\n\n")
  cat(paste(unlist(blocks), collapse = "\n"))
  sink()
  message(sprintf("Survey Import file was written to: %s", getwd()))
}
