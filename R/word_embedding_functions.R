create_lsa_embedding <- function(text, topics = 5L, ...) {
  .x <- create_lexicon(text, ...)
  .dtm <-  text2vec::create_dtm(.x[[1]], .x[[2]])
  .tfidf <- text2vec::TfIdf$new()
  .lsa <- text2vec::LSA$new(n_topics = topics)
  .doc_emb <- text2vec::fit_transform(.dtm, .tfidf)
  .doc_emb <- text2vec::fit_transform(.doc_emb, .lsa)
  return(.doc_emb)
}
create_gloVe_embeddings <- function(text, skip_win = 5L, co_occur_max = 100L, word_vec_len = 50L, convrgnce_tol = 0.01, ...){
  .x <- create_lexicon(text, ...)
  .dtm <-  text2vec::create_tcm(.x[[1]], .x[[2]], skip_win)
  .gloVe <- text2vec::GlobalVectors$new(word_vectors_size = word_vec_len, vocabulary = .x[[3]], x_max = co_occur_max)
  .wd_vec <- text2vec::fit_transform(x = .dtm, model = .gloVe, n_iter = 10L, convergence_tol = convrgnce_tol)
  .wd_vec_context <- .gloVe$components
  .word_vectors <- .wd_vec + t(.wd_vec_context)
  return(as.data.frame(.word_vectors))
}
create_doc_vectors <- function(text, word_vec, doc_ids, has_words_as_row_names = TRUE, ignore_case = TRUE, wts = NULL, FUNC = NULL, SEED = 46) {
  .wordEmbed_helper <- function(.res, .wts, .FUNC) {
    if(is.null(.wts) && is.null(.FUNC)) {
      return(.res)
    }
    .n <- sapply(.res, nrow)
    if(!is.null(.wts)) {
      if(length(.wts) == 1) .wts <- rep(.wts, sum(.n))
      if(length(.wts) != sum(.n)) {
        warning("weights argument must be equal to number of valid words in text returning object for inspection")
        return(.res)
      } else {
        .indx <- rep(seq_along(.n), .n)
        .res <- do.call(rbind, .res)
        .wres <- sweep(x = .res, MARGIN = 1, STATS = .wts, `*`)
        .res <- split(as.data.frame(.wres), .indx)
      }
    }
    if(is.null(.FUNC)) {
      .FUNC <- mean
    }
    
    .res <- lapply(.res, function(.x) {
      .x <- apply(.x, MARGIN = 2, .FUNC)
    })
    
    .res <- do.call(rbind, .res)
    
    return(as.data.frame(.res))
  }
  if(!inherits(word_vec, "data.frame")) {
    word_vec <- as.data.frame(word_vec)
  }
  if(has_words_as_row_names || !is.character(word_vec[,1])) {
    word_vec <- data.frame(row.names(word_vec), word_vec)
  }
  if(!all(sapply(word_vec[,-1], is.numeric))) {
    word_vec[,-1] <- sapply(word_vec[,-1], as.numeric)
  }
  
  
  if(ignore_case) {
    text <- tolower(text)
    word_vec[,1] <- tolower(word_vec[,1])
  }
  if(anyDuplicated(word_vec[,1])) {
    stop("Duplicate words in word embeddings file, please de-duplicate file or specify ignore_case = FALSE")
  }
  # add unknown word tag with a based on a random normal distribution of word embedding file
  set.seed(SEED)
  UNK <- c("<UNK>", as.list(runif(ncol(word_vec) - 1, -.25, .25)))
  names(UNK) <- names(word_vec)
  word_vec <-  rbind(UNK, word_vec)
  if(!is.null(wts)) {
    .res <- word_embed(text, word_vec, meanVec = FALSE)
    if(is.null(FUNC)) {
      .res <- .wordEmbed_helper(.res, .wts = wts, .FUNC = mean)
    } else {
      .res <- .wordEmbed_helper(.res, .wts = wts, .FUNC = FUNC)
    }
  } else {
    if (!is.null(FUNC)) {
      .res <- .wordEmbed_helper(.res, .wts = 1L, .FUNC = FUNC)
    } else {
      .res <- word_embed(text, word_vec)
    }
  }
  row.names(.res) <- doc_ids
  return(.res)
}
create_BERT_embeddings <- function(txt, verbose = F, ...) {
  BERT_PRETRAINED_DIR <- RBERT::download_BERT_checkpoint(
    model = "bert_base_uncased"
  )
  BERT_feats <- RBERT::extract_features(
    examples = tolower(txt),
    ckpt_dir = BERT_PRETRAINED_DIR,
    layer_indexes = 1:12, ...)
  
  if (verbose) {
    return(BERT_feats)
  } else {
    .keep <- BERT_feats$output$layer_index == 12 & BERT_feats$output$token == "[CLS]"
    BERT_feats <- BERT_feats$output[.keep, grepl("^V", names(BERT_feats$output))]
  }
  BERT_feats
}
get_rwmd <- function(text, embeddings) {
  tokens <- text2vec::word_tokenizer(tolower(text))
  v <- text2vec::create_vocabulary(text2vec::itoken(tokens))
  .dtm <-  text2vec::create_dtm(text2vec::itoken(tokens), text2vec::vocab_vectorizer(v))
  .rwmd <- text2vec::RelaxedWordMoversDistance$new(.dtm, embeddings)
  return(.rwmd)
}
embed_document <- word_embed <- function(docs, word_embed_mat, FUN){
  nDoc <- length(docs)
  nDim <- dim(word_embed_mat)[2] - 1
  wordMatList <- list()
  wordvec = matrix(rep(0.0, nDoc * nDim), nrow = nDoc)
  vocab = as.character(word_embed_mat[,1])
  for (i in seq_len(nDoc)) {
    curWordArray = unlist(strsplit(docs[i]," "))
    wordIdx = match(curWordArray, vocab)
    idxNotFound = 1  # Set the same as first word 'a' 
    wordIdx[which(is.na(wordIdx))] = idxNotFound
    curWordMat = as.matrix(word_embed_mat[wordIdx, 2:dim(word_embed_mat)[2]])
    rownames(curWordMat) = curWordArray
    wordMatList = c(wordMatList, list(curWordMat))
  }
  # make a functional wrapper where
  if (meanVec == TRUE) {
    for (i in 1:nDoc) {
      wordvec[i,] = colSums(wordMatList[[i]])/dim(wordMatList[[i]])[1]
    }
    colnames(wordvec) = colnames(word_embed_mat[,-1])
    return (wordvec)
  } else {
    return (wordMatList)
  }
}
import_fasttext <- function(bin_file, path = TRUE, filter = NULL, ignore.case = F) {
  gc()
  if(path) {
    if(!file.exists(bin_file)){
      stop("Verfy txt valid binary file path")
    } else {
      .txt <- fastTextR::ft_load(bin_file)
    }
  } else {
    .txt <- bin_file
  }
  if(!is.null(filter)) {
    filter <- gsub("_","-", filter)
    if(ignore.case) {
      .txt <- fastTextR::ft_word_vectors(.txt, unique(tolower(filter)))
    } else {
      .txt <- fastTextR::ft_word_vectors(.txt, unique(filter))
    }
  }
  return(.txt)
}
import_glove <- function(txt, path = TRUE, filter = NULL, ignore.case = F, ...) {
  gc()
  if(path) {
    if(!file.exists(txt)){
      stop("Verfy txt valid text file path")
    } else {
      .txt <- data.table::fread(file = txt, ...)
      .txt <- as.data.frame(.txt)
    }
  } else {
    .txt <- txt
    .txt <- sapply(.txt, strsplit, split = " ", USE.NAMES = FALSE)
    .txt <- do.call("rbind", .txt)
    .txt <- as.matrix(.txt)
  }
  if(!is.null(filter)) {
    filter <- gsub("_","-", filter)
    if(ignore.case) {
      .txt <- .txt[tolower(.txt[,1]) %in% unique(tolower(filter)),]
    } else {
      .txt <- .txt[.txt[,1] %in% unique(filter),]
    }
  }
  row.names(.txt) <- .txt[,1]
  return(.txt[,-1])
}