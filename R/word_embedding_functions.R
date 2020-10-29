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