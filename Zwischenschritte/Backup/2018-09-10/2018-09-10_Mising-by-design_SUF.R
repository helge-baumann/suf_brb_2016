num <- num+1

Codebook_SUF[num,2] <- paste0(
  "\\textit{", "missing by design", "}"
)
Codebook_SUF[num,3] <- paste0(
  "\\textit{", ".", "}"
)
Codebook_SUF[num,4] <- 
  nrow(sufdat) - length(na.omit(sufdat[[i]]))