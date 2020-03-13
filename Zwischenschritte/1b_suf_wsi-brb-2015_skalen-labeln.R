# Unvollständige Labels nachreichen

for(b in names(rawdat)) {
 
  # check, ob es Valueabels gibt
  if(is.null(attr(rawdat[[b]], "labels")) == FALSE) {
    
    # Labels extrahieren
    valuelabels <- attr(rawdat[[b]], "labels")
    
    # alle Werte sortieren
    values <- na.omit(unique(sort(rawdat[[b]])))
    
    # Label für den ersten Wert, z.B. "1"
    firstvalue <- valuelabels[valuelabels == values[1]]
    
    # Ist der erste Wert benannt und ein weiterer nicht?
    if(
      length(firstvalue) == 1 & 
      length(values) > length(valuelabels)
      ) {
      
      unnamed <- which(!(values %in% valuelabels))
      valuelabels <- c(valuelabels, unnamed)
      names(valuelabels)[valuelabels==unnamed] <- as.character(unnamed)
      valuelabels <- sort(valuelabels)
      
      attr(rawdat[[b]], "labels") <- valuelabels
      attr(attr(rawdat[[b]], "labels"), "names") <- names(valuelabels)
      
      print(b)
      
    }
    
  }
   
}

rm(list=c("b", "firstvalue", "unnamed", "valuelabels", "values"))
