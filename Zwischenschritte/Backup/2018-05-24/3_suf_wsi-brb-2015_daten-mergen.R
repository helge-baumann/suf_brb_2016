# Gewichtungs- und Interviewerdaten mergen

# Gewichte
gew <- gew[,c("lfd", "gewab_k", "gewab_l", "gewbr_k", "gewbr_l")]
sufdat <- merge(rawdat, gew, by="lfd")

# Interviewerdaten 

## 1. Interviewernummer über Methodendatensatz
meth <- meth[, c("lfd", "internr_n")]

sufdat <- merge(sufdat, meth, by="lfd", all.y=FALSE)
#sufdat <- merge(sufdat, int, by.x="internr_n", by.y="Internr_n", all.y=F)

for(b in names(sufdat)) {
  
  if(is.null(attr(rawdat[[b]], "label")) ==FALSE) {
    attr(sufdat[[b]], "label") <- attr(rawdat[[b]], "label") 
  }
  
  if(is.null(attr(gew[[b]], "label")) ==FALSE) {
    attr(sufdat[[b]], "label") <- attr(gew[[b]], "label") 
  }
  
  if(is.null(attr(meth[[b]], "label")) ==FALSE) {
    attr(sufdat[[b]], "label") <- attr(meth[[b]], "label") 
  }


}

rm(list=c("gew", "meth", "int", "b"))