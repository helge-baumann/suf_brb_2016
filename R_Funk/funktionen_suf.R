# Mit Dank an Sjewo:
get.labeltables <- function(dat) {
        varnames <- setNames(names(dat), names(dat))
        lapply(varnames, function(varname) get.label(dat, get.label.name(dat, varname)))
}

set_fac <- function(x) {
  
        for(b in names(x)) {
            
                labname <- get.label.name(x, b)
                labtable <- get.label(x, labname)
                varunique <- na.omit(unique(x[, b]))
        
                names(varunique) <- as.character(varunique)
                gen.lab  <- sort(c(varunique[!varunique %in% labtable], labtable))
                
                if(is.null(labtable)==F) {
                if(class(x[,b])=="integer" &
                   labtable[1]==sort(unique(varunique))[1]) {
        
                x[, b] <- factor(x[, b], levels=gen.lab,
                            labels=names(gen.lab))
                
                attr(x, "label.table")[[b]] <- gen.lab
                
                }
              }
        }
        return(x)
        
}


   



