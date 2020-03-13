# Erstellung des SUFs: Missings definieren

# Löschen unnötiger Informationen ----
#rm(list=setdiff(ls(), c("sufdat", "Codebook_Rawdata", "rawdat")))

# Missings umdefinieren (negative Werte) ----

# Missing durch trifft nicht zu

for(i in 1:ncol(sufdat)) {
  
  # Prüfung: Gibt es Valuelabels?
  if("labels" %in% names(attributes(sufdat[[10]]))) { # 10???
    
    # Prüfung: Gibt es ein Label für "trifft nicht zu"?
    if(TRUE %in% str_detect(names(attributes(sufdat[[i]])$labels), 
                            "rifft nicht zu")) {
      # Prüfung: Gibt es ein Label für "trifft zu"? (dann Skalenfrage)
      if(TRUE %in% str_detect(names(attributes(sufdat[[i]])$labels), 
                              "rifft zu") == F) {
      
        # Trifft-nicht-zu-Wert abgreifen:  
        valuelabels <- attributes(sufdat[[i]])$labels
        value_tnz_alt <- valuelabels[
          str_detect(names(valuelabels), "rifft nicht zu")
          ]
        valuelabels[
          str_detect(names(valuelabels), "rifft nicht zu")
          ] <- -6
        attributes(sufdat[[i]])$labels <- valuelabels
        sufdat[[i]][sufdat[[i]] == value_tnz_alt] <- -6
        
      }
    }
    
    # Prüfung: Gibt es ein Label für "TNZ"?
    if(TRUE %in% str_detect(names(attributes(sufdat[[i]])$labels), 
                            "TNZ")) {
      # Prüfung: Gibt es ein Label für "trifft zu"? (dann Skalenfrage)
      if(TRUE %in% str_detect(names(attributes(sufdat[[i]])$labels), 
                              "rifft zu") == F) {
        
        # Trifft-nicht-zu-Wert abgreifen:  
        valuelabels <- attributes(sufdat[[i]])$labels
        value_tnz_alt <- valuelabels[
          str_detect(names(valuelabels), "TNZ")
          ]
        valuelabels[
          str_detect(names(valuelabels), "TNZ")
          ] <- -6
        attributes(sufdat[[i]])$labels <- valuelabels
        sufdat[[i]][sufdat[[i]] == value_tnz_alt] <- -6
        
      }
    }
    
    # Prüfung: Gibt es ein Label für "verweigert"?
    if(TRUE %in% str_detect(names(attributes(sufdat[[i]])$labels), 
                            fixed("verweigert", ignore_case = T))) {
        
        # Verweigerungs-Wert abgreifen:  
        valuelabels <- attributes(sufdat[[i]])$labels
        value_verw_alt <- valuelabels[
          str_detect(names(valuelabels), fixed("verweigert", ignore_case = T))
          ]
        if(nchar(names(value_verw_alt)) == 10) {
        valuelabels[
          fixed("verweigert", ignore_case = T)
          ] <- -7
        attributes(sufdat[[i]])$labels <- valuelabels
        sufdat[[i]][sufdat[[i]] == value_verw_alt] <- -7
        }
    }
    
    # Prüfung: Gibt es ein Label für "weiß nicht"?
    if(TRUE %in% str_detect(names(attributes(sufdat[[i]])$labels), 
                            fixed("weiß nicht", ignore_case = T))) {
      
      # Weiß nicht-Wert abgreifen:  
      valuelabels <- attributes(sufdat[[i]])$labels
      value_wn_alt <- valuelabels[
        str_detect(names(valuelabels), fixed("weiß nicht", ignore_case = T))
        ]
      if(nchar(names(value_wn_alt)) == 10) {
        valuelabels[
          fixed("weiß nicht", ignore_case = T)
          ] <- -8
        attributes(sufdat[[i]])$labels <- valuelabels
        sufdat[[i]][sufdat[[i]] == value_wn_alt] <- -8
      }
    }
    
    # Ende der if-Bedingung
  }
  
# Ende der for-Schleife
}



# Missings für Fragebogenversion und Filter einführen ----

for(i in 1:ncol(sufdat)) {
  
  if((FALSE %in% names(table(is.na(sufdat[[i]][sufdat$langkurz == 2])))) == F)  {
    
  valuelabels <- attr(sufdat[[i]], "labels")
  
  valuelabels <- c(valuelabels, -4)
  names(valuelabels)[valuelabels == -4] <- 
    "Item in Fragebogenversion nicht erhoben"
  
  attr(sufdat[[i]], "labels") <- sort(valuelabels)
  
  sufdat[[i]][is.na(sufdat[[i]]) == T & sufdat$langkurz == 2] <- -4
  
  }
}

# Missings durch Filterführung
for(i in 1:ncol(sufdat)) {
  
  if(TRUE %in% names(table(is.na(sufdat[[i]]))))  {
    
    valuelabels <- attr(sufdat[[i]], "labels")
    
    valuelabels <- c(valuelabels, -5)
    names(valuelabels)[valuelabels == -5] <- 
      "Missing durch Filterführung"
    
    attr(sufdat[[i]], "labels") <- sort(valuelabels)
    
    sufdat[[i]][is.na(sufdat[[i]]) == T] <- -5
    
  }
}


