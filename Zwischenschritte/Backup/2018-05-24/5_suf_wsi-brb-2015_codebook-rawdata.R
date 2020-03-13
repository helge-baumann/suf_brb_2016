# Codebook Rawdata

Codebook_Rawdata <- data.frame(
  "Frage" = NA,
  "Bezeichnung und Labels" = NA,
  "Schlüssel" = NA,
  "n" = NA, 
  "Frage" = NA
  )

# Startzähler (für die Zeilen)
num <- 0
x <- 0

for(i in 1:ncol(rawdat)) {
 
  num <- num+1
  
  # Variablenname
  Codebook_Rawdata[num,1] <- paste0(
    "\\textbf{", names(rawdat)[i], "}"
    )
 
  # Variablenlabel laut Datensatz (mit Fehlern)
  Codebook_Rawdata[num,2] <- paste0(
    "\\textbf{", attr(rawdat[[i]], "label"), "}"
  )
  
  # In welcher Fragebogenversion wurde Frage gestellt?
  #if(all(is.na(rawdat[[i]][rawdat$langkurz==2])==T)) {
    #Codebook_Rawdata[num,5] <- "Langversion"
    #} else {
    #  Codebook_Rawdata[num,5] <- "Alle"
#}
  frage  <- fb_link$fragen[fb_link$vars == names(rawdat)[i]]
  
  # welche Frage?
  Codebook_Rawdata[num,5] <- paste0(
    "\\hyperref[",
    str_replace_all(frage, fixed("_"), ":"),
    "]{",
    frage,
    "}"
  )
  
  num <- num+1
  
  values      <- na.omit(unique(sort(rawdat[[i]])))
  valuelabels <- attr(rawdat[[i]], "labels")
  
  # Einträge für Variablen mit Wertelabels
  if(is.null(valuelabels)==FALSE) {
    
    # numerische Variablen (zB Betriebsgröße)
    if(length(values) > length(valuelabels)) {
      
      num <- num+1
      
      if(any(str_detect(values, ".")==T)) {
        Codebook_Rawdata[num,2] <- paste0(
          round(range(values[!values %in% valuelabels])[1], digits=2),
          "--",
          round(range(values[!values %in% valuelabels])[2], digits=2)
        )
        Codebook_Rawdata[num,3] <- paste0(
          round(range(values[!values %in% valuelabels])[1], digits=2),
          "--",
          round(range(values[!values %in% valuelabels])[2], digits=2)
        )
      }
      
      if(any(str_detect(values, ".")==F)) {
      Codebook_Rawdata[num,2] <- paste0(
        range(values[!values %in% valuelabels])[1],
        "--",
        range(values[!values %in% valuelabels])[2]
      )
      Codebook_Rawdata[num,3] <- paste0(
        range(values[!values %in% valuelabels])[1],
        "--",
        range(values[!values %in% valuelabels])[2]
      )
      }
      
      # Häufigkeit numerischer Angaben (ohne gelabelte Werte, zB missings)
      Codebook_Rawdata[num, 4] <- length(
        na.omit(rawdat[[i]][!rawdat[[i]] %in% valuelabels])
      )
      
      num <- num+1
      
      # Gelabelte Werte (hauptsächlich Missings)
      Codebook_Rawdata[num:(num+length(valuelabels)-1),2] <- names(valuelabels)
      Codebook_Rawdata[num:(num+length(valuelabels)-1),3] <- round(valuelabels,
                                                                     digits=0)
      # Häufigkeiten gelabelter Werte
      Codebook_Rawdata[num:(num+length(valuelabels)-1),4] <- 
        unlist(lapply(valuelabels, 
                      function(x) 
                        length(na.omit(rawdat[[i]][rawdat[[i]] == x])))
               )
        
        num <- num+length(valuelabels)-1
        
        # Jetzt vollständig gelabelte Variablen (kategoriale Var.)
        } else {
          
          num <- num+1
          Codebook_Rawdata[num:(num+length(valuelabels)-1),2] <- names(valuelabels)
          Codebook_Rawdata[num:(num+length(valuelabels)-1),3] <- round(valuelabels,
                                                                     digits=0)
          # Häufigkeiten der Werte
          Codebook_Rawdata[num:(num+length(valuelabels)-1),4] <- 
            unlist(lapply(valuelabels, 
                        function(x) 
                          length(na.omit(rawdat[[i]][rawdat[[i]] == x])))
          )
          
          num <- num+length(valuelabels)-1
        }
    # komplett ungelabelte Variablen
    } else {
      # numerische ungelabelte Var. (zB laufende Nummer)
      if(str_detect(attr(rawdat[[i]], "label"), "offen") == F) {
        
        num <- num+1
      
        Codebook_Rawdata[num,2] <- paste0(
          round(range(values)[1], digits=2),
          "--",
          round(range(values)[2], digits=2)
          )
        Codebook_Rawdata[num,3] <- paste0(
          round(range(values)[1], digits=2),
          "--",
          round(range(values)[2], digits=2)
          )
        
        Codebook_Rawdata[num,4] <- 
        length(na.omit(rawdat[[i]]))
        } else {
          Codebook_Rawdata[num,2] <- "offene Angabe"
          Codebook_Rawdata[num,4] <- length(na.omit(rawdat[[i]]))
        }
      }
  
  num <- num+1
  
  Codebook_Rawdata[num,2] <- paste0(
    "\\textit{", "missing by design", "}"
  )
  Codebook_Rawdata[num,3] <- paste0(
    "\\textit{", ".", "}"
  )
  Codebook_Rawdata[num,4] <- 
    nrow(rawdat) - length(na.omit(rawdat[[i]]))
  }

# Unterstriche einführen
for(i in 1:nrow(Codebook_Rawdata)) {
  for(j in 1:ncol(Codebook_Rawdata)) {
    Codebook_Rawdata[i,j] <- 
      str_replace_all(
        Codebook_Rawdata[i,j], 
        fixed("_"), "\\_")
  }
}


Codebook_Rawdata[,1] <- 
  str_replace(Codebook_Rawdata[,1], "Interviewdauer", "Interview\\-dauer")

# in ein xtable überführen (Latex-Format)
Codebook_tex <- xtable(Codebook_Rawdata, digits=0)

for(i in 1:nrow(Codebook_tex)) {
  for(j in 1:ncol(Codebook_tex)) {
    Codebook_tex[i,j] <- 
      str_replace_all(
        Codebook_tex[i,j], 
        fixed("e+05"), "00000")
  }
}

# An welchen Stellen sollen midrules eingefügt werden?
midrules <- which(is.na(Codebook_Rawdata[,1])==F)-1

print.xtable(Codebook_tex, 
             include.rownames=F,
             sanitize.text.function = identity,
             only.contents=T,
             booktabs=T,
             hline.after=midrules,
             include.colnames=F, 
             file="./Berichte/Codebook/Codebook_Rawdata.tex"
             ) 

                                  