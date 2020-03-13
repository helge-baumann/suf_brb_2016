# Codebook Rawdata

# für Link zwischen Roh und SUF----
options(encoding="Windows-1252")
fr_b <- read.table("./Hintergrund/bereinigung_suf.txt", sep="\t", header=T)[1:3]
fr_b[,2] <- stri_enc_toutf8(fr_b[,2])
options(encoding="UTF-8")

Codebook_Rawdata <- data.frame(
  "Frage" = NA,
  "Bezeichnung und Labels" = NA,
  "Schlüssel" = NA,
  "n" = NA, 
  "Frage" = NA,
  "SUF" = NA
  )

# Startzähler (für die Zeilen)
num <- 0
#x <- 0 # notwendig?

for(i in 1:ncol(rawdat)) {
 
  num <- num+1
  
  # Variablenname mit Link
  Codebook_Rawdata[num,1] <- paste0(
    "\\textbf{", names(rawdat)[i], "}",
    "\\phantomsection\\label{var:",
    str_replace_all(names(rawdat)[i], fixed("_"), ":"),
    "}"
    )
 
  # Variablenlabel laut Datensatz (mit Fehlern)
  Codebook_Rawdata[num,2] <- paste0(
    "\\textbf{", attr(rawdat[[i]], "label"), "}"
  )
  
  frage  <- fb_link$fragen[fb_link$vars == names(rawdat)[i]]
  
  # welche Frage
  if(!is.na(frage)) {
    
    Codebook_Rawdata[num,5] <- paste0(
      "\\hyperref[",
      str_replace_all(frage, fixed("_"), ":"),
      "]{",
      frage,
      "}"
    )
  
  }
  
  # welche Variable im SUF
  sufvar <- c(which(names(sufdat) == names(rawdat[i])), 
    which(names(sufdat) == paste0(names(rawdat[i]), "_gen")), 
    which(names(sufdat) == paste0(names(rawdat[i]), "_kat"))
    )
  
    Codebook_Rawdata[num,6] <- paste0(
      "\\hyperref[var:suf:",
      str_replace_all(names(sufdat)[sufvar], fixed("_"), ":"),
      "]{",
      names(sufdat)[sufvar],
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
      if(!str_detect(attr(rawdat[[i]], "label"), "offen")) {
        
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
# Unterstriche einführen, Silbentrennung----
Codebook_Rawdata <- data.frame(lapply(Codebook_Rawdata, str_replace_all, fixed("_"), "\\_"))

Codebook_Rawdata[,1] <- 
  str_replace(Codebook_Rawdata[,1], "Interviewdauer", "Interview\\-dauer")
Codebook_Rawdata[,2] <- str_replace(Codebook_Rawdata[,2], fixed("e+05"), "00000")
Codebook_Rawdata[,3] <- str_replace(Codebook_Rawdata[,3], fixed("e+05"), "00000")

# in ein xtable überführen (Latex-Format)
Codebook_tex <- xtable(Codebook_Rawdata, digits=0)

# An welchen Stellen sollen midrules eingefügt werden?
midrules <- which(is.na(Codebook_Rawdata[,1])==F)-1
midrules <- midrules[-1]

print.xtable(Codebook_tex, 
             include.rownames=F,
             sanitize.text.function = identity,
             only.contents=T,
             booktabs=T,
             hline.after=midrules,
             include.colnames=F, 
             file="./Berichte/Codebook/Codebook_Rawdata.tex"
             ) 
