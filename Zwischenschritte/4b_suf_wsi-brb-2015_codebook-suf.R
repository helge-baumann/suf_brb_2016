# Codebook SUF

# Fragebogen Link von Variablen zu Fragen----
# Fragebogen einlesen (von Word als .txt, danach bearbeitet)
#fb <- readLines("./Hintergrund/Fragebogen_Betriebsrätebefragung_2015_text.txt")

# Link zwischen Variablen und Fragen im Fragebogen
fb_link <- data.frame(vars = names(sufdat), fragen = NA, roh=NA)

# Fragen (code) auslesen
fragen <- str_remove(
  str_extract(fb, "^[:alpha:][:digit:].?.?.?.?.?.?:"),
  ":"
)
fragen <- fragen[complete.cases(fragen)]

# Fragen mit Variablen verlinken
for(i in 1:nrow(fb_link)) {
  
  # wenn Var = Frage: dann fb_link$fragen = var
  # wenn es keine Entsprechung gibt: letzten Unterstrich plus Zusatz entfernen
  # oder bei Order umgekehrt.
  if(tolower(fb_link[i,1]) %in% tolower(fragen)) {
    
    fb_link[i,2] <- as.character(fb_link[i,1])
    
  } else { # wenn Variable keiner Frage exakt entspricht
    
    if(str_detect(fb_link[i,1], fixed("_"))) {
      
      if(str_detect(fb_link[i,1], fixed("Order", ignore_case=TRUE))) {
        
        # nur LETZTEN Unterstrich entfernen!
        fb_link[i,2] <- str_remove(fb_link[i,1], "[^\\_]+\\_")
        
      }
      
      # Wenn die variable auf eine Frage passt (ignore case)
      if(str_detect(fb_link[i,1], "\\_[:alnum:][:alnum:]?$")) {
        
        # LETZTEN Unterstrich entfernen
        x <- str_remove(fb_link[i,1], "\\_[:alnum:][:alnum:]?$")
        
        # Passt es JETZT?
        if(tolower(x) %in% tolower(fragen)) {
          
          fb_link[i,2] <- x 
          
        }
      }
      
      # Wenn die variable vergröbert ist
      if(str_detect(fb_link[i,1], "\\_kat")) {
        
        # LETZTEN Unterstrich entfernen
        x <- str_remove(fb_link[i,1], "\\_kat")
        
        # Passt es JETZT?
        if(tolower(x) %in% tolower(fragen)) {
          
          fb_link[i,2] <- x 
          
        }
      }
      
      # Wenn die Variable generiert ist
      if(str_detect(fb_link[i,1], "\\_gen")) {
        
        # LETZTEN Unterstrich entfernen
        x <- str_remove(fb_link[i,1], "\\_gen")
        
        # Passt es JETZT?
        if(tolower(x) %in% tolower(fragen)) {
          
          fb_link[i,2] <- x 
          
        } else {
          
          x <- str_remove(x, "\\_[:alnum:][:alnum:]?$")
          fb_link[i,2] <- x 
          
        }
      }
    }
  }
}

# Ausnahmen manuell bestimmen
fb_link[fb_link[,1] == "K12b_1_1",2] <- "K12"
fb_link[fb_link[,1] == "K12b_1_2",2] <- "K12"
fb_link[fb_link[,1] == "P3_o_5",2] <- "P3"
fb_link[fb_link[,1] == "D1b_1",2] <- "D1b"
fb_link[is.na(fb_link[,2]) == T,2] <- 
  as.character(fb_link[is.na(fb_link[,2]) == T,1]) 



# Codebuch erzeugen ----
Codebook_SUF <- data.frame(
  "Variable" = NA,
  "Bezeichnung und Labels" = NA,
  "Schlüssel" = NA,
  "n" = NA, 
  "Frage(n)" = NA
)

# Vorarbeit: Variablenlabels sortieren, so dass Missings als Letztes
for(i in 1:ncol(sufdat)) {
  
  if(is.null(attr(sufdat[[i]], "labels"))==F) {
    
    x <- attr(sufdat[[i]], "labels")
    x <- c(sort(x[x>=0]), sort(x[x<0]))
    attr(sufdat[[i]], "labels") <- x
    
  }
  
}


# Variablen aus Generierungsindex----
gen1 <- str_extract_all(Gen[,1], "label\\{(.*?)\\}", simplify=T)
gen1 <- str_remove(gen1, "label\\{")
gen1 <- str_remove(gen1, "\\}")
gen1 <- str_replace_all(gen1, ":", "\\_")
for(i in 1:length(gen1)) if(is.na(gen1[i])==T) gen1[i] <- gen1[i-1]

gen2 <- list()
num <- 0

for(b in gen1) {
  
  num <- num+1
  
  index <- which(gen1 == b)
  
  vars <- str_extract_all(Gen[index,2], "\\{(.*?)\\}", simplify=T)
  vars <- str_remove(vars, "\\{")
  vars <- str_remove(vars, "\\}")
  #vars <- str_replace_all(vars, ":", "\\_")
  vars <- unique(vars)
  
  gen2[[b]] <- vars
  
}

# in Codebook_Rawdata: 1. und 5. Spalte
#\\textbf{C0\\_1}


num <- 0

vars <- str_extract_all(Codebook_Rawdata[,1], "textbf\\{(.*?)\\}", simplify=T)
vars <- str_remove(vars, "textbf\\{")
vars <- str_remove(vars, "\\}")
fragen <- Codebook_Rawdata[,5]
gen3 <- data.frame(vars, fragen)

gen3 <- unique(gen3)

#----

# Startzähler (für die Zeilen)
num <- 0
x <- 0
for(i in 1:ncol(sufdat)) {
  
  num <- num+1
  
  # Variablenname
  Codebook_SUF[num,1] <- paste0(
    "\\textbf{", names(sufdat)[i], "}",
    "\\phantomsection\\label{",
    "var:suf:",
    str_replace_all(names(sufdat)[i], fixed("_"), ":"),
    "}"
  )
  
  # Variablenlabel laut Datensatz (mit Fehlern)
  Codebook_SUF[num,2] <- paste0(
    "\\textbf{", attr(sufdat[[i]], "label"), "}"
  )
  
  frage  <- fb_link$fragen[fb_link$vars == names(sufdat)[i]]
  
  # welche Frage
  if(frage != "lfd" 
     & str_detect(attr(sufdat[[i]], "label"), "generiert")==F
     & str_detect(attr(sufdat[[i]], "label"), "Interviewernummer")==F
     & str_detect(attr(sufdat[[i]], "label"), "Gewichtungsfaktor")==F
     & str_detect(attr(sufdat[[i]], "label"), "WZ 2008")==F
     & str_detect(attr(sufdat[[i]], "label"), "WESTOST")==F
     ) {
    Codebook_SUF[num,5] <- paste0(
      "\\hyperref[",
      str_replace_all(frage, fixed("_"), ":"),
      "]{",
      frage,
      "}"
    )
  }
  
  # Für generierte Variablen etwas komplizierter
  if(str_detect(attr(sufdat[[i]], "label"), "generiert")) {
      
    a <- names(sufdat)[i]
    var.link <- gen2[[a]]
    var.link2 <- gen3[gen3$vars %in% var.link,2]
    
    Codebook_SUF[num,5] <- paste(var.link2, collapse=", ")
    
  }
  
  num <- num+1
  
  values      <- na.omit(unique(sort(sufdat[[i]])))
  valuelabels <- attr(sufdat[[i]], "labels")
  
  # Einträge für Variablen mit Wertelabels
  if(is.null(valuelabels)==FALSE) {
    
    # numerische Variablen (zB Betriebsgröße)
    if(length(values) > length(valuelabels)) {
      
      num <- num+1
      
      if(any(str_detect(values, ".")==T)) {
        Codebook_SUF[num,2] <- paste0(
          round(range(values[!values %in% valuelabels])[1], digits=2),
          "--",
          round(range(values[!values %in% valuelabels])[2], digits=2)
        )
        Codebook_SUF[num,3] <- paste0(
          round(range(values[!values %in% valuelabels])[1], digits=2),
          "--",
          round(range(values[!values %in% valuelabels])[2], digits=2)
        )
      }
      
      if(any(str_detect(values, ".")==F)) {
        Codebook_SUF[num,2] <- paste0(
          range(values[!values %in% valuelabels])[1],
          "--",
          range(values[!values %in% valuelabels])[2]
        )
        Codebook_SUF[num,3] <- paste0(
          range(values[!values %in% valuelabels])[1],
          "--",
          range(values[!values %in% valuelabels])[2]
        )
      }
      
      # Häufigkeit numerischer Angaben (ohne gelabelte Werte, zB missings)
      Codebook_SUF[num, 4] <- length(
        na.omit(sufdat[[i]][!sufdat[[i]] %in% valuelabels])
      )
      
      num <- num+1
      
      # Gelabelte Werte (hauptsächlich Missings)
      Codebook_SUF[num:(num+length(valuelabels)-1),2] <- names(valuelabels)
      Codebook_SUF[num:(num+length(valuelabels)-1),3] <- round(valuelabels,
                                                                   digits=0)
      # Häufigkeiten gelabelter Werte
      Codebook_SUF[num:(num+length(valuelabels)-1),4] <- 
        unlist(lapply(valuelabels, 
                      function(x) 
                        length(na.omit(sufdat[[i]][sufdat[[i]] == x])))
        )
      
      num <- num+length(valuelabels)-1
      
      # Jetzt vollständig gelabelte Variablen (kategoriale Var.)
    } else {
      
      num <- num+1
      Codebook_SUF[num:(num+length(valuelabels)-1),2] <- names(valuelabels)
      Codebook_SUF[num:(num+length(valuelabels)-1),3] <- round(valuelabels,
                                                                   digits=0)
      # Häufigkeiten der Werte
      Codebook_SUF[num:(num+length(valuelabels)-1),4] <- 
        unlist(lapply(valuelabels, 
                      function(x) 
                        length(na.omit(sufdat[[i]][sufdat[[i]] == x])))
        )
      
      num <- num+length(valuelabels)-1
    }
    # komplett ungelabelte Variablen
  } else {
    # numerische ungelabelte Var. (zB laufende Nummer)
    if(str_detect(attr(sufdat[[i]], "label"), "offen") == F) {
      
      num <- num+1
      
      Codebook_SUF[num,2] <- paste0(
        round(range(values)[1], digits=2),
        "--",
        round(range(values)[2], digits=2)
      )
      Codebook_SUF[num,3] <- paste0(
        round(range(values)[1], digits=2),
        "--",
        round(range(values)[2], digits=2)
      )
      
      Codebook_SUF[num,4] <- 
        length(na.omit(sufdat[[i]]))
    } else {
      Codebook_SUF[num,2] <- "offene Angabe"
      Codebook_SUF[num,4] <- length(na.omit(sufdat[[i]]))
    }
  }
  
 
}

# Unterstriche einführen
for(i in 1:nrow(Codebook_SUF)) {
  for(j in 1:ncol(Codebook_SUF)) {
    Codebook_SUF[i,j] <- 
      str_replace_all(
        Codebook_SUF[i,j], 
        fixed("_"), "\\_")
  }
}

# Unterstriche einführen
for(i in 1:nrow(Codebook_SUF)) {
  for(j in 1:ncol(Codebook_SUF)) {
    Codebook_SUF[i,j] <- 
      str_replace_all(
        Codebook_SUF[i,j], 
        fixed("\\\\_"), "\\_")
  }
}

# Prozente einführen
for(i in 1:nrow(Codebook_SUF)) {
  for(j in 1:ncol(Codebook_SUF)) {
    Codebook_SUF[i,j] <- 
      str_replace_all(
        Codebook_SUF[i,j], 
        fixed("%"), "\\%")
  }
}


Codebook_SUF[,1] <- 
  str_replace(Codebook_SUF[,1], "Interviewdauer", "Interview\\-dauer")

# subsections()
num1 <- 0
num2 <- 0
index <- which(!is.na(Codebook_SUF[,1]))
vars <- str_remove(str_remove(Codebook_SUF[index,1], "\\\\textbf\\{"), "\\}.*")
var_ref <- vars[1]
ind_ref <- index[1]

for(b in vars) {
  
  num1 <- num1+1
  num2 <- num2+1
  
  firstletter <- str_extract(b, "^.")
  firstletterbefore <- str_extract(vars[num1-1], "^.")
  
  if(identical(firstletter == firstletterbefore, F) & num2 > 30) {
    
    Codebook_SUF[ind_ref+1,2] <- 
      paste0(
        "\\protect\\subsection[Variablen ",
        var_ref, " bis " , vars[num1], "]", "{}"
      )
    
    num2 <- 0
    var_ref <- vars[num1+1]
    ind_ref <- index[num1+1]
  }
  
}
  

# in ein xtable überführen (Latex-Format)
Codebook_tex <- xtable(Codebook_SUF, digits=0)

for(i in 1:nrow(Codebook_tex)) {
  for(j in 1:ncol(Codebook_tex)) {
    Codebook_tex[i,j] <- 
      str_replace_all(
        Codebook_tex[i,j], 
        fixed("e+05"), "00000")
  }
}

# An welchen Stellen sollen midrules eingefügt werden?
midrules <- which(is.na(Codebook_SUF[,1])==F)-1
midrules <- midrules[-1]

print.xtable(Codebook_tex, 
             include.rownames=F,
             sanitize.text.function = identity,
             only.contents=T,
             booktabs=T,
             hline.after=midrules,
             include.colnames=F, 
             file="./Berichte/Codebook/Codebook_SUF.tex"
) 
