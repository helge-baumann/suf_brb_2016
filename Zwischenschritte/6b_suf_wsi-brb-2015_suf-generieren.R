# SUF erzeugen: Variablen generieren

# Anteilswerte von Beschäftigtenangaben

# Für welche Variablen sollen Anteile berechnet werden? ----
prozent <- NA

for(i in 1:ncol(sufdat)) {
  
  if(length(attr(sufdat[[i]], "labels")) > 0) {
    prozent[i] <-  any(
      str_detect(
        string=names(
          attr(
          sufdat[[i]], "labels"
          )), 
        pattern=regex("Angabe in Prozent", ignore_case=T)
        )) & names(sufdat)[i] != "D5a"
       
  }
}

# Index für Variablen, für die Anteilswerte berechnet werden sollen
prozent <- which(prozent == TRUE)+1


# Vorarbeit: Anzahl auf 0, wenn Label  zuvor "Vorhandensein" enthält ----
for(i in prozent) {
  
  if(str_detect(
    string=attr(sufdat[[i-2]], "label"), 
    pattern=regex("Vorhandensei", ignore_case=T)
    )) {

   sufdat[[i-1]][sufdat[[i-2]] == "nein"] <- 0
  
  }
}

# Anteile berechnen ----
for(i in prozent) {
  
  # neue Variable generieren
  
  # neuer Variablenname
  newname <- paste0(
    names(sufdat[i]),
    "_gen")
  
  # vorläufig wird die neue Variable aus der urspr. Anteilsvariable gespeist
  sufdat[[newname]] <- sufdat[[i]]
  
  # Variablenlabel wird erweitert
  attr(sufdat[[newname]], "label") <- paste0(
    attr(sufdat[[i]], "label"), " (in Prozent, generierte Variable)"
  )
  
  # Werte ersetzen, wenn absolute Angabe
  sufdat[[newname]][
    sufdat$D1 >= 0 &
      sufdat[[i-1]] >= 0 &
      sufdat[[i-1]] < 999996
  ] <- sufdat[[i-1]][
    sufdat$D1 >= 0 &
      sufdat[[i-1]] >= 0 &
      sufdat[[i-1]] < 999996
  ]/sufdat$D1[
    sufdat$D1 >= 0 &
      sufdat[[i-1]] >= 0 &
      sufdat[[i-1]] < 999996
    ]*
    100
  
  # Missings
  sufdat[[newname]][
    (sufdat$D1 == -8 | sufdat[[i-1]]== -8) &
    sufdat$D1 != -7 & sufdat[[i-1]] != -7 &
    sufdat[[i-1]] != -5 & # Filter
    sufdat[[i-1]] != -4  # Fragebogen
    ] <- -8
  sufdat[[newname]][
    (sufdat$D1 == -7 | sufdat[[i-1]]== -7) &
      sufdat[[i-1]] != -5 & # Filter
      sufdat[[i-1]] != -4  # Fragebogen
    ] <- -7
  
}


## Anteilswerte von Betriebsratsangaben ----
br_anteil <- NA
for(i in 1:ncol(sufdat)) {
  
  br_anteil[i] <-  str_detect(
      string=attr(sufdat[[i]], "label"), 
      pattern=regex("Anzahl der", ignore_case=T)
    ) &
    str_detect(
      string=attr(sufdat[[i]], "label"), 
      pattern=regex("Betriebsratsmitglieder", ignore_case=T)
    ) &
    nchar(attr(sufdat[[i]], "label")) > 33
    
  }

br_anteil <- which(br_anteil == TRUE)

for(i in br_anteil) {
  
  # neue Variable generieren
  
  # neuer Variablenname
  newname <- paste0(
    names(sufdat[i]),
    "_gen")
  
  # vorläufig wird die neue Variable aus der urspr. Anteilsvariable gespeist
  sufdat[[newname]] <- sufdat[[i]]
  
  # Variablenlabel wird erweitert
  attr(sufdat[[newname]], "label") <- paste0(
    attr(sufdat[[i]], "label"), " (in Prozent, generierte Variable)"
  )
  attr(sufdat[[newname]], "label") <- str_replace(
    attr(sufdat[[newname]], "label"), "Anzahl", "Anteil"
  )
  
  # Werte ersetzen, wenn absolute Angabe
  sufdat[[newname]][
    sufdat$M3 >= 0 &
      sufdat[[i]] >= 0 
    ] <- sufdat[[i]][
      sufdat$M3 >= 0 &
        sufdat[[i]] >= 0 
      ]/sufdat$M3[
        sufdat$M3 >= 0 &
          sufdat[[i]] >= 0 
        ]*
    100
  
  # Missings
  sufdat[[newname]][
    (sufdat$M3 == -8 | sufdat$M3 == -7) &
      sufdat[[i]] != -5 & # Filter
      sufdat[[i]] != -4  # Fragebogen
    ] <- sufdat$M3[
      (sufdat$M3 == -8 | sufdat$M3 == -7) &
        sufdat[[i]] != -5 & # Filter
        sufdat[[i]] != -4  # Fragebogen
      ] 
}

## Latex für Erklärung der Berechnung (siehe Fragebogen?)----

# welche Variablen generiert?

index_gen <- which(str_detect(names(sufdat), fixed("_gen")))

num <- 1

Gen <- data.frame(
  r1=NA, 
  r2=NA,
  r3=NA)

for(i in index_gen) { # Index gen. Var.
  
  # erste Spalte: Name generierter Variable
  Gen[num,1] <- paste0(
    "\\textbf{",
    names(sufdat)[i],
    "}\\phantomsection\\label{",
    str_replace_all(names(sufdat)[i], fixed("_"), ":"),
  "}"
  )
  
  
  # Weitere Spalten
  
  # Variablenname
  name_gen <- names(sufdat)[i]
  name_urspr <- str_remove(names(sufdat)[i], fixed("_gen"))
    
    # Prozentangaben----
    if(any(names(sufdat)[prozent]==name_urspr)) {
      
      # Index Ursprung
      i_temp <- which(names(sufdat) == name_urspr)
      
      # zweite Spalte: Generierung
      # Fall I: "Vorhandensein" abgefragt
      if(str_detect(
        string=attr(sufdat[[i_temp-2]], "label"), 
        pattern=regex("Vorhandensei", ignore_case=T)
      )) {
        
      Gen[num,2] <- paste0(
      "\\hyperref[var:",
      str_replace_all(names(sufdat)[i_temp-2], fixed("_"), ":"),
      "]{",
      names(sufdat)[i_temp-2],
      "} = \"nein\""
    )
    
    # dritte Spalte: DANN
    Gen[num,3] <- paste0(
      "\\hyperref[var:",
      str_replace_all(name_gen, fixed("_"), ":"),
      "]{",
      name_gen,
      "} = 0"
    )
    num <- num+1
    }
      
    # weitere Schritte (gleich)
      Gen[num,2] <- paste0(
        "\\hyperref[var:",
        str_replace_all(names(sufdat)[i_temp-1], fixed("_"), ":"),
        "]{",
        names(sufdat)[i_temp-1],
        "} $  <  $ 999996 \\& ",
        "\\hyperref[var:D1]{D1} $ < $ 999997"
      )
      
      # dritte Spalte: DANN
      Gen[num,3] <- paste0(
        name_gen,
        " = $ \\frac{", # Hier Fehler
        "\\hyperref[var:",
        str_replace_all(names(sufdat)[i_temp-1], fixed("_"), ":"),
        "]{",
        names(sufdat)[i_temp-1],
        "}}{",
        "\\hyperref[var:D1]{D1}",
        "} $"
        
      )
    
      num <- num+1
    
      # dritter Vorgang Anteilsvariablen
      Gen[num,2] <- paste0(
        "(",
        "\\hyperref[var:",
        str_replace_all(names(sufdat)[i_temp-1], fixed("_"), ":"),
        "]{",
        names(sufdat)[i_temp-1],
        "} = \" weiß nicht\" \\xspace ODER ",
        "\\hyperref[var:D1]{D1} = \"weiß nicht\") \\& ",
        "\\hyperref[var:",
        str_replace_all(names(sufdat)[i_temp], fixed("_"), ":"),
        "]{",
        names(sufdat)[i_temp],
        "} $ \\neq $ \"verweigert\" \\& ",
        "\\hyperref[var:",
        str_replace_all(names(sufdat)[i_temp], fixed("_"), ":"),
        "]{",
        names(sufdat)[i_temp],
        "} $ \\neq $ \\grqq Angabe in Prozent\" \\& ",
        "\\hyperref[var:D1]{D1} $ \\neq $ \"verweigert\" "
        
      )
      
      # dritte Spalte: DANN
      Gen[num,3] <- paste0(
        name_gen,
        " = -8"
      )
      
      num <- num+1
      
      # vierter Vorgang Anteilsvariablen
      Gen[num,2] <- paste0(
        "(",
        "\\hyperref[var:",
        str_replace_all(names(sufdat)[i_temp], fixed("_"), ":"),
        "]{",
        names(sufdat)[i_temp],
        "} = \"verweigert\" \\xspace ODER ",
        "\\hyperref[var:D1]{D1} = \"verweigert\") \\& ",
        "\\hyperref[var:",
        str_replace_all(names(sufdat)[i_temp], fixed("_"), ":"),
        "]{",
        names(sufdat)[i_temp],
        "} $ \\neq $ \\grqq Angabe in Prozent\" "
        
      )
      
      # vierte Spalte: DANN
      Gen[num,3] <- paste0(
        name_gen,
        " = -7"
      )
      
      num <- num+1
      
      # fünfter Vorgang Anteilsvariablen
      Gen[num,2] <- paste0(
        "\\hyperref[var:",
        str_replace_all(names(sufdat)[i_temp], fixed("_"), ":"),
        "]{",
        names(sufdat)[i_temp],
        "} = \\grqq Angabe in Prozent\" "
        
      )
      
      # fünfte Spalte: DANN
      Gen[num,3] <- paste0(
        name_gen,
        " = ",
        "\\hyperref[var:",
        str_replace_all(names(sufdat)[i_temp], fixed("_"), ":"),
        "]{",
        names(sufdat)[i_temp],
        "}"
      )
      
      num <- num+1
      
    }
  
  # BR-angaben----
  if(any(names(sufdat)[br_anteil]==name_urspr)) {
    
    # Index Ursprung
    i_temp <- which(names(sufdat) == name_urspr)
    
    
    # Schritt I
    Gen[num,2] <- paste0(
      "\\hyperref[var:",
      str_replace_all(names(sufdat)[i_temp], fixed("_"), ":"),
      "]{",
      names(sufdat)[i_temp],
      "} $ \\neq $ \" weiß nicht\" \\xspace \\& ",
      "\\hyperref[var:",
      str_replace_all(names(sufdat)[i_temp], fixed("_"), ":"),
      "]{",
      names(sufdat)[i_temp],
      "} $ \\neq $ \" verweigert\" \\xspace \\& ",
      "\\hyperref[var:M3]{M3} $ \\neq $ \"verweigert\" \\& ",
      "\\hyperref[var:M3]{M3} $ \\neq $ \"weiß nicht\" \\& "
      
      
    )
    
    # dritte Spalte: DANN
    Gen[num,3] <- paste0(
      name_gen,
      " = $ \\frac{",
      "\\hyperref[var:",
      str_replace_all(names(sufdat)[i_temp], fixed("_"), ":"),
      "]{",
      names(sufdat)[i_temp],
      "}}{",
      "\\hyperref[var:M3]{M3} ",
      "} $"
    )
    
    num <- num+1
    
    # Schritt II
    Gen[num,2] <- paste0(
      "(",
      "\\hyperref[var:M3]{M3} = \"verweigert\" \\xspace ODER ",
      "\\hyperref[var:M3]{M3} = \"weiß nicht\" ",
      ") \\& ",
      "\\hyperref[var:",
      str_replace_all(names(sufdat)[i_temp], fixed("_"), ":"),
      "]{",
      names(sufdat)[i_temp],
      "} $ \\neq $ \\textit{missing by design}"
    )
    
    # dritte Spalte: DANN
    Gen[num,3] <- paste0(
      name_gen,
      " = ",
      "\\hyperref[var:M3]{M3}"
    )
    
    num <- num+1
    
  }
  
  
    
}
  
# print ----

# Unterstriche einführen
for(i in 1:nrow(Gen)) {
  for(j in 1:ncol(Gen)) {
    Gen[i,j] <- 
      str_replace_all(
        Gen[i,j], 
        fixed("_"), "\\_")
  }
}

midrules <- which(is.na(Gen[,1])==F)-1
midrules <- midrules[-1]

Gen_tex <- xtable(Gen)

print.xtable(Gen_tex, 
             include.rownames=F,
             sanitize.text.function = identity,
             only.contents=T,
             booktabs=T,
             hline.after=midrules,
             include.colnames=F, 
             file="./Berichte/Codebook/Generierte_Variablen.tex"
) 




