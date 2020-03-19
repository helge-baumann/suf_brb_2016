# Übertrag aus 2015 (Angaben BR)
dat15 <- read_dta(
  paste0(
    "./Input/Daten/",
    "infas_Befragungsdaten_Betriebsrätepanel_4808_20150518_1",
    "_Korrektur-R1a", # korrigierte Fassung (ohne Duplikate) verwenden
    ".dta"
  )
)
dat15 <- as_tibble(lapply(dat15, replace_missings))

## Anteilswerte von Betriebsratsangaben aus 2015 ----
br_anteil <- NA
for(i in 1:ncol(dat15)) {
  
  br_anteil[i] <-  str_detect(
    string=attr(dat15[[i]], "label"), 
    pattern=regex("Anzahl der", ignore_case=T)
  ) &
    str_detect(
      string=attr(dat15[[i]], "label"), 
      pattern=regex("Betriebsratsmitglieder", ignore_case=T)
    ) &
    nchar(attr(dat15[[i]], "label")) > 33
  
}

br_anteil <- which(br_anteil == TRUE)

for(i in br_anteil) {
  
  # neue Variable generieren
  
  # neuer Variablenname
  newname <- paste0(
    names(dat15[i]),
    "_gen")
  
  # vorläufig wird die neue Variable aus der urspr. Anteilsvariable gespeist
  dat15[[newname]] <- dat15[[i]]
  
  # Variablenlabel wird erweitert
  attr(dat15[[newname]], "label") <- paste0(
    attr(dat15[[i]], "label"), " (in Prozent, generierte Variable)"
  )
  attr(dat15[[newname]], "label") <- str_replace(
    attr(dat15[[newname]], "label"), "Anzahl", "Anteil"
  )
  
  # Werte ersetzen, wenn absolute Angabe
  dat15[[newname]][
    dat15$M3 >= 0 &
      dat15[[i]] >= 0 
  ] <- dat15[[i]][
    dat15$M3 >= 0 &
      dat15[[i]] >= 0 
  ]/dat15$M3[
    dat15$M3 >= 0 &
      dat15[[i]] >= 0 
  ]*
    100
  
  # Missings
  dat15[[newname]][
    (dat15$M3 == -8 | dat15$M3 == -7) &
      dat15[[i]] != -5 & # Filter
      dat15[[i]] != -4  # Fragebogen
  ] <- dat15$M3[
    (dat15$M3 == -8 | dat15$M3 == -7) &
      dat15[[i]] != -5 & # Filter
      dat15[[i]] != -4  # Fragebogen
  ] 
}


## Übertrag aus 2015----

# welche Variablen gehören zusammen?
match <- data.frame(
  x=c("G7", "G10", "G10a", "G10c", "G10d", "G10e", "G10f", "G11", "G12", "G13", 
      "G14", "G15"),
  y=c("M2", "M6", "M6a", "M6c", "M6e", "M6f", "M6g", "M7", "M8", "M10", "M12", 
      "M13"),
  stringsAsFactors = F
)

cat("Fehler in G12a: Nur gefragt, wenn G6 != 2!")

sufdat <- left_join(sufdat, dat15[c("lfd", match$y)], by="lfd")

for(b in match$x) {
  
  var15 <- match$y[match$x == b]
  sufdat[b][sufdat$G6 == 2,] <- sufdat[var15][sufdat$G6 == 2,]
  
}
