# Das eigentliche SUF erzeugen
dir.create("./Output/Datensaetze", showWarnings = F)

# SPSS----
write_sav(
  sufdat,
  paste0(
    "./Output/Datensaetze/",
    format(Sys.time(), "%Y-%m-%d_%H%M%S"),
    "_SUF_WSI-Betriebsraetebefragung_2015",
    ".sav"
  )
)

# Stata----
write_dta(
  sufdat,
  paste0(
    "./Output/Datensaetze/",
    format(Sys.time(), "%Y-%m-%d_%H%M%S"),
    "_SUF_WSI-Betriebsraetebefragung_2015",
    ".dta"
  )
)