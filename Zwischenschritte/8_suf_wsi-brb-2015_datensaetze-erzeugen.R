# Das eigentliche SUF erzeugen
rm(list=setdiff(ls(), c("sufdat")))
dir.create("./Output/Datensaetze", showWarnings = F)

# SPSS----
write_sav(
  sufdat,
  paste0(
    "./Output/Datensaetze/",
    format(Sys.time(), "%Y-%m-%d"),
    "_SUF_WSI-Betriebsraetebefragung_2015",
    ".sav"
  )
)

# Stata----
# Fehlermeldung (Anfrage #401 an Hadley Wickham läuft)
#write_dta()
write_dta(
  sufdat,
  paste0(
    "./Output/Datensaetze/",
    format(Sys.time(), "%Y-%m-%d"),
    "_SUF_WSI-Betriebsraetebefragung_2015",
    ".dta"
  )
)