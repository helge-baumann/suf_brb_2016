############
# Masterfile zur Erzeugung eines SUF der WSI-Betriebsraetebefragung 2015
# Ersteller: Helge Emmler, WSI
# Letztes Update: 13.03.2020
############

# Präambel --------------------------------------------------------------------

## Pakete ggfs. installieren und laden
library(pacman)
p_load("styler", "labelled", "stringr", "here", "xtable", "tools", "dplyr", 
       "haven", "stringi") 

# Schritte in ./Zwischenschritte ausführen
sapply(dir("./Zwischenschritte", full.names=T), source, encoding="UTF-8")

# SessionInfo erzeugen ---------------------------------------------------------
dir.create("./sessionInfo", showWarnings=F)
writeLines(
  capture.output(sessionInfo()), 
  paste0(
    "./sessionInfo/",
    format(Sys.time(), "%Y-%m-%d"),
    "_sessionInfo.txt")
)
