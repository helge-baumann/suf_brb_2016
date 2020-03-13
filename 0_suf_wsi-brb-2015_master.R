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

# Daten laden und mergen -------------------------------------------------------

# Daten in ./Input 
source(
  "./Zwischenschritte/1_suf_wsi-brb-2015_datensaetze-laden.R", 
  encoding = "UTF-8"
  ) 

# Unvollständige Skalen labeln (zB I3_1)
source(
  "./Zwischenschritte/2_suf_wsi-brb-2015_skalen-labeln.R", 
  encoding = "UTF-8"
)

source(
  "./Zwischenschritte/3_suf_wsi-brb-2015_daten-mergen.R", 
  encoding = "UTF-8"
)

# Bericht: Fragebogen-----------------------------------------------------------
source(
  "./Zwischenschritte/4_suf_wsi-brb-2015_fragebogen.R", 
  encoding = "UTF-8"
) # Output: "Fragebogen.tex" im Ordner Berichte/Codebuch

# SUF erzeugen------------------------------------------------------------------
source(
  "./Zwischenschritte/6a_suf_wsi-brb-2015_suf-missings.R", 
  encoding = "UTF-8"
) 

source(
  "./Zwischenschritte/6b_suf_wsi-brb-2015_suf-generieren.R", 
  encoding = "UTF-8"
) 

source(
  "./Zwischenschritte/6c_suf_wsi-brb-2015_suf-vergroebern.R", 
  encoding = "UTF-8"
) 

source(
  "./Zwischenschritte/6d_suf_wsi-brb-2015_suf-loeschen.R", 
  encoding = "UTF-8"
) 

# Bericht: Codebooks------------------------------------------------------------
source(
  "./Zwischenschritte/7a_suf_wsi-brb-2015_codebook-rawdata.R", 
  encoding = "UTF-8"
) # Output: "Codebook_SUF.tex" im Ordner Berichte/Codebuch

source(
  "./Zwischenschritte/7_suf_wsi-brb-2015_codebook-suf.R", 
  encoding = "UTF-8"
) # Output: "Codebook_SUF.tex" im Ordner Berichte/Codebuch

# Dateien erzeugen--------------------------------------------------------------
source(
  "./Zwischenschritte/8_suf_wsi-brb-2015_datensaetze-erzeugen.R", 
  encoding = "UTF-8"
) # Output: Dateien im Ordner Output

# Bericht kompilieren-----------------------------------------------------------
source(
  "./Zwischenschritte/suf_wsi-brb-2015_datenhandbuch.R", 
  encoding = "UTF-8"
) # Output: Codebook im Ordner Output

# SessionInfo erzeugen ---------------------------------------------------------
dir.create("./sessionInfo", showWarnings=F)
writeLines(
  capture.output(sessionInfo()), 
  paste0(
    "./sessionInfo/",
    format(Sys.time(), "%Y-%m-%d"),
    "_sessionInfo.txt")
)
