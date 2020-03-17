# Datensätze einlesen

# Übersicht der Datensätze: ----------------------------------------------------


# Befragungsdaten einlesen -----------------------------------------------------
rawdat <- read_dta(
  "./Input/Daten/infas_Befragungsdatensatz_5195_WSI_BR_20160524.dta"
  )

# Gewichtungsdaten einlesen ----------------------------------------------------
gew <- read_dta("./Input/Daten/infas_Gewichte_WSI_BR_5195_20160523_1.dta")

# Interviewerdaten einlesen -----------------------------------------------------
int <- read_dta("./Input/Daten/brbefragung_interviewerdaten_2016.dta")

# Methodenvariablen einlesen (für Interviewer)----------------------------------
meth <- read_dta("./Input/Daten/brbefragung_methodendaten_2016.dta")

# Stichprobenmerkmale
sti <- read_dta("./Input/Daten/infas_Stichprobenmerkmale_WSI_4808_20150611.dta")
