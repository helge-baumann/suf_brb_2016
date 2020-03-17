# SUF erzeugen: Löschen
#rm(list=setdiff(ls(), c("sufdat", "prozent", "br_anteil", "Gen", "Codebook_Rawdata", "rawdat")))

# Fragen mit Anweisungen einlesen-----------------------------------------------

# Codes:
# 0 = unverändert
# 1 = Löschen wegen ZP
# 2 = Löschen/Auslagen wegen Methodenvariablen
# 3 = Löschen wegen offene Angabe
# 4 = Vergröbern
# 5 = Generieren

options(encoding="Windows-1252")
fr_b <- read.table("./Hintergrund/bereinigung_suf.txt", sep="\t", header=T)
fr_b <- fr_b[,1:3]
fr_b[,2] <- stri_enc_toutf8(fr_b[,2])
options(encoding="UTF-8")

# Nachbearbeitung: Generierte Variablen----
fr_b[,3][fr_b[,3] >= 4] <- 0

  for(i in prozent) {
    
    fr_b[i-1,3] <- 4
    fr_b[i,3]   <- 4
    
    if(str_detect(
      string=attr(sufdat[[i-2]], "label"), 
      pattern=regex("Vorhandensein", ignore_case=T)
    )) {
      
      fr_b[i-2,3] <- 4
      
    }
  }

for(i in br_anteil) { fr_b[i,3] <- 4 }

# Nachbearbeitung: Vergröberung----
for(i in 1:nrow(fr_b)) {

  if(str_detect(
    string=names(sufdat)[i], 
    pattern=fixed("_kat")
  )) {
    
    fr_b[i,3] <- 5
    
  }
}


  


# Variablen löschen und dokumentieren ------------------------------------------
Loesch <- data.frame(
  r1=NA, 
  r2=NA,
  r3=NA)

del_gr <- c("Zielpersonen-angaben", "Methoden-variablen", "offene Angaben",
            "Neue Var. generiert", "Angaben vergröbert")
num <- 1

for(i in 1:5) {
  Loesch[num,1] <- paste0(
    "\\multirow{2}{2.5cm}{\\textbf{",
    del_gr[i],
    "}}"
  )
  
  for(b in fr_b[,1][fr_b[,3] == i]) {
    Loesch[num,2] <- paste0(
      "\\hyperref[var:",
      str_replace_all(b, fixed("_"), ":"),
      "]{",
      b,
      "}"
    )
    Loesch[num,3] <- paste0(
      "\\textbf{",
     fr_b[fr_b[,1]==b,2],
      "}"
    )
    num <- num+1
  }

}

keep <- c(fr_b$code, rep(0, ncol(sufdat)-length(fr_b$code)))
# Variablen behalten
sufdat <- sufdat[, which(keep == 0 | str_detect(names(sufdat), "\\_gen|\\_kat"))]

# print ----

# Unterstriche einführen
for(i in 1:nrow(Loesch)) {
  for(j in 1:ncol(Loesch)) {
    Loesch[i,j] <- 
      str_replace_all(
        Loesch[i,j], 
        fixed("_"), "\\_")
  }
}

midrules <- which(is.na(Loesch[,1])==F)-1
midrules <- midrules[-1]

Loesch_tex <- xtable(Loesch)

print.xtable(Loesch_tex, 
             include.rownames=F,
             sanitize.text.function = identity,
             only.contents=T,
             booktabs=T,
             hline.after=midrules,
             include.colnames=F, 
             file="./Berichte/Codebook/E2_Geloeschte_Variablen.tex"
) 

#rm(list=setdiff(ls(), c("sufdat", "Gen", "Codebook_Rawdata", "rawdat")))





