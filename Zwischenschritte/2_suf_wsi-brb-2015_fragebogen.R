# Fragebogen einlesen (von Word als .txt, danach bearbeitet)--------------------
options(encoding="Windows-1252")
fb <- readLines("./Hintergrund/Fragebogen_Betriebsrätebefragung_2015_text.txt")
fb <- stri_enc_toutf8(fb)
options(encoding="UTF-8")

fragen <- unique(na.omit(str_extract(fb, "^[A-Z][0-9][[:alnum:]\\_]*")))

# Funktionen: get.questions (get.qu) und get.langkurz (get.lk)----

get.qu <- function(names) {
  
  # Erster Schritt: Variablen am Unterstrich splitten
  var <- unlist(str_split(names, "\\_"))
  # Wenn EIN Element einer Frage entspricht: ablegen (wichtig für "Order")
  frage <- fragen[tolower(fragen) == tolower(var[which(var %in% fragen)])]
  
  # Jetzt strings zusammenpacken; 
  # der längste String, der einer Frage entspricht, ist korrekt.
  for(i in 1:length(var)) {
    
    var_c <- paste(var[1:i], collapse="_")
    
    # nur wenn ein zusammengesetzter String einer Frage entspricht:
    if(any(tolower(var_c) %in% tolower(fragen))) {
      frage <- fragen[tolower(fragen) == tolower(var_c[which(var_c %in% fragen)])]
    }
    
  }
  
  # Ergebnis: Entweder eine Frage oder NA
  if(length(frage) > 0) { return(frage) } else { return(NA) }
  
}

get.lk <- function(x, y, value) { 
  
  return(all(is.na(x[y==value])==T))
  
}

# Link zwischen Variablen und Fragen im Fragebogen------------------------------
fb_link <- data.frame(vars = names(rawdat), 
                      fragen = unlist(sapply(names(rawdat), get.qu)), 
                      kurz=as.integer(unlist(sapply(
                        rawdat, get.lk, y=rawdat$langkurz, value=2)))
)

# Fragebogen als Data Frame-----------------------------------------------------
fb_df <- data.frame(Frage = NA, Text = NA, Variablen=NA)

# Splitten
for(i in seq_along(fb)) {
  
  # leere Zeilen benötigen keinen Eintrag,
  # sondern werden über den Index automatisch erzeugt!
  
  # Schritt 1: Trennen (Fragen, Items, Codes, Programmierhinweise...)
  if(str_detect(fb[i], "[^:]+")) {
    fb_df[i,1] <- str_extract(fb[i], "[^:]+:")
    fb_df[i,2] <- str_remove(fb[i], "[^:]+: ")  
  }
  
  # Schritt 2: Blöcke erkennen und abtrennen!
  if(str_detect(fb[i], "Block ..?:")) {
    
    fb_df[i, 1:2] <- c(
      paste0(
        "\\protect\\subsection[\\parbox{\\mylength}{",
        fb_df[i, 1], "} ", fb_df[i, 2], "]", "{", fb_df[i, 1], "}"
      ), 
      paste0("\\protect\\subsection*{", fb_df[i,2], "}")
    )
    
  }
  
  # reiner Text
  if(is.na(fb_df[i,1]) & !is.na(fb_df[i,2]))  {
    
    fb_df[i,2] <- paste0("\\textbf{", fb_df[i,2], "}")
    
  }
  
}

# Programmierebene erkennen-----------------------------------------------------
for(i in which(!is.na(fb_df[,1]))) {
  
  if(str_detect(fb_df[i,1], "PROG:|WENN:|INT:")) {
    
    fb_df[i,1:2] <- c(
      paste0("\\textcolor{red}{", fb_df[i,1],"}"),
      paste0("\\textcolor{red}{", fb_df[i,2],"}")
    )
    
  }
  
  # Fragenebene
  if(str_detect(fb_df[i,1], "^[A-Z][0-9][[:alnum:]\\_]*|Intromail.:")) {
    
    # Variablen zu Fragen verlinken
    fr <- str_remove(fb_df[i,1], ":")
    vars <- split(fb_link, fb_link[,2])[[fr]]$vars
    laenge <- length(vars)
    
      # Hyperref für alle Variablen
      temp <- paste0(
          "\\hyperref[var:",
          str_replace_all(vars, fixed("_"), ":"),
          "]{",
          vars,
          "}"
        )
      
      # wenn eine Variable
      if(laenge == 1) fb_df[i,3] <- temp
      
      # Verkürzen, wenn mehrere Variablen
      if(laenge == 2) fb_df[i,3] <- paste(temp, collapse= ", ")
      if(laenge >  2) fb_df[i,3] <- paste(
        temp[c(1,length(temp))], collapse= " [...] ")
    
    # Label zum Referenzieren für Fragen
    fb_df[i,1] <- paste0(
      "\\textbf{",
      fb_df[i,1],
      "}\\phantomsection\\label{",
      str_replace_all(str_remove(fb_df[i,1], ":"), fixed("_"), ":"),
      "}")
    
    # Fragentext in fett
    fb_df[i,2] <- paste0("\\textbf{", fb_df[i,2], "}")
    
  }
  
  # Codes rechtsbündig
  if(str_detect(fb_df[i,1], 
                "^[:digit:]{1,6}:|^[:upper:]{1,2}:|^[:upper:][:lower:]{1,6}:") 
     ) {
    
    fb_df[i,1] <- paste0(
      "\\multicolumn{1}{!{\\color{black}\\vline width 1pt}",
      "r!{\\color{black}\\vline width 1pt}  }{",
      fb_df[i,1],
      "}")
    
  }
  
}

# Bearbeitungen (zB midrules)---------------------------------------------------
midrules <- NA

for(i in which(!is.na(fb_df[,1]))) {
  
  if(str_detect(fb_df[i,1], "\\{[A-Z][0-9][[:alnum:]\\_]*:|Intromail.:")) {
    midrules[i] <- i-1-identical(str_detect(fb_df[i-1,1], "WENN"), T)
  }
   
  if(str_detect(fb_df[i,1], "Block")) {
    midrules[i] <- i-1
  }
  
}

midrules <- na.omit(midrules)[-1]

# Farbige Zeilen----
switch <- 0

for(i in seq_along(fb)) {
  
  x <- str_extract(fb[i], "^[A-Z][0-9][[:alnum:]\\_]*")
  
  if(x %in% fb_link$fragen[fb_link$kurz == 1]) switch <- 1
  if(x %in% fb_link$fragen[fb_link$kurz == 0] & !is.na(x)) switch <- 0
  
  if(switch == 1) {
    fb_df[i,1] <- paste0(
      "\\rowcolor[gray]{.9}",
      fb_df[i,1]
    )
  }

}

# Hyperlinks für Filterführungen----
for(i in 
    which(str_detect(
      fb_df[,2], "[A-Z][0-9]") & str_detect(fb_df[,1], "PROG:|WENN:")
      )) {
  
  x <- str_extract_all(fb_df[i,2], "[A-Z][0-9][[:alnum:]\\_]{0,6}", simplify=T)
  z <- str_extract(x, "\\_[:upper:]")
  x <- str_replace(x, "\\_[:upper:]", "")
  x <- paste0("\\hyperref[", str_replace(x, fixed("_"), ":"), "]{", x, "}")
  x <- paste0(x, z)
  y <- str_split(fb_df[i,2], "[A-Z][0-9][[:alnum:]\\_]{0,6}", simplify=T)
  
  fb_df[i,2] <- paste0(
    paste(y[1:length(x)], x, collapse=""), y[length(y)], 
    collapse="")
  
}

# Unterstriche einführen, NAs löschen----
fb_df <- data.frame(lapply(fb_df, str_replace_all, fixed("_"), "\\_"))
fb_df <- data.frame(lapply(fb_df, str_replace_all, fixed("NA"), ""))
fb_df <- data.frame(lapply(fb_df, str_replace_na, ""))

# in ein xtable überführen (Latex-Format)---------------------------------------
fb_tex <- xtable(fb_df, digits=0)

print.xtable(fb_tex, 
             include.rownames=F,
             sanitize.text.function = identity,
             only.contents=T,
             booktabs=T,
             hline.after=midrules,
             include.colnames=F, 
             file="./Berichte/Codebook/Fragebogen.tex"
) 

