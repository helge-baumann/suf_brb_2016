fb <- readLines("./Hintergrund/Fragebogen_Betriebsrätebefragung_2015_text.txt")

# Link zwischen Variablen und Fragebogen
fb_link <- data.frame(
  vars = names(rawdat),
  fragen = NA,
  kurz=NA
)

fragen <- NA
num <- 0
for(i in 1:length(fb)) {
  
  if(str_detect(fb[i], "^[:alpha:][:digit:].?.?.?.?.?.?:")) {
    num <- num+1
  fragen[num] <- str_extract(fb[i], "^[:alpha:][:digit:].?.?.?.?.?.?:")
  }
}

fragen <- str_remove(fragen, ":")

for(i in 1:nrow(fb_link)) {
 
  # wenn Var = Frage: dann fb_link$fragen = var
  # wenn es keine Entsprechung gibt: letzten Unterstrich plus Zusatz entfernen
  # oder bei Order umgekehrt.
  if(tolower(fb_link[i,1]) %in% tolower(fragen)) {
    fb_link[i,2] <- as.character(fb_link[i,1])
  } else {
  if(str_detect(fb_link[i,1], fixed("_"))) {
    if(str_detect(fb_link[i,1], fixed("Order", ignore_case=TRUE))) {
   fb_link[i,2] <- str_remove(fb_link[i,1], "[^\\_]+\\_")
    }
    # wenn die variable auf eine Frage passt (ignore case)
    if(str_detect(fb_link[i,1], "\\_[:alnum:][:alnum:]?$")) {
      x <- str_remove(fb_link[i,1], "\\_[:alnum:][:alnum:]?$")
      if(tolower(x) %in% tolower(fragen)) {
       fb_link[i,2] <- x 
      }
    }
  }
   
  }
}

fb_link[fb_link[,1] == "K12b_1_1",2] <- "K12"
fb_link[fb_link[,1] == "K12b_1_2",2] <- "K12"
fb_link[fb_link[,1] == "P3_o_5",2] <- "P3"
fb_link[is.na(fb_link[,2]) == T,2] <- as.character(fb_link[is.na(fb_link[,2]) == T,1]) 

for(i in 1:nrow(fb_link)) {
  
  if(all(is.na(rawdat[[i]][rawdat$langkurz==2])==T)) {
    fb_link[i,3] <- 1
  } else {
    fb_link[i,3] <- 0
  } 
  
}

fb_lang <- fb_link[str_detect(fb_link[,1], fixed("Order", ignore_case=TRUE)) ==F,2:3]
fb_lang <- unique(fb_lang)

#################################

# Fragebogen als Data Frame
fb_df <- data.frame(Frage = NA, Text = NA)

for(i in 1:length(fb)) {
 
# leere Zeilen benötigen keinen Eintrag,
# sondern werden über den Index automatisch erzeugt!
  
  # Schritt 1: Trennen
  if(str_detect(fb[i], "[^:]+")==T &
     str_detect(fb[i], "Block ..?")==F) {
    fb_df[i,1] <- str_extract(fb[i], "[^:]+:")
    fb_df[i,2] <- str_remove(fb[i], "[^:]+: ")  
  }
  
  # Schritt 2: Blöcke erkennen und abtrennen!
  if(str_detect(fb[i], "Block ..?:") == T) {
    #fb_df[i-2, 1] <- "\\multicolumn{1}{!{\\color{black}\\vline width 1pt}  l}{}"
    #fb_df[i-1, 1] <- "\\multicolumn{1}{!{\\color{black}\\vline width 1pt}  l}{}"
    #fb_df[i+1, 1] <- "\\multicolumn{1}{!{\\color{black}\\vline width 1pt}  l}{}"
    #fb_df[i-2, 2] <- "\\multicolumn{1}{r!{\\color{black}\\vline width 1pt}  }{}"
    #fb_df[i-1, 2] <- "\\multicolumn{1}{r!{\\color{black}\\vline width 1pt}  }{}"
    #fb_df[i+1, 2] <- "\\multicolumn{1}{r!{\\color{black}\\vline width 1pt}  }{}"
    fb_df[i, 1] <- paste0(
      "\\protect\\subsection[",
      "\\parbox{\\mylength}{",
      str_extract(fb[i], "Block ..?: "),
      "}",
      str_remove(fb[i], "Block ..?: "),
      "]",
      "{",
      str_extract(fb[i], "Block ..?:"),
      "}"
    )
    fb_df[i, 2] <- paste0(
      "\\protect\\subsection*{",
      str_remove(fb[i], "Block ..?: "),
      "}"
    )
      #"\\multicolumn{1}{!{\\color{black}\\vline width 1pt}  l!{\\color{black}\\vline width 1pt}  }{\\large \\textbf{",
      #str_extract(fb[i], "Block ..?:"),
      #"}}"
    #)
    #fb_df[i, 2] <- paste0(
     # "\\multicolumn{1}{!{\\color{black}\\vline width 1pt}  l!{\\color{black}\\vline width 1pt}  }{\\large \\textbf{",
     # str_remove(fb[i], "Block ..?: "),
     # "}}"
    #)
  }
}

# Programmierebene erkennen
for(i in 1:nrow(fb_df)) {
  
  if(is.na(fb_df[i,1])==F) {
     
  if(str_detect(fb_df[i,1], "PROG:") | 
     str_detect(fb_df[i,1], "WENN:")) {
    
    fb_df[i,1] <- paste0(
      "\\textcolor{red}{",
      fb_df[i,1],
      "}")
    fb_df[i,2] <- paste0(
      "\\textcolor{red}{",
      fb_df[i,2],
      "}")
  }
  
  if(str_detect(fb_df[i,1], "INT:")) {
    
    fb_df[i,1] <- paste0(
      "\\textcolor{blue}{",
      fb_df[i,1],
      "}")
    fb_df[i,2] <- paste0(
      "\\textcolor{blue}{",
      fb_df[i,2],
      "}")
    
  }
  
  if(str_detect(fb_df[i,1], "^[:alpha:][:digit:].?.?.?.?.?.?:") |
     str_detect(fb_df[i,1], "^Intromail.:")) {
    
    fb_df[i,1] <- paste0(
      "\\textbf{",
      fb_df[i,1],
      "}\\phantomsection\\label{",
      str_replace_all(str_remove(fb_df[i,1], ":"), fixed("_"), ":"),
      "}")
    fb_df[i,2] <- paste0(
      "\\textbf{",
      fb_df[i,2],
      "}")
    
  }
    
    
  if(str_detect(fb_df[i,1], 
                "^[:digit:][:digit:]?[:digit:]?[:digit:]?[:digit:]?[:digit:]?:") |
     str_detect(fb_df[i,1], 
                "^[:upper:]:") |
     str_detect(fb_df[i,1], 
                "^[:upper:][:upper:]:") |
     str_detect(fb_df[i,1], 
                "^[:upper:][:lower:].?.?.?.?.?:")) {
      
      fb_df[i,1] <- paste0(
        "\\multicolumn{1}{!{\\color{black}\\vline width 1pt}  r!{\\color{black}\\vline width 1pt}  }{",
        fb_df[i,1],
        "}")
   
  }
    }
    
  
    if((is.na(fb_df[i,1])==T & is.na(fb_df[i,2])==F) 
       #| 
       #(is.na(fb_df[i,2])==T & is.na(fb_df[i,1])==F)
       ) {
      
      fb_df[i,2] <- paste0(
        "\\textbf{",
        fb_df[i,2],
        "}")
      
    }
    
  
  
}


# midrules einfügen!!
midrules <- NA

for(i in 1:nrow(fb_df)) {
  
  if(is.na(fb_df[i,1]) == F) {
  if(str_detect(fb_df[i,1], "[:alpha:][:digit:].?.?.?.?.?.?:") |
     str_detect(fb_df[i,1], "Intromail.:")) {
    
    if(is.na(fb_df[i-1,1]) ==F) {
    if(str_detect(fb_df[i-1,1], "WENN")) {
      midrules[i] <- i-2
    }
    } else {
      midrules[i] <- i-1
    }
    
  }
    if(str_detect(fb_df[i,1], "Block")) {
      midrules[i] <- i-1
    }
  }
}

midrules <- midrules[complete.cases(midrules)]

# Farbige Zeilen
cheat <- rep(c(0,1), 100000)
num <- 1

for(i in 1:nrow(fb_df)) {
  
  if(i > 1 & i < nrow(fb_df)) {
  if(cheat[num] == 0) {
  if(
    # entweder: nur diese Reihe (ohne Filterbedingung)
    (str_remove(
      str_extract(fb_df[i,1], "[:alpha:][:digit:].?.?.?.?.?.?:"),
      ":") %in%
    fb_lang[fb_lang[,2] == 1,1] &
    is.na(fb_df[i-1,1]) ==T) |
    (str_remove(
      str_extract(fb_df[i+1,1], "[:alpha:][:digit:].?.?.?.?.?.?:"),
      ":") %in%
     fb_lang[fb_lang[,2] == 1,1] &
     is.na(fb_df[i,1]) == F)
    ) {
   
    num <- num+1
    
  }
  }
  
  if(cheat[num] == 1) {
  if(
    (str_remove(
      str_extract(fb_df[i,1], "[:alpha:][:digit:].?.?.?.?.?.?:"),
      ":") %in%
    na.omit(fb_lang[fb_lang[,2] == 0,1])) |
    (str_detect(fb_df[i,1], "Block") %in% TRUE)) {
    
    num <- num+1
    
  }
    
  }
  
  if(cheat[num] == 1) {
    
    fb_df[i,1] <- paste0(
      "\\rowcolor[gray]{.9}",
      fb_df[i,1]
    )
    
  }
  }
  
  #print(cheat[num])
}
    


# Unterstriche einführen
for(i in 1:nrow(fb_df)) {
  for(j in 1:ncol(fb_df)) {
    fb_df[i,j] <- 
      str_replace_all(
        fb_df[i,j], 
        fixed("_"), "\\_")
  }
}

# in ein xtable überführen (Latex-Format)
fb_tex <- xtable(fb_df, digits=0)

for(i in 1:nrow(fb_tex)) {
  for(j in 1:ncol(fb_tex)) {
    fb_tex[i,j] <- 
      str_replace_all(
        fb_tex[i,j], 
        fixed("NA"), "")
  }
}

print.xtable(fb_tex, 
             include.rownames=F,
             sanitize.text.function = identity,
             only.contents=T,
             booktabs=T,
             hline.after=midrules,
             include.colnames=F, 
             file="./Berichte/Codebook/Fragebogen.tex"
) 

# Fragen identifizieren:
# str_detect(fb[200], "^[:alpha:][:digit:].?:")