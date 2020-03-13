# Dateien verschieben----

moving <- function(path) {
  
  files <- list.files(path, full.names=T, recursive=T)
  movers <- !str_detect(files, format(Sys.time(), "%Y-%m-%d"))
  file.move(files[movers], paste0(path, "/bin"))
  
}
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