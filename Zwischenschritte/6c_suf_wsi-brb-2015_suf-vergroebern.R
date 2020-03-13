# Variablen vergröbern

# Variablenlabels noch erweitern!
# Betriebsgröße manuell (D1) ----

sufdat$D1_kat <- sufdat$D1

sufdat$D1_kat[sufdat$D1 >= 1 & sufdat$D1 <= 9] <- 1
sufdat$D1_kat[sufdat$D1 >= 10 & sufdat$D1 <= 19] <- 2
sufdat$D1_kat[sufdat$D1 >= 20 & sufdat$D1 <= 29] <- 3
sufdat$D1_kat[sufdat$D1 >= 30 & sufdat$D1 <= 39] <- 4
sufdat$D1_kat[sufdat$D1 >= 40 & sufdat$D1 <= 49] <- 5

sufdat$D1_kat[sufdat$D1 >= 50 & sufdat$D1 <= 59] <- 6
sufdat$D1_kat[sufdat$D1 >= 60 & sufdat$D1 <= 69] <- 7
sufdat$D1_kat[sufdat$D1 >= 70 & sufdat$D1 <= 79] <- 8
sufdat$D1_kat[sufdat$D1 >= 80 & sufdat$D1 <= 89] <- 9
sufdat$D1_kat[sufdat$D1 >= 90 & sufdat$D1 <= 99] <- 10

sufdat$D1_kat[sufdat$D1 >= 100 & sufdat$D1 <= 149] <- 11
sufdat$D1_kat[sufdat$D1 >= 150 & sufdat$D1 <= 199] <- 12

sufdat$D1_kat[sufdat$D1 >= 200 & sufdat$D1 <= 299] <- 13
sufdat$D1_kat[sufdat$D1 >= 300 & sufdat$D1 <= 399] <- 14
sufdat$D1_kat[sufdat$D1 >= 400 & sufdat$D1 <= 499] <- 15

sufdat$D1_kat[sufdat$D1 >= 500 & sufdat$D1 <= 599] <- 16
sufdat$D1_kat[sufdat$D1 >= 600 & sufdat$D1 <= 699] <- 17
sufdat$D1_kat[sufdat$D1 >= 700 & sufdat$D1 <= 799] <- 18
sufdat$D1_kat[sufdat$D1 >= 800 & sufdat$D1 <= 899] <- 19
sufdat$D1_kat[sufdat$D1 >= 900 & sufdat$D1 <= 999] <- 20

sufdat$D1_kat[sufdat$D1 >= 1000 & sufdat$D1 <= 1499] <- 21
sufdat$D1_kat[sufdat$D1 >= 1500 & sufdat$D1 <= 1999] <- 22

sufdat$D1_kat[sufdat$D1 >= 2000 & sufdat$D1 <= 4999] <- 23
sufdat$D1_kat[sufdat$D1 >= 5000 & sufdat$D1 <= max(sufdat$D1, na.rm=T)] <- 24

attr(sufdat$D1_kat, "labels") <- c(attr(sufdat$D1, "labels"),
                                   sort(unique(sufdat$D1_kat[sufdat$D1_kat >= 0]))
                                  )
names(attr(sufdat$D1_kat, "labels")) <- 
  c("verweigert", "weiß nicht",
    "weniger als 10 Beschäftigte", 
    "10 bis 19 Beschäftigte",
    "20 bis 29 Beschäftigte",
    "30 bis 39 Beschäftigte",
    "40 bis 49 Beschäftigte",
    
    "50 bis 59 Beschäftigte",
    "60 bis 69 Beschäftigte",
    "70 bis 79 Beschäftigte",
    "80 bis 89 Beschäftigte",
    "90 bis 99 Beschäftigte",
    
    "100 bis 149 Beschäftigte",
    "150 bis 199 Beschäftigte",
    
    "200 bis 299 Beschäftigte",
    "300 bis 399 Beschäftigte",
    "400 bis 499 Beschäftigte",
    
    "500 bis 599 Beschäftigte",
    "600 bis 699 Beschäftigte",
    "700 bis 799 Beschäftigte",
    "800 bis 899 Beschäftigte",
    "900 bis 999 Beschäftigte",
    
    "1000 bis 1499 Beschäftigte",
    "1500 bis 1999 Beschäftigte",
    
    "2000 bis 4999 Beschäftigte",
    "5000 und mehr Beschäftigte")


# Krankenstand (I7)----
sufdat$I7_kat <- sufdat$I7

sufdat$I7_kat[sufdat$I7 == 0] <- 1
sufdat$I7_kat[sufdat$I7 >= 1 & sufdat$I7 <= 3] <- 2
sufdat$I7_kat[sufdat$I7 >= 4 & sufdat$I7 <= 6] <- 3
sufdat$I7_kat[sufdat$I7 >= 7 & sufdat$I7 <= 10] <- 4
sufdat$I7_kat[sufdat$I7 >= 11 & sufdat$I7 <= 15] <- 5
sufdat$I7_kat[sufdat$I7 >= 16 & sufdat$I7 <= 20] <- 6
sufdat$I7_kat[sufdat$I7 >= 21 & sufdat$I7 <= 30] <- 7
sufdat$I7_kat[sufdat$I7 >= 31 & sufdat$I7 <= max(sufdat$I7, na.rm=T)] <- 8

attr(sufdat$I7_kat, "labels") <- sort(unique(sufdat$I7_kat))
names(attr(sufdat$I7_kat, "labels")) <- 
  c("weiß nicht", "verweigert", "Item in Fragebogenversion nicht erhoben",
    "Krankenstand 0%", "Krankenstand 1 bis 3%",
    "Krankenstand 4 bis 6%", "Krankenstand 7 bis 10%",
    "Krankenstand 11 bis 15%", "Krankenstand 15 bis 20%",
    "Krankenstand 20 bis 30%","Krankenstand 30 bis 100%")

# Bestehen des Betriebsrates----

sufdat$M1_kat <- sufdat$M1

sufdat$M1_kat[sufdat$M1 >= 0 & sufdat$M1 <= 1950] <- 1
sufdat$M1_kat[sufdat$M1 >= 1951 & sufdat$M1 <= 1960] <- 2
sufdat$M1_kat[sufdat$M1 >= 1961 & sufdat$M1 <= 1970] <- 3
sufdat$M1_kat[sufdat$M1 >= 1971 & sufdat$M1 <= 1980] <- 4
sufdat$M1_kat[sufdat$M1 >= 1981 & sufdat$M1 <= 1990] <- 5
sufdat$M1_kat[sufdat$M1 >= 1991 & sufdat$M1 <= 1995] <- 6
sufdat$M1_kat[sufdat$M1 >= 1996 & sufdat$M1 <= 2000] <- 7
sufdat$M1_kat[sufdat$M1 >= 2001 & sufdat$M1 <= 2005] <- 8
sufdat$M1_kat[sufdat$M1 >= 2006 & sufdat$M1 <= 2010] <- 9
sufdat$M1_kat[sufdat$M1 >= 2011 & sufdat$M1 <= 2013] <- 10
sufdat$M1_kat[sufdat$M1 >= 2014 & sufdat$M1 <= max(sufdat$M1, na.rm=T)] <- 11

attr(sufdat$M1_kat, "label") <- "Existenzdauer des Betriebsrats in Jahren"
attr(sufdat$M1_kat, "labels") <- c(attr(sufdat$M1, "labels"), sort(unique(sufdat$M1_kat[
  sufdat$M1 >= 0])))
names(attr(sufdat$M1_kat, "labels")) <- 
  c("verweigert", "weiß nicht",
    "Betriebsrat wurde vor 1951 gegründet",
    "Betriebsrat wurde zwischen 1951  und 1960 gegründet",
    "Betriebsrat wurde zwischen 1961  und 1970 gegründet",
    "Betriebsrat wurde zwischen 1971  und 1980 gegründet",
    "Betriebsrat wurde zwischen 1981  und 1990 gegründet",
    "Betriebsrat wurde zwischen 1991  und 1995 gegründet",
    "Betriebsrat wurde zwischen 1996  und 2000 gegründet",
    "Betriebsrat wurde zwischen 2001  und 2005 gegründet",
    "Betriebsrat wurde zwischen 2006  und 2010 gegründet",
    "Betriebsrat wurde zwischen 2011  und 2013 gegründet",
    "Betriebsrat wurde seit 2014 gegründet")

# Letzte Wahl des BR----
sufdat$M2_kat <- sufdat$M2

sufdat$M2_kat[sufdat$M2 >= 0 & sufdat$M2 <= 2010] <- 1
sufdat$M2_kat[sufdat$M2 >= 2011 & sufdat$M2 <= 2012] <- 2
sufdat$M2_kat[sufdat$M2 == 2013] <- 3
sufdat$M2_kat[sufdat$M2 == 2014] <- 4
sufdat$M2_kat[sufdat$M2 == 2015] <- 5

attr(sufdat$M2_kat, "labels") <- c(attr(sufdat$M2, "labels"), 
                                   sort(unique(sufdat$M2_kat[sufdat$M2 >= 0])))
names(attr(sufdat$M2_kat, "labels")) <- 
  c("verweigert", "weiß nicht",
    "Der Betriebsrat wurde zuletzt 2010 oder früher gewählt",
    "Der Betriebsrat wurde zuletzt 2011 oder 2012 gewählt",
    "Der Betriebsrat wurde zuletzt 2013 gewählt",
    "Der Betriebsrat wurde zuletzt 2014 gewählt",
    "Der Betriebsrat wurde zuletzt 2015 gewählt")

# Größe des  Betriebsrats----
sufdat$M3_kat <- sufdat$M3

sufdat$M3_kat[sufdat$M3 >= 1 & sufdat$M3 <= 2] <- 1
sufdat$M3_kat[sufdat$M3 >= 3 & sufdat$M3 <= 4] <- 2
sufdat$M3_kat[sufdat$M3 >= 5 & sufdat$M3 <= 6] <- 3
sufdat$M3_kat[sufdat$M3 >= 7 & sufdat$M3 <= 8] <- 4
sufdat$M3_kat[sufdat$M3 >= 9 & sufdat$M3 <= 10] <- 5
sufdat$M3_kat[sufdat$M3 >= 11 & sufdat$M3 <= 12] <- 6
sufdat$M3_kat[sufdat$M3 >= 13 & sufdat$M3 <= 14] <- 7
sufdat$M3_kat[sufdat$M3 >= 15 & sufdat$M3 <= 16] <- 8
sufdat$M3_kat[sufdat$M3 >= 17 & sufdat$M3 <= 18] <- 9
sufdat$M3_kat[sufdat$M3 >= 19 & sufdat$M3 <= 20] <- 10
sufdat$M3_kat[sufdat$M3 >= 21 & sufdat$M3 <= 22] <- 11
sufdat$M3_kat[sufdat$M3 >= 23 & sufdat$M3 <= 24] <- 12
sufdat$M3_kat[sufdat$M3 >= 25 & sufdat$M3 <= 30] <- 13
sufdat$M3_kat[sufdat$M3 >= 31 & sufdat$M3 <= 36] <- 14
sufdat$M3_kat[sufdat$M3 >= 37 & sufdat$M3 <= 42] <- 15
sufdat$M3_kat[sufdat$M3 >= 43 & sufdat$M3 <= 96] <- 16

attr(sufdat$M3_kat, "labels") <- c(attr(sufdat$M3, "labels"), 
                                   sort(unique(sufdat$M3_kat[sufdat$M3 >= 0])))
names(attr(sufdat$M3_kat, "labels")) <- 
  c("verweigert", "weiß nicht",
    "1 oder 2 Betriebsratsmitglieder",
    "3 oder 4 Betriebsratsmitglieder",
    "5 oder 6 Betriebsratsmitglieder",
    "7 oder 8 Betriebsratsmitglieder",
    "9 oder 10 Betriebsratsmitglieder",
    "11 oder 12 Betriebsratsmitglieder",
    "13 oder 14 Betriebsratsmitglieder",
    "15 oder 16 Betriebsratsmitglieder",
    "17 oder 18 Betriebsratsmitglieder",
    "19 oder 20 Betriebsratsmitglieder",
    "21 oder 22 Betriebsratsmitglieder",
    "23 oder 24 Betriebsratsmitglieder",
    "25 bis 30 Betriebsratsmitglieder",
    "31 bis 36 Betriebsratsmitglieder",
    "37 oder 42 Betriebsratsmitglieder",
    "43 oder mehr Betriebsratsmitglieder")


# Alter des BR-Vorsitzenden----
sufdat$M13_kat <- sufdat$M13

# Missings fehlten im Original!
sufdat$M13_kat[sufdat$M13 == 97] <- -7
sufdat$M13_kat[sufdat$M13 == 98] <- -8

sufdat$M13_kat[sufdat$M13 >= 21 & sufdat$M13 <= 30] <- 1
sufdat$M13_kat[sufdat$M13 >= 31 & sufdat$M13 <= 35] <- 2
sufdat$M13_kat[sufdat$M13 >= 36 & sufdat$M13 <= 40] <- 3
sufdat$M13_kat[sufdat$M13 >= 41 & sufdat$M13 <= 45] <- 4
sufdat$M13_kat[sufdat$M13 >= 46 & sufdat$M13 <= 50] <- 5
sufdat$M13_kat[sufdat$M13 >= 51 & sufdat$M13 <= 55] <- 6
sufdat$M13_kat[sufdat$M13 >= 56 & sufdat$M13 <= 60] <- 7
sufdat$M13_kat[sufdat$M13 >= 61 & sufdat$M13 <= 75] <- 8

attr(sufdat$M13_kat, "labels") <- c(sort(unique(sufdat$M13_kat)))
                                   
names(attr(sufdat$M13_kat, "labels")) <- 
  c("weiß nicht", "verweigert", "Item in Fragebogenversion nicht erhoben",
    "Der/die BR-Vorsitzende ist 30 oder jünger",
    "Der/die BR-Vorsitzende ist 31 bis 35 Jahre alt",
    "Der/die BR-Vorsitzende ist 36 bis 40 Jahre alt",
    "Der/die BR-Vorsitzende ist 40 bis 45 Jahre alt",
    "Der/die BR-Vorsitzende ist 46 bis 50 Jahre alt",
    "Der/die BR-Vorsitzende ist 51 bis 55 Jahre alt",
    "Der/die BR-Vorsitzende ist 56 bis 60 Jahre alt",
    "Der/die BR-Vorsitzende ist zwischen 61 und älter")

# Unternehmensgröße manuell (R1a) ----

sufdat$R1a_kat <- sufdat$R1a

sufdat$R1a_kat[sufdat$R1a >= 1 & sufdat$R1a <= 49] <- 1
sufdat$R1a_kat[sufdat$R1a >= 50 & sufdat$R1a <= 99] <- 2
sufdat$R1a_kat[sufdat$R1a >= 100 & sufdat$R1a <= 199] <- 3
sufdat$R1a_kat[sufdat$R1a >= 200 & sufdat$R1a <= 499] <- 4
sufdat$R1a_kat[sufdat$R1a >= 500 & sufdat$R1a <= 999] <- 5
sufdat$R1a_kat[sufdat$R1a >= 1000 & sufdat$R1a <= 1999] <- 6
sufdat$R1a_kat[sufdat$R1a >= 2000 & sufdat$R1a <= 4999] <- 7
sufdat$R1a_kat[sufdat$R1a >= 5000 & sufdat$R1a <= 9999] <- 8
sufdat$R1a_kat[sufdat$R1a >= 10000 & sufdat$R1a <= max(sufdat$R1a, na.rm=T)] <- 9

attr(sufdat$R1a_kat, "labels") <- c(attr(sufdat$R1a, "labels"),
                                   sort(unique(sufdat$R1a_kat[sufdat$R1a_kat >= 0]))
)
names(attr(sufdat$R1a_kat, "labels")) <- 
  c("weiß nicht", "verweigert", "Missing durch Filterführung",
    "weniger als 50 Beschäftigte", "50 bis 99 Beschäftigte",
    "100 bis 199 Beschäftigte", "200 bis 499 Beschäftigte",
    "500 bis 999 Beschäftigte", "1000 bis 1999 Beschäftigte",
    "2000 bis 4999 Beschäftigte", "5000 bis 9999 Beschäftigte",
    "10000 und mehr Beschäftigte")

    
# Labels erweitern ("(vergröbert)")----

for(i in 1:ncol(sufdat)) {
  
  if(str_detect(
    names(sufdat)[i], fixed("_kat")
  )) {
  attr(sufdat[[i]], "label") <- paste0(attr(sufdat[[i]], "label"),
                                       " (vergröbert)")
  }
}


# Variablen ersetzen ----
for(i in 1:ncol(sufdat)) {
  
  if(str_detect(
    names(sufdat)[i], fixed("_gen")
  )) {
    
    index_alt <- which(names(sufdat) == str_remove(names(sufdat[i]), fixed("_gen")))
    sufdat[,index_alt] <- sufdat[,i]
    names(sufdat)[index_alt] <- names(sufdat)[i]
    
  }
  
  if(str_detect(
    names(sufdat)[i], fixed("_kat")
  )) {
    
    index_alt <- which(names(sufdat) == str_remove(names(sufdat[i]), fixed("_kat")))
    sufdat[,index_alt] <- sufdat[,i]
    names(sufdat)[index_alt] <- names(sufdat)[i]
    
  }
}

sufdat <- sufdat[, 1:length(unique(names(sufdat)))]
