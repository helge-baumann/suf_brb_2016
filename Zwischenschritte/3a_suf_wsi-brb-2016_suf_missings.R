# Erstellung des SUFs: Missings definieren

sufdat <- as_tibble(lapply(sufdat, replace_missings))
