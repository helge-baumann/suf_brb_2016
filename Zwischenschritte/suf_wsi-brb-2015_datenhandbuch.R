texi2pdf(
  "./Berichte/Codebook/2018-05-17_Codebook-Rawdata-BRB-2015.tex",
  clean=T)

# Bericht ablegen
file.rename(
  "2018-05-17_Codebook-Rawdata-BRB-2015.pdf",
  paste0(
    format(Sys.time(), "%Y-%m-%d"),
    "_Codebook_WSI-Betriebsrätebefragung-2015.pdf"
  )
)
file.copy(
  paste0(
    format(Sys.time(), "%Y-%m-%d"),
    "_Codebook_WSI-Betriebsrätebefragung-2015.pdf"
  ),
   to =  "./Output/Berichte/",
  overwrite=T
)
file.remove(paste0(
  format(Sys.time(), "%Y-%m-%d"),
  "_Codebook_WSI-Betriebsrätebefragung-2015.pdf"
))
