#options(encoding="Windows-1252")
texi2pdf(
  "./Berichte/Codebook/2018-05-17_Codebook-Rawdata-BRB-2016.tex",
  clean=F, quiet=F)

zeit <- format(Sys.time(), "%Y-%m-%d_%H%M%S")

# Bericht ablegen
file.rename(
  "2018-05-17_Codebook-Rawdata-BRB-2016.pdf",
  paste0(zeit,"_Codebook_WSI-Betriebsrätebefragung-2016.pdf")
)
file.move(paste0(zeit,"_Codebook_WSI-Betriebsrätebefragung-2016.pdf"), 
                 "./Output/Berichte/")
#file.remove(
 # list.files()[which(str_detect(list.files(), "2018-05-17"))]
#)
