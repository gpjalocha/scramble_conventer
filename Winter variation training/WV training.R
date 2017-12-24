source("alg_in_scramble.R")
source("parse Scrambles.R")
source("put_alg_in scramble.R")
source("reper pairity.R")
source("scramble to string.R")
source("string_cube_representation.R")
source("vector_to_string.R")

library(permutations);library(stringr)
scrambles <- c()
for(a in 1:30){
  edges <- c(0,0,0,0,5,6,7,1,9,10,11,12)
  edgesO <- rep(0,12)
  corners <- c(0,0,0,0,2,6,7,8)
  cornersO <- c(-1,-1,-1,-1,1,0,0,0)
  
  edges[which(edges==0)] <- sample(c(8,2,3,4))
  corners[which(corners==0)] <- sample(c(5,1,3,4))
  cornersO[which(cornersO==-1)] <- sample(0:2,sum(cornersO==-1),rep=T)
  cornersO[which(corners==5)] <- 0
  cornersO <- reperCO(cornersO,ind=2)
  edges <- reperPairity(edges,corners,c(3,4),pieces = 'E')
  
  
  scrambles <- c(scrambles,vectors_to_string(edges,edgesO,corners,cornersO))
}


scr<-c()
for(a in scrambles){
  scr<-c(scr,as.character(read.csv(paste(
    'http://localhost:8080/',a,sep=''
  ),sep='\t',header = F)[2,]))
}
scr


# Parser- cube srambled to url for image ----------------------------------
link <- 'http://www.cyotheking.com/winter-variation/'
wv_algs <- str_match_all(paste(readLines(link),collapse = '\n'),'<p class=\"text-align-center\">([RUDFLBxyz\'2 \\(\\)]*)(&nbsp;)?([RUDFLBxyz\'2 \\(\\)]*)(&nbsp;)?([RUDFLBxyz\'2 \\(\\)]*)</p>')
wv_algs <- paste(wv_algs[[1]][,2],wv_algs[[1]][,4],wv_algs[[1]][,6])
library(stringr);library(png)
URL_cube_image_parser <- function(edges,edgesO,corners,cornersO){
  gsub('^(.{9})(.{9})(.{9}).*','\\1\\3\\2',vectors_to_string(edges,edgesO,corners,cornersO))
}
algorithms_wv <- data.frame(stringsAsFactors = F)
for(a in wv_algs){
  scr <-  gsub('\\(|\\)','',a)
  scr <- inverse_scr(scr)
  cor_orientation <- applyMoves(a)[[4]][applyMoves(a)[[3]][c(8,1,2)]]
  cor_orientation[1] <- ifelse(cor_orientation[1]==0,"F",ifelse(cor_orientation[1]==1,"L","T"))
  cor_orientation[2] <- ifelse(cor_orientation[2]==0,"A",ifelse(cor_orientation[2]==1,"U","I"))
  cor_orientation[3] <- ifelse(cor_orientation[3]==0,"C",ifelse(cor_orientation[3]==1,"M","D"))
  scr <-  gsub(' ','',scr)
  nazwa=paste(cor_orientation,collapse = '')
  download(paste('http://cube.crider.co.uk/visualcube.php?fmt=svg&size=200&alg=',scr,'.svg',sep = ''),nazwa)
  rsvg::rsvg_raw(nazwa) %>% writePNG(.,paste(nazwa,'png',sep='.'))
  cat(nazwa)
  algorithms_wv <- rbind(algorithms_wv,data.frame(alg=a,nazwa=nazwa))
}

algs <- dir(pattern='^F...png')
wv_alg <- sample(algs,1)
img <- readPNG(wv_alg)
grid::grid.raster(img)
cat(as.character(algorithms_wv$alg[algorithms_wv$nazwa==gsub('.png','',wv_alg)]))
