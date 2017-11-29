scr<-fread('WCA_export_Scrambles.tsv',select = c(3,8))
scr<-scr[eventId=='333']

sapply(scr$scramble,applyMoves)

all<-lapply(scr$scramble,function(x)as.character(applyMoves(x)))
data.frame(matrix(unlist(all),ncol = 4,byrow = T))->ku
ku$scramble=scr$scramble


