library(data.table)
# Moves definitions -------------------------------------------------------

applyMoves<-function(x){
  scr=unlist(strsplit(x,split=' '))
  edges=1:12
  corners=1:8
  edgesOrientation=rep(0,12)
  cornerOrientation=rep(0,8)
  for(a in scr){
    if(grepl('2',a)){
      a<-gsub('2','',a)
        if(a=='R'){
          edges[c(4,7,8,12)]<-edges[c(8,4,12,7)]
          corners[c(4,3,7,8)]<-corners[c(8,4,3,7)]
          edgesOrientation[c(4,7,8,12)]<-edgesOrientation[c(8,4,12,7)]
          cornerOrientation[c(4,3,7,8)]<-cornerOrientation[c(8,4,3,7)]
          cornerOrientation[c(4,3,7,8)]<-(cornerOrientation[c(4,3,7,8)]+c(2,1,2,1))%%3
        }else 
          if(a=="L"){
                edges[c(5,2,6,10)]<-edges[c(2,6,10,5)]
                corners[c(5,1,2,6)]<-corners[c(1,2,6,5)]
                edgesOrientation[c(5,2,6,10)]<-edgesOrientation[c(2,6,10,5)]
                cornerOrientation[c(5,1,2,6)]<-cornerOrientation[c(1,2,6,5)]
                cornerOrientation[c(1,2,6,5)]<-(cornerOrientation[c(1,2,6,5)]+c(1,2,1,2))%%3
              }else 
                if(a=="U"){
                  edges[c(1,2,3,4)]<-edges[c(4,1,2,3)]
                  corners[c(1,2,3,4)]<-corners[c(4,1,2,3)]
                  edgesOrientation[c(1,2,3,4)]<-edgesOrientation[c(4,1,2,3)]
                  cornerOrientation[c(1,2,3,4)]<-cornerOrientation[c(4,1,2,3)]
                    }else 
                      if(a=="D"){
                        edges[c(9,10,11,12)]->edges[c(12,9,10,11)]
                        corners[c(5,6,7,8)]->corners[c(8,5,6,7)]
                        edgesOrientation[c(9,10,11,12)]->edgesOrientation[c(12,9,10,11)]
                        cornerOrientation[c(5,6,7,8)]->cornerOrientation[c(8,5,6,7)]
                      }else 
                        if(a=="F"){
                          edges[c(1,8,9,5)]<-edges[c(5,1,8,9)]
                          corners[c(1,4,8,5)]<-corners[c(5,1,4,8)]
                          edgesOrientation[c(1,8,9,5)]<-edgesOrientation[c(5,1,8,9)]
                          cornerOrientation[c(1,4,8,5)]<-cornerOrientation[c(5,1,4,8)]
                          edgesOrientation[c(1,8,9,5)]<-(edgesOrientation[c(1,8,9,5)]+1)%%2
                          cornerOrientation[c(1,4,8,5)]<-(cornerOrientation[c(1,4,8,5)]+c(2,1,2,1))%%3
                        }else 
                          if(a=="B"){
              edges[c(3,6,11,7)]<-edges[c(7,3,6,11)]
              corners[c(2,6,7,3)]<-corners[c(3,2,6,7)]
              edgesOrientation[c(3,6,11,7)]<-edgesOrientation[c(7,3,6,11)]
              cornerOrientation[c(2,6,7,3)]<-cornerOrientation[c(3,2,6,7)]
              edgesOrientation[c(3,6,11,7)]<-(edgesOrientation[c(3,6,11,7)]+1)%%2
              cornerOrientation[c(2,6,7,3)]<-(cornerOrientation[c(2,6,7,3)]+c(1,2,1,2))%%3
            }} 
    if(a=='R'){
      edges[c(4,7,8,12)]<-edges[c(8,4,12,7)]
      corners[c(4,3,7,8)]<-corners[c(8,4,3,7)]
      edgesOrientation[c(4,7,8,12)]<-edgesOrientation[c(8,4,12,7)]
      cornerOrientation[c(4,3,7,8)]<-cornerOrientation[c(8,4,3,7)]
      cornerOrientation[c(4,3,7,8)]<-(cornerOrientation[c(4,3,7,8)]+c(2,1,2,1))%%3
    }else 
      if(a=="R'"){
        edges[c(8,4,12,7)]<-edges[c(4,7,8,12)]
        corners[c(8,4,3,7)]<-corners[c(4,3,7,8)]
        edgesOrientation[c(8,4,12,7)]<-edgesOrientation[c(4,7,8,12)]
        cornerOrientation[c(8,4,3,7)]<-cornerOrientation[c(4,3,7,8)]
        cornerOrientation[c(8,4,3,7)]<-(cornerOrientation[c(8,4,3,7)]+c(1,2,1,2))%%3
      }else
        if(a=="L'"){
          edges[c(2,6,10,5)]<-edges[c(5,2,6,10)]
          corners[c(1,2,6,5)]<-corners[c(5,1,2,6)]
          edgesOrientation[c(2,6,10,5)]<-edgesOrientation[c(5,2,6,10)]
          cornerOrientation[c(1,2,6,5)]<-cornerOrientation[c(5,1,2,6)]
          cornerOrientation[c(1,2,6,5)]<-(cornerOrientation[c(1,2,6,5)]+c(1,2,1,2))%%3
        }else
          if(a=="L"){
            edges[c(5,2,6,10)]<-edges[c(2,6,10,5)]
            corners[c(5,1,2,6)]<-corners[c(1,2,6,5)]
            edgesOrientation[c(5,2,6,10)]<-edgesOrientation[c(2,6,10,5)]
            cornerOrientation[c(5,1,2,6)]<-cornerOrientation[c(1,2,6,5)]
            cornerOrientation[c(1,2,6,5)]<-(cornerOrientation[c(1,2,6,5)]+c(1,2,1,2))%%3
          }else
            if(a=="U"){
              edges[c(1,2,3,4)]<-edges[c(4,1,2,3)]
              corners[c(1,2,3,4)]<-corners[c(4,1,2,3)]
              edgesOrientation[c(1,2,3,4)]<-edgesOrientation[c(4,1,2,3)]
              cornerOrientation[c(1,2,3,4)]<-cornerOrientation[c(4,1,2,3)]
            }else
              if(a=="U'"){
                edges[c(1,2,3,4)]<-edges[c(2,3,4,1)]
                corners[c(1,2,3,4)]<-corners[c(2,3,4,1)]
                edgesOrientation[c(1,2,3,4)]<-edgesOrientation[c(2,3,4,1)]
                cornerOrientation[c(1,2,3,4)]<-cornerOrientation[c(2,3,4,1)]
              }else
                if(a=="D'"){
                  edges[c(9,10,11,12)]<-edges[c(12,9,10,11)]
                  corners[c(5,6,7,8)]<-corners[c(8,5,6,7)]
                  edgesOrientation[c(9,10,11,12)]<-edgesOrientation[c(12,9,10,11)]
                  cornerOrientation[c(5,6,7,8)]<-cornerOrientation[c(8,5,6,7)]
                }else
                  if(a=="D"){
                    edges[c(12,9,10,11)]<-edges[c(9,10,11,12)]
                    corners[c(8,5,6,7)]<-corners[c(5,6,7,8)]
                    edgesOrientation[c(12,9,10,11)]<-edgesOrientation[c(9,10,11,12)]
                    cornerOrientation[c(8,5,6,7)]<-cornerOrientation[c(5,6,7,8)]
                  }else
                    if(a=="F"){
                      edges[c(1,8,9,5)]<-edges[c(5,1,8,9)]
                      corners[c(1,4,8,5)]<-corners[c(5,1,4,8)]
                      edgesOrientation[c(1,8,9,5)]<-edgesOrientation[c(5,1,8,9)]
                      cornerOrientation[c(1,4,8,5)]<-cornerOrientation[c(5,1,4,8)]
                      edgesOrientation[c(1,8,9,5)]<-(edgesOrientation[c(1,8,9,5)]+1)%%2
                      cornerOrientation[c(1,4,8,5)]<-(cornerOrientation[c(1,4,8,5)]+c(2,1,2,1))%%3
                    }else
                      if(a=="F'"){
                        edges[c(5,1,8,9)]<-edges[c(1,8,9,5)]
                        corners[c(5,1,4,8)]<-corners[c(1,4,8,5)]
                        edgesOrientation[c(5,1,8,9)]<-edgesOrientation[c(1,8,9,5)]
                        cornerOrientation[c(5,1,4,8)]<-cornerOrientation[c(1,4,8,5)]
                        edgesOrientation[c(1,8,9,5)]<-(edgesOrientation[c(1,8,9,5)]+1)%%2
                        cornerOrientation[c(1,4,8,5)]<-(cornerOrientation[c(1,4,8,5)]+c(2,1,2,1))%%3
                      }else
                        if(a=="B'"){
                          edges[c(7,3,6,11)]<-edges[c(3,6,11,7)] 
                          corners[c(3,2,6,7)]<-corners[c(2,6,7,3)]          
                          edgesOrientation[c(7,3,6,11)]<-edgesOrientation[c(3,6,11,7)]
                          cornerOrientation[c(3,2,6,7)]<-cornerOrientation[c(2,6,7,3)]
                          edgesOrientation[c(7,3,6,11)]<-(edgesOrientation[c(7,3,6,11)]+1)%%2
                          cornerOrientation[c(2,6,7,3)]<-(cornerOrientation[c(2,6,7,3)]+c(1,2,1,2))%%3
                        }else
                          if(a=="B"){
                            edges[c(3,6,11,7)]<-edges[c(7,3,6,11)]
                            corners[c(2,6,7,3)]<-corners[c(3,2,6,7)]
                            edgesOrientation[c(3,6,11,7)]<-edgesOrientation[c(7,3,6,11)]
                            cornerOrientation[c(2,6,7,3)]<-cornerOrientation[c(3,2,6,7)]
                            edgesOrientation[c(3,6,11,7)]<-(edgesOrientation[c(3,6,11,7)]+1)%%2
                            cornerOrientation[c(2,6,7,3)]<-(cornerOrientation[c(2,6,7,3)]+c(1,2,1,2))%%3
                          }
    }
  return(list(edges,edgesOrientation,corners,cornerOrientation))
}

parse.wide.moves<-function(x){
  scr<-unlist(strsplit(x,split = ' '))
  for(a in length(x):1){
    if(grepl('w',x[a])){
      move<-gsub("(.)w['2]?",'\\1',x[a])
      move<-c('R','L','U','D','F','B')[c('L','R','D','U','B','F')==move]
      x[a]<-paste(move,gsub(".w(['2]?)",'\\1',x[a]),sep = '')
      if(a!=length(x)){
        if(x[a]=='R2'|x[a]=='L2'){
        for(b in (a+1):length(x)){
          move<-gsub("(.)['2]?",'\\1',x[b])
          move<-c('R','L','U','D','F','B')[c('R','L','D','U','B','F')==move]
          x[b]<-paste(move,gsub(".(['2]?)",'\\1',x[b]),sep = '')
        }
        }else if(x[a]=='U2'|x[a]=='D2'){
          for(b in (a+1):length(x)){
            move<-gsub("(.)['2]?",'\\1',x[b])
            move<-c('R','L','U','D','F','B')[c('L','R','U','D','B','F')==move]
            x[b]<-paste(move,gsub(".(['2]?)",'\\1',x[b]),sep = '')
          }
        }else if(x[a]=='F2'|x[a]=='B2'){
          for(b in (a+1):length(x)){
            move<-gsub("(.)['2]?",'\\1',x[b])
            move<-c('R','L','U','D','F','B')[c('L','R','D','U','F','B')==move]
            x[b]<-paste(move,gsub(".(['2]?)",'\\1',x[b]),sep = '')
          }
        }else if(x[a]=="L'"|x[a]=='R'){
          for(b in (a+1):length(x)){
            move<-gsub("(.)['2]?",'\\1',x[b])
            move<-c('R','L','U','D','F','B')[c('R','L','F','B','D','U')==move]
            x[b]<-paste(move,gsub(".(['2]?)",'\\1',x[b]),sep = '')
          }
        }else if(x[a]=="L"|x[a]=="R'"){
          for(b in (a+1):length(x)){
            move<-gsub("(.)['2]?",'\\1',x[b])
            move<-c('R','L','U','D','F','B')[c('R','L','B','F','U','D')==move]
            x[b]<-paste(move,gsub(".(['2]?)",'\\1',x[b]),sep = '')
          }
        }else if(x[a]=="U"|x[a]=="D'"){
          for(b in (a+1):length(x)){
            move<-gsub("(.)['2]?",'\\1',x[b])
            move<-c('R','L','U','D','F','B')[c('B','F','U','D','R','L')==move]
            x[b]<-paste(move,gsub(".(['2]?)",'\\1',x[b]),sep = '')
          }
        }else if(x[a]=="U'"|x[a]=="D"){
          for(b in (a+1):length(x)){
            move<-gsub("(.)['2]?",'\\1',x[b])
            move<-c('R','L','U','D','F','B')[c('F','B','U','D','L','R')==move]
            x[b]<-paste(move,gsub(".(['2]?)",'\\1',x[b]),sep = '')
          }
        }else if(x[a]=="F'"|x[a]=="B"){
          for(b in (a+1):length(x)){
            move<-gsub("(.)['2]?",'\\1',x[b])
            move<-c('R','L','U','D','F','B')[c('D','U','R','L','F','B')==move]
            x[b]<-paste(move,gsub(".(['2]?)",'\\1',x[b]),sep = '')
          }
        }else if(x[a]=="F"|x[a]=="B'"){
          for(b in (a+1):length(x)){
            move<-gsub("(.)['2]?",'\\1',x[b])
            move<-c('R','L','U','D','F','B')[c('U','D','L','R','F','B')==move]
            x[b]<-paste(move,gsub(".(['2]?)",'\\1',x[b]),sep = '')
          }
        }
        
      }
      
    }
  }
}

inverse_scr <- function(x){
  scr <- rev(unlist(strsplit(x,split=' ')))
  scr <- ifelse(grepl("'",scr),gsub("'",'',scr),
                ifelse(grepl("^[BURLFD]$",scr),gsub("(.)","\\1'",scr),scr))
  return(paste(scr,collapse = ' '))
}
