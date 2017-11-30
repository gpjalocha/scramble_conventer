source("alg_in_scramble.R")
source("parse Scrambles.R")
source("put_alg_in scramble.R")
source("reper pairity.R")
source("scramble to string.R")
source("string_cube_representation.R")

reperPairity<-function(x,y,ind=c(1,2),pieces='E'){
s=0
for(a in 1:12){
  for(b in a:12){
    if(x[b]>x[a]){
      s=s+1
    }
  }
}

z=0
for(a in 1:8){
  for(b in a:8){
    if(y[b]>y[a]){
      z=z+1
    }
  }
}

if((s%%2!=z%%2)&pieces=='E'){
  index<-(1:12)
  index[c(ind[1],ind[2])]<-index[c(ind[2],ind[1])]
  return(x[index])}
else if(s%%2!=z%%2){
  index<-(1:8)
  index[c(ind[1],ind[2])]<-index[c(ind[2],ind[1])]
  return(y[index])
}
else if(pieces=='E'){
  return(x)}else{return(y)}
}
reperCO<-function(x,ind=1){
  s=sum(x)%%3
    if(s==0){return(x)}else{return(
      c((x[ind]-s)%%3,x[setdiff(1:8,ind)])[order(c(ind,setdiff(1:8,ind)))]
      )
      }
}
reperEO<-function(x,ind=1){
  s=sum(x)%%2
  if(s==0){return(x)}else{return(
    c((x[ind]+s)%%2,x[setdiff(1:12,ind)])[order(c(ind,setdiff(1:12,ind)))]
  )
  }
}

#conditional scrambler!-----------
scrambles<-c()
for(a in 1:30){
edges<-sample(1:12)
edgesEO<-sample(c(0,1),12,replace = T)
edgesEO<-reperEO(edgesEO)
#edge twists
ind<-sample(c(1:3,5:8),1)

# corners<-sample(1:8)
# cornersEO<-sample(c(0,1,2),8,replace = T)
# cornersEO<-reperCO(cornersEO,setdiff(2:8,ind))

#corner twists
corners<-1:8
corners[-ind]<-sample(corners[-ind])
cornersEO<-sample(c(0,2),8,T)
cornersEO[ind]<-sample(c(1,2),1)

cornersEO<-reperCO(cornersEO,setdiff(1:8,ind))

if(sum((corners[-ind]==(1:8)[-ind])&(cornersEO[-ind]!=0)&c(F,rep(T,7))[-ind])>0){
  whereT<-which((corners[-ind]==(1:8)[-ind])&(cornersEO[-ind]!=0))
  whereF<-which(!((corners[-ind]==(1:8)[-ind])&(cornersEO[-ind]!=0)))
  if(length(whereT)==1){
    corners[-ind][c(whereT,whereF[1])]<-corners[-ind][c(whereF[1],whereT)]
  }else{
    corners[-ind][whereT]<-corners[-ind][c(whereT[length(whereT)],whereT[1:(length(whereT)-1)])]
  }}


# edges eo- 2 twisted -----------------------------------------------------
# edges[-ind]<-sample(edges[-ind])
# edgesEO<-sample(c(0,1),12,T)
# edgesEO[ind]<-1
# 
# edgesEO[1]<-ifelse(sum(edgesEO)%%2==1,(edgesEO[1]+1)%%2,edgesEO[1])
# if(sum((edges[-ind]==(1:12)[-ind])&(edgesEO[-ind]==1)&c(F,rep(T,11))[-ind])>0){
#   whereT<-which((edges[-ind]==(1:12)[-ind])&(edgesEO[-ind]==1))
#   whereF<-which(!((edges[-ind]==(1:12)[-ind])&(edgesEO[-ind]==1)))
#   if(length(whereT)==1){
#   edges[-ind][c(whereT,whereF[1])]<-edges[-ind][c(whereF[1],whereT)]
# }else{
#   edges[-ind][whereT]<-edges[-ind][c(whereT[length(whereT)],whereT[1:(length(whereT)-1)])]
# }}


# 
# ind=2
# corners<-1:8
# corners[-ind]<-sample(corners[-ind])
# cornersEO<-sample(c(0,2),8,T)
# cornersEO[ind]=sample(c(1,2),1)
# cornersEO[-ind][1]<-ifelse(sum(cornersEO)%%3!=1,
#                            (cornersEO[-ind][1]-sum(cornersEO)%%3)%%3,
#                            cornersEO[-ind][1])

edges<-reperPairity(edges,corners,setdiff(1:12,ind)[1:2])

eo.p<-stringCube[edges*2-1+24+edgesEO]
eu.p<-stringCube[edges*2-1+24+(edgesEO+1)%%2]
co.p<-stringCube[corners*3-2+cornersEO]
cu1.p<-stringCube[corners*3-2+(cornersEO+c(rep(1,4),rep(2,4)))%%3]
cu2.p<-stringCube[corners*3-2+(cornersEO+c(rep(2,4),rep(1,4)))%%3]

scrambles<-c(scrambles,caseTostring(eo.p,eu.p,co.p,cu1.p,cu2.p))
}
scr<-c()
for(a in scrambles){
  scr<-c(scr,as.character(read.csv(paste(
    'http://localhost:8080/',a,sep=''
  ),sep='\t',header = F)[2,]))
}


stringCube<-c(
  c('U','F','R'),
  c('U','L','F'),
  c('U','B','L'),
  c('U','R','B'),
  c('D','R','F'),
  c('D','F','L'),
  c('D','L','B'),
  c('D','B','R'),
  c('U','F'),
  c('U','L'),
  c('U','B'),
  c('U','R'),
  c('F','L'),
  c('B','L'),
  c('B','R'),
  c('F','R'),
  c('D','F'),
  c('D','L'),
  c('D','B'),
  c('D','R')
)
eo.p<-stringCube[edges*2-1+24+edgesEO]
eu.p<-stringCube[edges*2-1+24+(edgesEO+1)%%2]
co.p<-stringCube[corners*3-2+cornersEO]
cu1.p<-stringCube[corners*3-2+(cornersEO+c(rep(1,4),rep(2,4)))%%3]
cu2.p<-stringCube[corners*3-2+(cornersEO+c(rep(2,4),rep(1,4)))%%3]


caseTostring<-function(edges,edgesEO,corners,cornersEO1,cornersEO2){
  str<-rep(NA,54)
  str[c(5,14,23,32,41,50)]<-c('U','R','F','D','L','B')
  str[setdiff(1:54,c(5,14,23,32,41,50))]<-
    c(edges,
      edgesEO,#12
      corners,#24
      cornersEO1,#32
      cornersEO2#40
    )[c(27,3,28,2,4,26,1,25,
        41,16,36,20,19,45,24,40,
        42,13,33,5,8,46,21,37,
        30,9,29,10,12,31,11,32,
        43,14,34,18,17,47,22,38,
        44,15,35,7,6,48,23,39)]
return(
  paste(str,collapse = '')
)
}

caseTostring_rev<-function(edges,edgesEO,corners,cornersEO1,cornersEO2){
  str<-rep(NA,54)
  str[c(5,14,23,32,41,50)]<-c('U','R','F','D','L','B')
  str[c(27,3,28,2,4,26,1,25,
                                        41,16,36,20,19,45,24,40,
                                        42,13,33,5,8,46,21,37,
                                        30,9,29,10,12,31,11,32,
                                        43,14,34,18,17,47,22,38,
                                        44,15,35,7,6,48,23,39)]<-
    c(edges,
      edgesEO,#12
      corners,#24
      cornersEO1,#32
      cornersEO2#40
    )[setdiff(1:54,c(5,14,23,32,41,50))]
  return(
    paste(str,collapse = '')
  )
}
dane<-data.frame()

  a<-sample(1:12)
  cyc<-lapply(vec2cyclist_single(a))
  ncycles<-length(cyc)
  cyclen<-sapply(cyc,length)
  totalCyclen<-sum(cyclen)+ncycles-(a[1]==1)*2
  dane<-rbind(dane,data.frame(
    ncycles,cyclen=paste(sort(cyclen),collapse = ','),totalCyclen
  ))
}
perms<-replicate(1000000,sample(1:12),simplify = T)
cyc<-apply(perms,2,vec2cyclist_single)
ncycles<-sapply(cyc,length)
cyclen<-sapply(cyc,function(x)sapply(x,length))
totalCyclen<-sapply(cyclen,sum)-(perms[1,]!=1)*2+ncycles
dane<-data.frame(
  ncycles,cyclen=sapply(cyclen,function(x)paste(sort(x),collapse = ',')),totalCyclen
)

# functions for conditional scrambler -------------------------------------

scramble_cycle_edges<-function(from,to,oriented){
  edges<-1:12
  edges[c(from,to)]<-edges[c(to,from)]
  edges[-c(from)]<-sample(edges[-c(from)])
  
  #push alg in 1st cycle
  edges <- vec2cyclist_single(edges)
  if(!(all(c(from,to)%in%edges[[1]]))&length(edges[[1]])>2&(1%in%edges[[1]])){
    #gdzie jest cykl?
    index <- which(sapply(edges, function(u)
      all(c(from,to) %in% u)))
    replInd <- sample(2:(length(edges[[1]])),1)
    replInd <- c(replInd-replInd%%2,replInd-replInd%%2+1)
    ind <- which(edges[[index]]%in%c(from,to))
    #replace
    rep <- edges[[1]][replInd]
    edges[[1]][replInd] <- c(from,to)
    edges[[index]][sort(ind)] <- rep
  }
  if(from%in%edges[[1]]){
    if(which(from==edges[[1]])%%2==1){
    # legal_ind <- seq(2,length(edges[[1]]),by=2)
    # if(length(edges[[1]])==legal_ind[length(legal_ind)]){legal_ind <- legal_ind[-length(legal_ind)]}
    # actual_ind <- which(from==edges[[1]])
    # actual_ind <- c(actual_ind,actual_ind+1)
    # legal_ind <- sample(legal_ind,1)
    # legal_ind <- c(legal_ind,legal_ind+1)
    # ind_ <- edges[[1]][setdiff(legal_ind,actual_ind)]
    # edges[[1]][legal_ind] <- c(from,to)
    # edges[[1]][setdiff(actual_ind,legal_ind)] <- ind_
      v <- length(edges[[1]])
    if(edges[[1]][v]==to){edges[[1]][c(v-2,v-1,v)] <- edges[[1]][c(v-1,v,v-2)]}else{
      v<- which(edges[[1]]==to)+1
      edges[[1]][c(v-1,v,v-2)] <- edges[[1]][c(v-2,v-1,v)]
    }
    }}
  #repair eo afterwards...
  
  if(length(edges[[1]])>2&1%in%edges[[1]]){
  edgesO<-sample(c(0,1),12,replace = T)
  edgesO[from]<-oriented
  edgesO<-reperEO(edgesO,ind=(1:12)[-from][1])
  
  if(from%in%edges[[1]]){
    or <- edgesO[edges[[1]][1:(which(edges[[1]]==from)-1)]]
    edgesO[edges[[1]][1:(which(edges[[1]]==from)-1)]][1] <- 
      ifelse((sum(or)%%2)==1,(or[1]+1)%%2,or[1])
    edgesO <- reperEO(edgesO,ind=(1:12)[-edges[[1]][1:(which(edges[[1]]==from)-1)]][1])
  }}
  edges <- cyclist2word_single(edges)
  return(list(as.numeric(edges),edgesO))
}

#dict
edges_dict<-data.frame(letter=c('B','A','C','J','P','N','L','E','F','G','H',
                                'W','M','I','Z','D','U','S','K','R','O','T'),
                       edgeN=rep(2:12,2),
                       edgeO=c(rep(0,11),rep(1,11)))
corner_dict<-data.frame(letter=c('B','A','C','F','G','H','E',
                                 'J','D','S','Z','P','U','L',
                                 'W','M','I','K','R','O','T'),
                       edgeN=rep(c(1,2,4:8),3),
                       edgeO=c(rep(0,7),rep(1,7),rep(2,7)))
str_to_case<-function(x){
  str<-strsplit(x,split='')
  str<-lapply(str,str_to_upper)
  num<-lapply(str,function(y)edges_dict$edgeN[match(y,edges_dict$letter)])
  Or<-lapply(str,function(y)ifelse(sum(edges_dict$edgeO[match(y,edges_dict$letter)])%%2==0,0,1))
  return(lapply(1:length(str),function(x)c(num[[x]],Or[[x]])))
}



# cases with certain alg --------------------------------------------------
N <- 30
algino <- c('la','al','na','an','ca','ac')
cases<-str_to_case(sample(as.character(algino),N,replace = T))

edges<-lapply(1:N,function(x)scramble_cycle_edges(cases[[x]][1],cases[[x]][2],cases[[x]][3]))

#generate
scrambles<-c()
for(a in 1:N){
  cornersEO<-sample(c(0,1,2),8,replace = T)
  cornersEO<-reperCO(cornersEO)
  corners<-sample(1:8)
  edges_scr=unlist(edges[[a]][1])
  edges_or=unlist(edges[[a]][2])
  
  corners<-reperPairity(edges_scr,corners,c(1,2),'c')
  
  eo.p<-stringCube[edges_scr*2-1+24+edges_or]
  eu.p<-stringCube[edges_scr*2-1+24+(edges_or+1)%%2]
  co.p<-stringCube[corners*3-2+cornersEO]
  cu1.p<-stringCube[corners*3-2+(cornersEO+c(rep(1,4),rep(2,4)))%%3]
  cu2.p<-stringCube[corners*3-2+(cornersEO+c(rep(2,4),rep(1,4)))%%3]
  
  scrambles<-c(scrambles,caseTostring(eo.p,eu.p,co.p,cu1.p,cu2.p))
}
scr<-c()
for(a in scrambles){
  scr<-c(scr,as.character(read.csv(paste(
    'http://localhost:8080/',a,sep=''
  ),sep='\t',header = F)[2,]))
}
algi_all <- sapply(scr,inverse_scr)
cat(algi_all)

solve_to_str<-function(e,c,eo,co){
  E<-do.call('c',lapply(vec2cyclist_single(e),
            function(x){if(any(x==1)){x[-1]}else{c(x,x[1])}}))
  O<-do.call('c',lapply(vec2cyclist_single(c),
            function(x){if(any(x==3)){b<-which(x==3);
            x<-x[c((b+1):length(x),1:b)];
            x<-x[!is.na(x)];
            x<-x[x!=3];return(x)}else{c(x,x[1])}}))
  EO<-cumsum(eo[E])%%2
  CO<-cumsum(co[O])%%3
  
  edges_str<-inner_join(data.frame(edgeN=E,edgeO=EO),edges_dict)$letter
  corner_str<-inner_join(data.frame(cornerN=O,cornerO=CO),corner_dict)$letter
  edges_str<-gsub('(..)','\\1 ',paste(edges_str,collapse = ''))
  corner_str<-gsub('(..)','\\1 ',paste(corner_str,collapse = ''))
  
 if(any((e==1:12)&(eo==1))){
                  obrotyE<-edges_dict$letter[
                    match(e[which((e==1:12)&(eo==1))],
                          edges_dict$edgeN)]}else{
                            obrotyE<-''}
 if(any((c==1:8)&(co!=0))){
   obrotyC<-inner_join(data.frame(cornerN=c[(c==1:8)&(co!=0)],
                                        cornerO=co[(c==1:8)&(co!=0)]
                                        ),corner_dict)$letter}else{
                                          obrotyC<-''}
  
  cat(corner_str,'\n',edges_str,'\n')
  if(obrotyE[1]!=''){cat('obroty krawędzie: ',as.character(obrotyE),'\n')}
  if(obrotyC[1]!=''){cat('obroty rogi: ',as.character(obrotyC))}
}

#custom
scrambles_print<-c()
  cornersEO<-sample(c(0,1,2),8,replace = T)
  cornersEO<-reperCO(cornersEO)
  corners<-sample(1:8)
  edges_scr=unlist(edges[[a]][1])
  edges_or=unlist(edges[[a]][2])
  
  corners<-reperPairity(edges_scr,corners,c(1,2),'c')

  solve_to_str(edges_scr,corners,edges_or,cornersEO)
a=a+1