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