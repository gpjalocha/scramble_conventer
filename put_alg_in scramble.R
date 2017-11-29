scramble_cycle_edges<-function(from,to,oriented){
  edges<-1:12
  edges[c(from,to)]<-edges[c(to,from)]
  edges[-c(from)]<-sample(edges[-c(from)])
  
  #push alg in 1st cycle
  edges <- vec2cyclist_single(edges)
  edges
  if(!(all(c(from,to)%in%edges[[1]]))&length(edges[[1]])>2&(1%in%edges[[1]])){
    #gdzie jest cykl?
    index <- which(sapply(edges, function(u)
      all(c(from,to) %in% u)))
    replInd <- sample(2:(ifelse(length(edges[[1]])%%2==0,
                                length(edges[[1]])-1,
                                length(edges[[1]]))),1)
    replInd <- c(replInd-replInd%%2,replInd-replInd%%2+1)
    ind <- which(edges[[index]]%in%c(from,to))
    #replace
    rep <- edges[[1]][replInd]
    edges[[1]][replInd] <- c(from,to)
    edges[[index]][sort(ind)] <- rep
  }
  edges
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
  edges
  #repair eo afterwards...
    edgesO<-sample(c(0,1),12,replace = T)
    edgesO[from]<-oriented
    edgesO<-reperEO(edgesO,ind=(1:12)[-from][1])
  
    
  if(length(edges[[1]])>2&(1%in%edges[[1]])){
      or <- edgesO[edges[[1]][1:(which(edges[[1]]==from)-1)]]
      edgesO[edges[[1]][1:(which(edges[[1]]==from)-1)]][1] <- 
        ifelse((sum(or)%%2)==1,(or[1]+1)%%2,or[1])
      edgesO <- reperEO(edgesO,ind=(1:12)[-edges[[1]][1:(which(edges[[1]]==from)-1)]][1])
  }
    edges
  edges <- cyclist2word_single(edges,12)
  
  
  return(list(as.numeric(edges),edgesO))
}
