vectors_to_string <- function(edges_scr,edges_or,corners,cornersEO){
  eo.p<-stringCube[edges_scr*2-1+24+edges_or]
  eu.p<-stringCube[edges_scr*2-1+24+(edges_or+1)%%2]
  co.p<-stringCube[corners*3-2+cornersEO]
  cu1.p<-stringCube[corners*3-2+(cornersEO+c(rep(1,4),rep(2,4)))%%3]
  cu2.p<-stringCube[corners*3-2+(cornersEO+c(rep(2,4),rep(1,4)))%%3]
  
  return(caseTostring(eo.p,eu.p,co.p,cu1.p,cu2.p))
}
