

nationalLandcover<-function(lTab,omitRare){
  #evergreens - 
  evergreens = subset(lTab,(grepl("evergreen",lTab$Label)|grepl("Evergreen",lTab$Label))&!grepl("Mixed",lTab$Label))
  evergreens = evergreens[order(-evergreens$ID),]
  evergreenCols = brewer.pal(nrow(evergreens),name="Greens")
  evergreenCols = col2rgb(evergreenCols)
  #evergreenCols= evergreenCols[,-1]
  evergreens$RGB1=evergreenCols[1,]
  evergreens$RGB2=evergreenCols[2,]
  evergreens$RGB3=evergreenCols[3,]
  evergreens$RGB1[grepl("Sparse NL",evergreens$Label,fixed=T)]=255
  evergreens$RGB2[grepl("Sparse NL",evergreens$Label,fixed=T)]=255
  evergreens$RGB3[grepl("Sparse NL",evergreens$Label,fixed=T)]=255
  
  
  lTab
  #mixed
  mix = subset(lTab,grepl("Mixed",lTab$Label))
  mix = mix[order(-mix$ID),]
  mixCols = brewer.pal(nrow(mix)+1,name="Oranges")
  mixCols = col2rgb(mixCols)
  mixCols=mixCols[,-1]
  mix$RGB1=mixCols[1,]
  mix$RGB2=mixCols[2,]
  mix$RGB3=mixCols[3,]
  
  #decid
  decid = subset(lTab,grepl("deciduous",lTab$Label)&!(grepl("mixed",lTab$Label)|grepl("Mixed",lTab$Label)))
  decid = decid[order(decid$ID),]
  decidCols = brewer.pal(nrow(decid)+1,name="Purples")
  decidCols = col2rgb(decidCols)
  decidCols=decidCols[,-1]
  decid$RGB1=decidCols[1,]
  decid$RGB2=decidCols[2,]
  decid$RGB3=decidCols[3,]
  
  #rare
  rare = subset(lTab,is.element(ID,omitRare$ID))
  rare$RGB1 = 35
  rare$RGB2 = 35
  rare$RGB3 = 35
  
  #wet
  wet = subset(lTab,grepl("Water",lTab$Label)|grepl("wet",lTab$Label)|grepl("Wet",lTab$Label)|grepl("bog",lTab$Label))
  wet = wet[order(wet$ID),]
  wetCols = brewer.pal(nrow(wet)+1,name="Blues")
  wetCols = col2rgb(wetCols)
  wetCols=wetCols[,-1]
  wet$RGB1=wetCols[1,]
  wet$RGB2=wetCols[2,]
  wet$RGB3=wetCols[3,]
  
  
  combo = rbind(evergreens,mix,decid,rare,wet)
  
  remainder = subset(lTab,!is.element(ID,combo$ID))
  remainder
  
  dryLow = subset(remainder,!grepl("young",remainder$Label,fixed=T)&!grepl("burns",remainder$Label,fixed=T)&!grepl("No Data",remainder$Label,fixed=T))
  dryLow = dryLow[order(dryLow$ID),]
  dryCols = brewer.pal(nrow(dryLow)+2,name="Greys")
  dryCols = col2rgb(dryCols)
  dryCols=dryCols[,-1]
  dryCols=dryCols[,-1]
  dryLow$RGB1=dryCols[1,]
  dryLow$RGB2=dryCols[2,]
  dryLow$RGB3=dryCols[3,]
  
  combo = rbind(evergreens,mix,decid,rare,wet,dryLow)
  
  remainder = subset(lTab,!is.element(ID,combo$ID))
  remainder
  remainder$RGB1[grepl("burns",remainder$Label)] = 197
  remainder$RGB2[grepl("burns",remainder$Label)] = 27
  remainder$RGB3[grepl("burns",remainder$Label)] = 138
  
  remainder$RGB1[grepl("young",remainder$Label)] = 250
  remainder$RGB2[grepl("young",remainder$Label)] = 159
  remainder$RGB3[grepl("young",remainder$Label)] = 181
  
  
  combo = rbind(remainder,evergreens,mix,decid,wet,dryLow,rare)
  return(combo)
  
}