# functions for preprocessing moos data


### translate Treetyp names from German in latin short
trlateTree <- function (main){
  main$treetyp <-as.character(main$treetyp)
  #rename treetyp
  main$treetyp[main$treetyp=="Buche"]     <-"FS_" #fargus sylvatica
  main$treetyp[main$treetyp=="Eiche"]     <-"QP_" #Quercus petrea cf
  main$treetyp[main$treetyp=="Fichte"]    <-"PA_" #Picea abies
  main$treetyp[main$treetyp=="Hainbuche"] <-"CB_" #Carpinus betulus
  main$treetyp[main$treetyp=="Birke"]     <-"BP_" #Betula pendula
  main$treetyp[main$treetyp=="Laerche"]   <-"LD_" #Larix decidua
  main$treetyp[main$treetyp=="Douglasie"] <-"PM_" #Pseudotsuga menziesii
  main$treetyp[main$treetyp=="Eberesche"] <-"SA_" #Sorbus aucuparia
  main$treetyp[main$treetyp=="Ahorn"]     <-"AS_" #Acer spec, unknown species
  print("translate done")
  return(main)}


### translate department treetyp in english
trlateDepart <- function(main){
  main$dep_ID <-as.character(main$dep_ID)
  main$dep_ID[main$dep_ID=="BU"]     <-"FS_" #fargus sylvatica
  main$dep_ID[main$dep_ID=="EI"]     <-"QP_" #Quercus petrea cf
  main$dep_ID[main$dep_ID=="DG"]     <-"PM_" #Pseudotsuga menziesii
  main$dep_ID[main$dep_ID=="FI"]     <-"PA_" #Picea abies
  main$dep_ID[main$dep_ID=="LA"]     <-"LD_" #Larix decidua
  main$dep_ID[main$dep_ID=="CL"]     <-"CL_" # clearing
  main$dep_ID[main$dep_ID=="XX"]     <-"XX_" # unknown
  print("translate done")
  return(main)}

###check for empty, " " or NA cells
# any(main$cover==" "|main$cover=="None"|is.na(main$cover) )

# following function is not needed, eventually there could be problems with facotr/character
#mincov <- function(main){
# main$cover <-as.character(main$cover)
#  
#  # set NA or " " cells to "None"
#  main$cover[is.na(main$cover)] <- "None" # set "None" to NA cells
#  main$cover[main$cover==""] <- "None" #set "None" to " " cells
#  main$cover[main$cover=="None"] <- "r" # replace all "None" with "r"
#  return(main)}

### reclass BB sclae to numeric
reclassBB <- function(main){
  main$cover <-as.character(main$cover)
  #check if any values are not in BB scale
  check<-any(main$cover!="r"
             &main$cover!="+"
             &main$cover!=1
             &main$cover!=2
             &main$cover!=3
             &main$cover!=4
             &main$cover!=5)
  if(check==FALSE){print("only BBscale values detected")} else {stop("there are missing values/empty cells")}
  
  main$cover[main$cover=="r"]<- 0.1 # far less 1
  main$cover[main$cover=="+"]<- 0.5 # less 1
  main$cover[main$cover==1]  <- 2.5 # less 5
  main$cover[main$cover==2]  <- 15  #mean(5:25)
  main$cover[main$cover==3]  <- 37.5#mean(25:50)
  main$cover[main$cover==4]  <- 62.5#mean(50:75)
  main$cover[main$cover==5]  <- 87.5#mean(75:100)
  main$cover <-as.numeric(main$cover)
  print("reclass to numeric done")
  return(main)
  }


### reclass numeric values back to BB
conv_num2bb <- function(dfp){
  dfp[dfp>=0.1 & dfp<0.5]  <-100
  dfp[dfp>=0.5 & dfp<2.5]  <-500
  dfp[dfp>=2.5 & dfp<15]  <- 1000
  dfp[dfp>=15 & dfp<37.5]   <- 2000
  dfp[dfp>=37.5 & dfp<62.5] <-3000
  dfp[dfp>=62.5 & dfp<87.5] <-4000
  dfp[dfp>=87.5 & dfp<100]<-5000
  
  dfp[dfp==1000] <- 1
  dfp[dfp==2000] <- 2
  dfp[dfp==3000] <- 3
  dfp[dfp==4000] <- 4
  dfp[dfp==5000] <- 5
  dfp[dfp==100] <- "r"
  dfp[dfp==500] <- "+"
  dfp[dfp==0] <- " "
  dfp <-t(dfp)
  print("reclass to BBscale done")
  return(dfp)
}


delete.species <-function(tab,name){
  species <- which(colnames(tab)==name)
  tab_del <- tab[,-species]
  if(any(rowSums(tab_del)==0)==TRUE){
    tab_clean <- tab_del[-which(rowSums(tab_del)==0),]
    print("done clean")
    return(tab_clean)
  } else {
    print("done del")
    return(tab_del)
  }
  
}


ind_groups <- function(df){
  length(rownames(df))
  dw <- sum(str_count(rownames(df),pattern ="DW"))
  ep <- sum(str_count(rownames(df),pattern ="EP"))
  sl <- sum(str_count(rownames(df),pattern ="SL"))
  
  var <- c(rep(1,dw),rep(2,ep),rep(3,sl))
  testlength <-length(rownames(df))==length(var)
  print(testlength)
  import_hc <-importance(df, var,show=NA)
  hc_ival <- indval(df, var)
  #return(hc_ival)
  #return(hc_ival$indval)
  print("Summary Indicators for defined groups")
  summary(hc_ival)  
    }

# print orination plots function 

#lcex= cex for the labels of points
#pcex= cex for ponits
#mcex= cex for maintitel
#acex= cex for axis
#lwd= cluster line witdt
# x/y relativ positions of "nmds stress level" on plots dependet on the axis values

print_hc_nmds <-function(df,cl,lcex=1,pcex=2,mcex=1,acex=1,lwd=1, air=1,main="name",x,y){
  #input check
  if (missing(cl)){
    stop("missing argument cl with no default")
  }
  
  nmds<-metaMDS(df)
  # nmds stress level
  print(nmds$stress)
  if(nmds$stress>0.2) {warning("Stress over 0.2 !")}
  if(nmds$stress>0.3) {warning("Stress over 0.3 ! Results are highly speculativ")}
  # cluster
  vdist <- vegdist(df, method = "bray", binary = FALSE)
  cluster <- hclust(vdist, method = "ward.D")
  cutclust <- cutree(cluster, k=cl)
  #plot with hc nmds
  sc<-scores(nmds)
  ordiplot(nmds,type="n",main=main, 
           cex.main=mcex,
           cex.axis=acex,
           mar=c(1,1,1,1))
  orditorp(nmds,display="sites",cex=lcex,air=air)
  text(x=x,y=y,cex=1,label=paste0("nmds stress level: ",round(nmds$stress,4)))
  points(sc[,1],sc[,2],cex=pcex,pch=20,col=cutclust)
  ordihull(nmds, cutclust, lty=2,lwd=lwd, col="blue")
}

