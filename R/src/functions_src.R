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
