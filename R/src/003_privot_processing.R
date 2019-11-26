#############################################################################################
###--- Setup Environment -------------------------------------------------------------------#
                                  ###############################################           #
# require libs for setup          #EEEE n   n v       v rrrr    m     m   ttttt #           #                  
require(raster)                   #E    nn  n  v     v  r   r  m m   m m    t   #           #         
require(envimaR)                  #EE   n n n   v   v   rrrr   m m   m m    t   #           #                
require(link2GI)                  #E    n  nn    v v    r  r  m   m m   m   t   #           #             
                                  #EEEE n   n     v     r   r m    m    m   t   #           #
                                  ###############################################           #

# define needed libs and src folder                                                         #
libs = c("link2GI","reshape2","dplyr","vegan","cluster","labdsv","rgdal","stringr") 
pathdir = "R/src/"

#set root folder for uniPC or laptop                                                        #
root_folder = alternativeEnvi(root_folder = "~/edu/Envimaster-BioGeo",                      #
                              alt_env_id = "COMPUTERNAME",                                  #
                              alt_env_value = "PCRZP",                                      #
                              alt_env_root_folder = "F:/edu/Envimaster-BioGeo")             #
#source environment script                                                                  #
source(file.path(root_folder, paste0(pathdir,"000_envrmt_bio_v1.R")))                                                              
###---------------------------------------------------------------------------------------###
#############################################################################################

#load data
list.files(envrmt$path_org)
org <- read.csv(file.path(envrmt$path_org,"003_Data_clean.csv"))
head(org)

### 1. Preprocessing
# dataset contains Na and " " in cover and level
# empty cover for all specs should be "r", empty lvl should be deleted for epi

# new df with selected columns
main <- dplyr::select(org,Plot.ID,Subplot.ID,Tree_level,Coverage_bb,Moss_species,Tree_species,Department_ID)

unique(main$Department_ID)

# rename in english
names(main)<- c("ID","Sub","level","cover","spec","treetyp","dep_ID")

# optional rename treetyp for later merge with plot ID for epi
unique(main$treetyp)
main$treetyp <-as.character(main$treetyp)
#rename treetyp
main$treetyp[main$treetyp=="Buche"]     <-"FS_" #fargus sylvatica
main$treetyp[main$treetyp=="Eiche"]     <-"QP_" #Quercus petrea cf
main$treetyp[main$treetyp=="Fichte"]    <-"PA_" #Picea abies
main$treetyp[main$treetyp=="Hainbuche"] <-"CB_" #Carpinus betulus
main$treetyp[main$treetyp=="Birke"]     <-"BP_" #Betula pendula
main$treetyp[main$treetyp=="Laerche"]    <-"LD_" # Larix decidua
main$treetyp[main$treetyp=="Douglasie"] <-"PM_" #Pseudotsuga menziesii
main$treetyp[main$treetyp=="Eberesche"] <-"SA_" #Sorbus aucuparia
main$treetyp[main$treetyp=="Ahorn"]     <-"AS_" # Acer spec, unknown species

# set minimum cover for NA
main$cover <-as.character(main$cover)

main$cover[is.na(main$cover)] <- "None" # set "None" to NA cells
main$cover[main$cover==""] <- "None" #set "None" to " " cells
main$cover[main$cover=="None"] <- "r" # replace "None" with "r"

# reclass bb scale to numeric % values
# ggf 2nd value correction (eg 76-100 instead of 75)

main$cover[main$cover=="r"]<- 0.1 # far less 1
main$cover[main$cover=="+"]<- 0.5 # less 1
main$cover[main$cover==1]  <- 2.5 # less 5
main$cover[main$cover==2]  <- 15  #mean(5:25)
main$cover[main$cover==3]  <- 37.5#mean(25:50)
main$cover[main$cover==4]  <- 62.5#mean(50:75)
main$cover[main$cover==5]  <- 87.5#mean(75:100)

main$cover <-as.numeric(main$cover)
# delete rows with missing spec name
main <- main[-which(main$spec==""),]
#####################################################################################

# 2. prepare datasets for privot tables

# change name for epi treetyp in new df


main4ep <- main
main4sub <- main
main4ep$ID <- do.call(paste0, main[c("treetyp","ID")])
main4sub$ID <- do.call(paste0, main[c("Sub","ID")])
main <- main[,-6]
head(main)

main$ID <- do.call(paste0,main[c("dep_ID","ID")])
# make subtables for all substrates

SL <- subset(main,main[,2]%in% grep("SL", main[,2], value = TRUE))              
DW<- subset(main,main[,2]%in% grep("DW", main[,2], value = TRUE))  
EP  <- subset(main4ep,main4ep[,2]%in% grep("EP", main[,2], value = TRUE))  

head(SL)
# delet unneeded "level" and "sub" column for SL and DW
# rownames (for ordination) can be chnaged with the upper merge of columns
DW <- DW[,-2:-3]
SL <- SL[,-2:-3]

# clean up NA lvl in EP
EP <- EP[-which(is.na(EP$level)),]

# merge columns ID and Sub names to one to get acces to select Epi subplots in privot by name suffix
EP_lvl <- EP
EP_lvl$ID <- do.call(paste0, EP_lvl[c("ID","Sub")])
EP_lvl <-EP_lvl[,-2]
EP <- EP[,-2]
###########################################################################################

# 3. compute privot tables for Mainplots and aggregated Subplots


#use reshape with melt and dcast to get privot table
# use 1rst col for rownames and delet 1st col (due to factor class not supportetd by MVS)

SU_p <- dcast(main4sub,ID~spec,value.var="cover",fun.aggregate = mean)
rownames(SU_p)<-SU_p$ID
SU_p <-SU_p[,-1] 

DW_p <- dcast(DW,ID~spec,value.var="cover",fun.aggregate = sum)
rownames(DW_p)<-DW_p$ID
DW_p <-DW_p[,-1] 

SL_p <- dcast(SL,ID~spec,value.var="cover",fun.aggregate = sum)
rownames(SL_p)<-SL_p$ID
SL_p <-SL_p[,-1] 

EP_p <- dcast(EP,ID~spec,value.var="cover",fun.aggregate = mean)
rownames(EP_p)<-EP_p$ID
EP_p <-EP_p[,-1] 

MP_p <- dcast(main,ID~spec,value.var="cover",fun.aggregate = mean)
rownames(MP_p)<-MP_p$ID
MP_p <-MP_p[,-1] 

EP_lvl_p <- dcast(EP_lvl,ID~spec,value.var="cover",fun.aggregate = mean)

################################################################################################

# 4. compute privot tables for Epi by lvl

#!!!!!!!!!!!!!!!!!!!!!!!! EP01 must not be same tree, check this !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
EP_p1 <- subset(EP_lvl_p,EP_lvl_p$ID%in% grep("EP01", EP_lvl_p$ID, value = TRUE)) 
EP_p2 <- subset(EP_lvl_p,EP_lvl_p$ID%in% grep("EP02", EP_lvl_p$ID, value = TRUE)) 
EP_p3 <- subset(EP_lvl_p,EP_lvl_p$ID%in% grep("EP03", EP_lvl_p$ID, value = TRUE)) 
EP_p4 <- subset(EP_lvl_p,EP_lvl_p$ID%in% grep("EP04", EP_lvl_p$ID, value = TRUE)) 

# rownames 
rownames(EP_p1)<-EP_p1$ID
rownames(EP_p2)<-EP_p2$ID
rownames(EP_p3)<-EP_p3$ID
rownames(EP_p4)<-EP_p4$ID
EP_p1 <-EP_p1[,-1]
EP_p2 <-EP_p2[,-1]
EP_p3 <-EP_p3[,-1]
EP_p4 <-EP_p4[,-1]
# 5. clean all NaN in privot tables (SL and DW are clean due to sum calculations)
# reclass NaN with 0 and delet missing species

EP_p1[EP_p1=="NaN"] <- 0
EP_p1 <- EP_p1[,-which(colSums(EP_p1) == 0)]
EP_p2[EP_p2=="NaN"] <- 0
EP_p2 <- EP_p2[,-which(colSums(EP_p2) == 0)]
EP_p3[EP_p3=="NaN"] <- 0
EP_p3 <- EP_p3[,-which(colSums(EP_p3) == 0)]
EP_p4[EP_p4=="NaN"] <- 0
EP_p4 <- EP_p4[,-which(colSums(EP_p4) == 0)]

EP_p[EP_p=="NaN"] <- 0
MP_p[MP_p=="NaN"] <- 0


SU_p[SU_p=="NaN"] <- 0
################################################################################################

####################################################################################################
# covert BB scale to only appeariance

#copy org datasets
SL_c <- SL_p
DW_c <- DW_p
EP_c <- EP_p
MP_c <- MP_p
SU_c <- SU_p

# replace all values unequal 0 to 1, leaves only 1 for existing and 0 for missing
SL_c[SL_c>0] <-1
DW_c[DW_c>0] <-1
EP_c[EP_c>0] <-1
MP_c[MP_c>0] <-1
SU_c[SU_c>0] <-1


# all privot tables rdy for ordination

################################################################################################

# test Reaver on privots for frist look on equal plots
# source Function
source(file.path(envrmt$path_REAVER_hyperspace,"dev_plot_hyperspace.r"))

# Explore Hyperspace with cover values
Reaver_plot_hyperspace(SL_p,6)
Reaver_plot_hyperspace(DW_p,4)
Reaver_plot_hyperspace(EP_p,4)
Reaver_plot_hyperspace(MP_p,4)
Reaver_plot_hyperspace(SU_p,4)

# Explore Hyperspace with only appeariance
Reaver_plot_hyperspace(SL_c,6, indi = T)
Reaver_plot_hyperspace(DW_c,4, indi = T)
Reaver_plot_hyperspace(EP_c,4)
Reaver_plot_hyperspace(MP_c,6)
Reaver_plot_hyperspace(SU_c,3, indi = T)
dev.off()

boxplot(SL_p,las=3)
boxplot(DW_p,las=3)
boxplot(EP_p,las=3)
boxplot(MP_p,las=3)
boxplot(SU_p,las=3)

# delete hypnum
dw <-DW_p[,-10]
# witout hynum with only count
dw_c <- dw
dw_c[dw_c>0]<-1

class(dw[2,2])
class(dw_c[2,2])
Reaver_plot_hyperspace(DW_p,4)
Reaver_plot_hyperspace(DW_c,4)
Reaver_plot_hyperspace(dw,4) # without hypnum
Reaver_plot_hyperspace(dw_c,4) # withour hypnum (count only)
# epi by level
Reaver_plot_hyperspace(EP_p1,4)
Reaver_plot_hyperspace(EP_p2,4)
Reaver_plot_hyperspace(EP_p3,4)
Reaver_plot_hyperspace(EP_p4,4)
dev.off()
boxplot(SU_p,las=3)


# convert cover to count
dft[dft>0] <- 1

#delete specs
# save desied species name to variable
sp <- "name"
# delete col
dft[,-sp]

# write out privot table

#chnage rows and cols
t(df)
write.csv(SU_p,file.path(envrmt$path_REAVER_hyperspace,"privot.csv"))

# dev species plot appeeariences

# copy dataset for dev
df <- SU_p
df

require(mapview)

#define target row (anzahl row+1) saved to var
n <-nrow(df)+1

appear <- function(df){
  n <-nrow(df)+1
  for (i in 1:ncol(df)) {
    df[n,i] <- length(which(df[,i]>0))
  }
return(df)}

df
df2 <-appear(df)

# works lengh von welche in spalte >0
length(which(df[,2]>0))
df[,2]
###################################################################################################

# dev spec richness

# copy dataset for dev
df <- MP_p
df

richness <- function(df){
  c <- ncol(df)+1
  for (i in 1:nrow(df)) {
    df[i,c] <- length(which(df[i,]>0))
  }
  return(df)
}
df2 <-richness(df)
df3 <- appear(df2)
dev.off()

test <- EP_p[,-7]
boxplot(test,las=3)
dev.off()
boxplot(EP_p,las=3)


dfp <- MP_p
# prepare privot table for drag and drop work in excel
conv_num2bb <- function(dfp){
#  dfp[dfp==0] <- " "
dfp[dfp>=0.1 & dfp<0.5]  <-100
dfp[dfp>=0.5 & dfp<2.5]  <-500
dfp[dfp>=2.5 & dfp<15]  <- 1000
dfp[dfp>=15 & dfp<37.5]   <- 2000
dfp[dfp>=37.5 & dfp<62.5] <-3000
dfp[dfp>=62.5 & dfp<87.5] <-4000
dfp[dfp>=87.5 & dfp<100]<-5000
#dfp <-t(dfp)
return(dfp)
}

class(dfp[13,2])

test <- c(1,2,3,4,5,6,7)
test[test>=2 & test<=6] <- "x"
test
test[test>=0.5 & test<=8.5] <- "y"
str(test)
as.numeric(dfp)
dfp[dfp==87.5]
sl_p <-conv_num2bb(SL_p)
dw_p <-conv_num2bb(DW_p)
ep_p <-conv_num2bb(EP_p)
mp_p <-conv_num2bb(MP_p)
su_p <-conv_num2bb(SU_p)
write.csv(sl_p,file.path(envrmt$path_stage2,"SL_priv.csv"))
write.csv(dw_p,file.path(envrmt$path_stage2,"DW_priv.csv"))
write.csv(ep_p,file.path(envrmt$path_stage2,"EP_priv.csv"))
write.csv(mp_p,file.path(envrmt$path_stage2,"MP_priv.csv"))
write.csv(su_p,file.path(envrmt$path_stage2,"SU_priv.csv"))



main$cover[main$cover=="r"]<- 0.1 # far less 1
main$cover[main$cover=="+"]<- 0.5 # less 1
main$cover[main$cover==1]  <- 2.5 # less 5
main$cover[main$cover==2]  <- 15  #mean(5:25)
main$cover[main$cover==3]  <- 37.5#mean(25:50)
main$cover[main$cover==4]  <- 62.5#mean(50:75)
main$cover[main$cover==5]  <- 87.5#mean(75:100)

# dca darstellung umweltvariablen
# typische arten pro substart, zb nicht Hypnum weil überall

# untersuchung unetrschiede substart gegenüber waldtyp
# substarte targe untershciedliche zusammensetzung abgrenzung pber MVS

# tyoscihe arten bennen

# für epi nadel und laubbaum arten nach höhe sortieren und dann ttest
# normlaverteilung vorhertesten.
# man whitney you test verwenen.

# baum, art, max level