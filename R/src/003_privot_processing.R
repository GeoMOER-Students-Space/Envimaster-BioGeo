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
org <- read.csv(file.path(envrmt$path_stage_1,"table_org.csv"))

### 1. Preprocessing
# dataset contains Na and " " in cover and level
# empty cover for all specs should be "r", empty lvl should be deleted for epi

# new df with selected columns
head(org)
main <- dplyr::select(org,Plot.ID,Subplot.ID,Level,Bedeckungsgrad,Art_überprüft,Baumart)

# rename in english
names(main)<- c("ID","Sub","level","cover","spec","treetyp")

# optional rename treetyp for later merge with plot ID for epi
unique(main$treetyp)
main$treetyp <-as.character(main$treetyp)
#rename treetyp
main$treetyp[main$treetyp=="Buche"]     <-"FS_" #fargus sylvatica
main$treetyp[main$treetyp=="Eiche"]     <-"QP_" #Quercus petrea cf
main$treetyp[main$treetyp=="Fichte"]    <-"PA_" #Picea abies
main$treetyp[main$treetyp=="Hainbuche"] <-"CB_" #Carpinus betulus
main$treetyp[main$treetyp=="Birke"]     <-"BP_" #Betula pendula
main$treetyp[main$treetyp=="Lärche"]    <-"LD_" # Larix decidua
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
main$cover[main$cover=="r"]<- 1 # far less 1
main$cover[main$cover=="+"]<- 10 # less 1
main$cover[main$cover==1]  <- 20 # less 5
main$cover[main$cover==2]  <- 30  #mean(25:50)
main$cover[main$cover==3]  <- 40#mean(25:50)
main$cover[main$cover==4]  <- 50#mean(50:75)
main$cover[main$cover==5]  <- 60#mean(75:100)
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

# make subtables for all substrates

SL <- subset(main,main[,2]%in% grep("SL", main[,2], value = TRUE))              
DW<- subset(main,main[,2]%in% grep("DW", main[,2], value = TRUE))  
EP  <- subset(main4ep,main4ep[,2]%in% grep("EP", main[,2], value = TRUE))  

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

# all privot tables rdy for ordination

################################################################################################

# test Reaver on privots for frist look on equal plots
source(file.path(envrmt$path_REAVER_hyperspace,"dev_plot_hyperspace.r"))
Reaver_plot_hyperspace(dft,4)



write.csv(SU_p,file.path(envrmt$path_REAVER_hyperspace,"privot.csv"))
# dev species plot appeeariences

# copy dataset for dev
df <- SU_p
df

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
# combine both functions and set last cell zu 0 ?

# dev anzahl arten

dft <- SU_p

dft[dft>0] <- 1
