#############################################################################################
###--- Setup Environment -----------------------------------------------------------------###
###############################################           #
# require libs for setup          #EEEE n   n v       v rrrr    m     m   ttttt #           #                  
require(raster)                   #E    nn  n  v     v  r   r  m m   m m    t   #           #         
require(envimaR)                  #EE   n n n   v   v   rrrr   m m   m m    t   #           #                
require(link2GI)                  #E    n  nn    v v    r  r  m   m m   m   t   #           #             
#EEEE n   n     v     r   r m    m    m   t   #           #
###############################################           #
#
# define needed libs and src folder                                                         #
libs = c("link2GI","vegan","cluster","labdsv","rgdal","stringr") 
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

# basic statistic
# script for the basic statistics for moos plots

# load data
list.files(envrmt$path_privot)

MP <- read.csv(file.path(envrmt$path_privot,"mainplots_occur.csv"),row.names = 1,header=T)
SU <- read.csv(file.path(envrmt$path_privot,"substrates_occur.csv"),row.names = 1,header=T)
SL <- read.csv(file.path(envrmt$path_privot,"soil_occur.csv"),row.names = 1,header=T)
DW <- read.csv(file.path(envrmt$path_privot,"deadwood_occur.csv"),row.names = 1,header=T)
EP <- read.csv(file.path(envrmt$path_privot,"epi_occur.csv"),row.names = 1,header=T)

#turn
MP <- t(MP)
SU <- t(SU)
SL <- t(SL)
DW <- t(DW)
EP <- t(EP)

# generate new df with species names
df <-data.frame(matrix(nrow=32,ncol=1))
row.names(df)<- row.names(MP)
df

# generate occuriance
oMP <- rowSums(MP)
oSL <- rowSums(SL)
oDW <- rowSums(DW)
oEP <- rowSums(EP)

# merge 
mMP <- merge(df,oMP,by=0,all=T)
mSL <- merge(df,oSL,by=0,all=T)
mDW <- merge(df,oDW,by=0,all=T)
mEP <- merge(df,oEP,by=0,all=T)

# bind to names
df <-cbind(df,mMP[3],mSL[,3],mDW[,3],mEP[,3])
df <-df[,-1]
df
colnames(df) <- c(paste0("MP /",as.character(ncol(MP))),
                  paste0("SL /",as.character(ncol(SL))),
                  paste0("DW /",as.character(ncol(DW))),
                  paste0("EP /",as.character(ncol(EP)))
)

#calc % occurience 
df$MPp <- round(df$`MP /18`/18,2)
df$SLp <- round(df$`SL /14`/14,2)
df$DWp <- round(df$`DW /18`/18,2)
df$EPp <- round(df$`EP /30`/30,2)
############################################################################

#same for cover (overwriting variables for privots)
MP <- read.csv(file.path(envrmt$path_privot,"mainplots_cov.csv"),row.names = 1,header=T)
SU <- read.csv(file.path(envrmt$path_privot,"substrates_cov.csv"),row.names = 1,header=T)
SL <- read.csv(file.path(envrmt$path_privot,"soil_cov.csv"),row.names = 1,header=T)
DW <- read.csv(file.path(envrmt$path_privot,"deadwood_cov.csv"),row.names = 1,header=T)
EP <- read.csv(file.path(envrmt$path_privot,"epi_cov.csv"),row.names = 1,header=T)

#turn
MP <- t(MP)
SU <- t(SU)
SL <- t(SL)
DW <- t(DW)
EP <- t(EP)

# generate new df with species names
dfcov <-data.frame(matrix(nrow=32,ncol=1))
row.names(dfcov)<- row.names(MP)


# generate occuriance
oMP <- rowSums(MP)
oSL <- rowSums(SL)
oDW <- rowSums(DW)
oEP <- rowSums(EP)

# merge 
mMP <- merge(dfcov,oMP,by=0,all=T)
mSL <- merge(dfcov,oSL,by=0,all=T)
mDW <- merge(dfcov,oDW,by=0,all=T)
mEP <- merge(dfcov,oEP,by=0,all=T)

# bind to names
dfcov <-cbind(dfcov,mMP[3],mSL[,3],mDW[,3],mEP[,3])
dfcov <-dfcov[,-1]
dfcov
colnames(dfcov) <- c("MP_cov",
                  "SL_cov",
                  "DW_cov",
                  "EP_cov"
)
head(dfcov)
head(df)

# merge informations
df2 <- cbind(df,dfcov)
head(df2)
