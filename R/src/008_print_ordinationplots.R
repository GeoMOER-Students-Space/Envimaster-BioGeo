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

# Print outout images for paper
# Script to test several grafic paramters for Ordination plots

# source Function
source(file.path(envrmt$path_REAVER_hyperspace,"dev_plot_hyperspace.r"))
source(file.path(envrmt$path_src,"functions_src.R"))

#load data
SL <-read.csv(file.path(envrmt$path_privot,"soil_cov.csv"),row.names = 1)
DW <-read.csv(file.path(envrmt$path_privot,"deadwood_cov.csv"),row.names = 1)
EP <-read.csv(file.path(envrmt$path_privot,"epi_cov.csv"),row.names = 1)
MP <-read.csv(file.path(envrmt$path_privot,"mainplots_cov.csv"),row.names = 1)
SU <-read.csv(file.path(envrmt$path_privot,"substrates_cov.csv"),row.names = 1)

sl <-read.csv(file.path(envrmt$path_privot,"soil_occur.csv"),row.names = 1)
dw <-read.csv(file.path(envrmt$path_privot,"deadwood_occur.csv"),row.names = 1)
ep <-read.csv(file.path(envrmt$path_privot,"epi_occur.csv"),row.names = 1)
mp <-read.csv(file.path(envrmt$path_privot,"mainplots_occur.csv"),row.names = 1)
su <-read.csv(file.path(envrmt$path_privot,"substrates_occur.csv"),row.names = 1)

# plot Reaver Hyperspace

# for interpretation of nmds stress level:
# over 0.2 caution, over 0.3 highly suspect

# first overlook - all methods compared

# accumulated cover
#Reaver_plot_hyperspace(SL,5)
#Reaver_plot_hyperspace(DW,3)
#Reaver_plot_hyperspace(EP,3)
Reaver_plot_hyperspace(MP,3)
Reaver_plot_hyperspace(SU,3)


# occuriance only
#Reaver_plot_hyperspace(sl,3)
#Reaver_plot_hyperspace(dw,3)
#Reaver_plot_hyperspace(ep,3)
Reaver_plot_hyperspace(mp,3)
Reaver_plot_hyperspace(su,3)

# clean up dataframes ####################################################################################

### modifie tables

# detect dominant species
boxplot(SL,las=3) # none
boxplot(DW,las=3) #hyp cup, bra rut
boxplot(EP,las=3) #hyp cup
boxplot(MP,las=3) #hyp cup, bra rut
boxplot(SU,las=3) #hyp cup
# not necceserie for only occourence

# plot boxplots (mar: defines the distance, cex.axis the ces of the labels)
par(mar=c(11,2,2,1))
boxplot(MP,las=3,cex.axis=0.9)
boxplot(SU,las=3,cex.axis=0.9)
dev.off()

colnames(DW)
DW_clean <- delete.species(DW,"hypnum.cupressiforme")
DW_clean <- delete.species(DW_clean,"brachythecium.rutabulum")

EP_clean <- delete.species(EP,"hypnum.cupressiforme")

MP_clean <- delete.species(MP,"hypnum.cupressiforme")
MP_clean <- delete.species(MP_clean,"brachythecium.rutabulum")

mp_clean <- delete.species(mp,"hypnum.cupressiforme")
mp_clean <- delete.species(mp_clean,"brachythecium.rutabulum")
SU_clean <- delete.species(SU,"hypnum.cupressiforme")
su_clean <- delete.species(su,"hypnum.cupressiforme")

########################## all data preprocessed ###################################################

####################################################################################################

# print output for method comparison all datasets

#set graphic parameters
#par(mfrow=c(2,2))
par(mar=c(2,2,2,2))

# accumulated cover
Reaver_plot_hyperspace(SL,5)
Reaver_plot_hyperspace(DW,5)
Reaver_plot_hyperspace(EP,5)
Reaver_plot_hyperspace(MP,5)
Reaver_plot_hyperspace(SU,5)


# occuriance only
#Reaver_plot_hyperspace(sl_clean,3)
#Reaver_plot_hyperspace(dw_clean,3)
#Reaver_plot_hyperspace(ep_clean,3)
Reaver_plot_hyperspace(mp_clean,3)
Reaver_plot_hyperspace(su_clean,3)

# print output for hc_nmds result for MP and SL

# IMPROTANT: the output quality depends on the size adjusted. by default the size of the "current seen" is used
# Therefore the desired size muste be teste before to get optimal results. 
# (eg if a 2x2 plot looks good, used the same size for 1x1 plot looks bad)

#set graphic parameters
dev.off()
par(mfrow=c(2,2))
par(mar=c(2,2,2,2))
# MP
print_hc_nmds(MP,cl=5,main="Mainplots Coverage",lcex=1.2,pcex=2,mcex=1,acex=1,lwd=1.5,x=0.9,y=-1.2,air=0.01)
print_hc_nmds(mp,cl=5,main="Mainplots Occurance",lcex=1.2,pcex=2,mcex=1,acex=1,lwd=1.5,x=0.7,y=-1.1,air=0.01)
print_hc_nmds(MP_clean,cl=5,main="Mainplots Coverage (clean)",lcex=1.2,pcex=2,mcex=1,acex=1,lwd=1.5,x=0.7,y=-1.1,air=0.01)
print_hc_nmds(mp_clean,cl=5,main="Mainplots Coverage (clean)",lcex=1.2,pcex=2,mcex=1,acex=1,lwd=1.5,x=0.7,y=-0.9,air=0.01)

#set graphic parameters
dev.off()
par(mfrow=c(2,2))
par(mar=c(2,2,2,2))
# SU
print_hc_nmds(SU,cl=3,main="Subplots Coverage",lcex=1.2,pcex=2,mcex=1,acex=1,lwd=1.5,x=0.5,y=-2.3,air=0.01)
print_hc_nmds(su,cl=3,main="Subplots Occurance",lcex=1.2,pcex=2,mcex=1,acex=1,lwd=1.5,x=0.7,y=-1.9,air=0.01)
print_hc_nmds(SU_clean,cl=3,main="Subplots Coverage (clean)",lcex=1.2,pcex=2,mcex=1,acex=1,lwd=1.5,x=2,y=-3,air=0.01)
print_hc_nmds(su_clean,cl=3,main="Subplots Coverage (clean)",lcex=1.2,pcex=2,mcex=1,acex=1,lwd=1.5,x=2,y=-2.9,air=0.01)

# output single images
dev.off()
par(mar=c(2,2,2,2))

#MP
print_hc_nmds(MP,cl=5,main="Mainplots Coverage",lcex=1.2,pcex=2,mcex=1,acex=1,lwd=1.5,x=0.9,y=-1.2,air=0.01)
print_hc_nmds(mp,cl=5,main="Mainplots Occurance",lcex=1.2,pcex=2,mcex=1,acex=1,lwd=1.5,x=0.7,y=-1.1,air=0.01)
print_hc_nmds(MP_clean,cl=5,main="Mainplots Coverage (clean)",lcex=1.2,pcex=2,mcex=1,acex=1,lwd=1.5,x=0.7,y=-1.1,air=0.01)
print_hc_nmds(mp_clean,cl=5,main="Mainplots Coverage (clean)",lcex=1.2,pcex=2,mcex=1,acex=1,lwd=1.5,x=0.7,y=-0.9,air=0.01)

