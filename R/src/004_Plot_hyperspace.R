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

# perform Multivariant statistics
# Script to run several MVS approches on the different privot tables

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


# cleaned - all methods compared ##################################################################

# accumulated cover
#Reaver_plot_hyperspace(SL_clean,5)
#Reaver_plot_hyperspace(DW_clean,3)
#Reaver_plot_hyperspace(EP_clean,3)
Reaver_plot_hyperspace(MP_clean,3)
Reaver_plot_hyperspace(SU_clean,3)


# occuriance only
#Reaver_plot_hyperspace(sl_clean,3)
#Reaver_plot_hyperspace(dw_clean,3)
#Reaver_plot_hyperspace(ep_clean,3)
Reaver_plot_hyperspace(mp_clean,3)
Reaver_plot_hyperspace(su_clean,3)

# print output for paper for MP #####################################################################
par(mfrow=c(2,2))
par(mar=c(5,2,2,1))
#dev.off()
Reaver_plot_hyperspace(MP,5,display = "hc_nmds",main="Mainplots with Coverage")
Reaver_plot_hyperspace(mp,5,display = "hc_nmds",main="Mainplots with only Occurance")
Reaver_plot_hyperspace(MP_clean,5,display = "hc_nmds", main = "Mainplots with Coverage (cleaned)")
Reaver_plot_hyperspace(mp_clean,5,display = "hc_nmds", main = "Mainplots with only Occurance (cleaned)")


# print output for paper for SU
par(mfrow=c(2,2))
dev.off()
Reaver_plot_hyperspace(SU,3,display = "hc_nmds",main="Subplots with Coverage")
Reaver_plot_hyperspace(su,3,display = "hc_nmds",main="Subplots with only Occurance")
Reaver_plot_hyperspace(SU_clean,3,display = "hc_nmds", main = "Subplots with Coverage (cleaned)")
Reaver_plot_hyperspace(su_clean,3,display = "hc_nmds", main = "Subplots with only Occurance (cleaned)")

# indicator species test ################################################################################

# for the Substrate Tests the cluster number is = 1 DW 2 EP 3 SL (alpabetical order i assume)
Reaver_plot_hyperspace(MP,5,display = "hc_nmds",indi=T)
Reaver_plot_hyperspace(mp,5,display = "hc_nmds",indi=T)
Reaver_plot_hyperspace(MP_clean,5,display = "hc_nmds", indi=T)
Reaver_plot_hyperspace(mp_clean,5,display = "hc_nmds", indi=T)

Reaver_plot_hyperspace(SU,3,display = "hc_nmds",indi=T)
Reaver_plot_hyperspace(su,3,display = "hc_nmds",indi=T)
Reaver_plot_hyperspace(SU_clean,3,display = "hc_nmds", indi=T)
Reaver_plot_hyperspace(su_clean,3,display = "hc_nmds", indi=T)

# indicator test with defined clusters (withour cluster analysis on raw dataset, by DW,EP,SL)
ind_groups(SU)
ind_groups(su)
ind_groups(SU_clean)
ind_groups(su_clean)

