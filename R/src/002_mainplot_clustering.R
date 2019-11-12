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

# Script to perform cluster analysis on vegeation plot survey

#source functions
source(file.path(envrmt$path_REAVER_hyperspace,"dev_plot_hyperspace.R"))

# load testdata

df <- read.csv(file.path(envrmt$path_org,"plotcsv_ohneBG.csv"),header = T)
df <- read.csv(file.path(envrmt$path_org,"plotcsv.csv"),header = T)
df

###long to wide
dfwide <-reshape(df, direction = "wide", idvar = "Plot.ID", timevar = "Art")

### replace NA to 0
dfwide[is.na(dfwide)] <- 0

rownames(dfwide) <- dfwide$Plot.ID

##
#normalverteilt - t-test
#nicht normalverteilt - smirnoff
#gruppen -anova()
#metrische daten - regression

#1. arten (gesamtliste) (allgemein descriptiv)
#2. gesellschaft nach waldtyp
#3. Artspezifisch (waldtyp substrat)
###exclude first row
dfw <-df[,-1]
###############################################################################################

#boxplot data
boxplot(dfwide)

# check hc cluster
par(mfrow=c(1,1))
vdist <- vegdist(dfw, method = "bray", binary = FALSE)
cluster <- hclust(vdist, method = "ward.D")
plot(cluster, hang = -1)

rect.hclust(cluster,2, border ="green")
rect.hclust(cluster,3, border ="blue")
rect.hclust(cluster,4, border ="orange")
rect.hclust(cluster,5, border ="red")
rect.hclust(cluster,6, border ="violet")
rect.hclust(cluster,7, border ="brown")

# set desired clusters
test <-Reaver_plot_hyperspace(dfw,cl=4,indi=T)

