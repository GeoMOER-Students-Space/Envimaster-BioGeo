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
libs = c("link2GI","vegan","cluster","labdsv","rgdal","stringr","ggplot2") 
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

df <- read.csv(file.path(envrmt$path_org,"004_tree_levels.csv"),header = T)

a <-as.numeric(df$Department_age)


# visualisierung der Daten
qqnorm(a)
qqline(a)

# t-test normalverteilung (wenn p-value > 0.05 dann liegt normalverteilung vor)
shapiro.test(a)

# t-test testet den Mittelwertuntewrschied (zweier) Datenreihen (vorraussetzung ist dass die daten normalverteilt sind)
t.test(df$Tree_level,a)


# correlation test
cor.test(df$Tree_level,a)
class(df$Department_age)
