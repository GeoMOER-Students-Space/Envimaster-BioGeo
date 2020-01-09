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

# preprocession
# script to preprocess the org datatables for further analysis

#load data
#list.files(envrmt$path_org)
org <- read.csv(file.path(envrmt$path_org,"003_Data_clean.csv"))
#head(org)

# new df with selected columns (desired informations)
main <- dplyr::select(org,Plot.ID,Subplot.ID,Tree_level,Coverage_bb,Moss_species,Tree_species,Department_ID)
rm(org)
# rename in english
names(main)<- c("ID","Sub","level","cover","spec","treetyp","dep_ID")

#translate Treetyp names
main <-trlateTree(main)

# reclass bb scale to numeric % values
main <-reclassBB(main)

# delete rows with missing spec name
sum(main$spec=="")
main <- main[-which(main$spec==""),]

head(main)

write.csv(main,file.path(envrmt$path_stage2,"main_clean.csv"))
