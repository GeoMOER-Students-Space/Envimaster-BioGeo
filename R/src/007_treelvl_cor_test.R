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
libs = c("link2GI","vegan","cluster","labdsv","rgdal","stringr","corrplot") 
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

# treelevel cor
# script to perform cor tests for treelevel hypothesis

# load data
list.files(envrmt$path_org)
csv <- read.csv(file.path(envrmt$path_org,"004_tree_levels.csv"))

class(csv$TREE_ID)
as.numeric(csv$TREE_ID)

# subset max lvl
lvl <-csv$Tree_level

# subset parameters
ttyp <- csv$TREE_ID # tree species (see csv for details)
tcla <- csv$tree_type # angiosperm =1  gymnosperm = 2

# cor tests

cor.test(lvl,ttyp)
cor.test(lvl,tcla)

cor.test(lvl,ttyp, method = "spearman")
cor.test(lvl,tcla, method = "spearman")

# result
# there is a correlation of tree class and the max lvl of moos


# approches to plot results

df <-as.data.frame(csv)
df2 <- df[,8:10] 
df3 <- df[,9:10]


c <- cor(df2)
corrplot(c)

