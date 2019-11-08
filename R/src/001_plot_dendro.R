#############################################################################################
###--- Setup Environment -------------------------------------------------------------------#
                                  ###############################################           #
# require libs for setup          #EEEE n   n v       v rrrr    m     m   ttttt #           #                  
require(raster)                   #E    nn  n  v     v  r   r  m m   m m    t   #           #         
require(envimaR)                  #EE   n n n   v   v   rrrr   m m   m m    t   #           #                
require(link2GI)                  #E    n  nn    v v    r  r  m   m m   m   t   #           #             
                                  #EEEE n   n     v     r   r m    m    m   t   #           #
                                  ###############################################           #
                                                                                            #
# define needed libs and src folder                                                         #
libs = c("link2GI") 
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

# developmnet script

# read csv
df <- read.csv(file.path(envrmt$path_dendro,"data_a1.csv"))
df[206,]
#delete unused rows
dfrow <- df[206:(nrow(df)-2),]
#get names from first col and delet first col
rownames(dfrow) <- dfrow[,1]
dfn <- dfrow[,-1]
# delete unused cols
dfcol <- dfn[,1:18]

#calc mean per row
dfcol$mean <- rowSums(dfcol,na.rm = T)/18
#plot with ts plot (rep() repeates a string)
ts.plot(ts(dfcol[1:18]),
        ts(dfcol$mean),
        col=c(rep("black", 18),"red"),
        ylab="mm",
        main="yearring width", lwd=2)

dfcol


