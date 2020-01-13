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

# correlation test species richness and species total cover vs envrmt parameters

#load mainplot table
MP <- read.csv(file.path(envrmt$path_privot,"mainplots_occur.csv"),row.names = 1,header=T)
MP <-t(MP)

# add richniss and total cover
richness <-colSums(MP,na.rm = T)
MP <- rbind(MP,richness)

#add total cover from mainplots cover
MPc <- read.csv(file.path(envrmt$path_privot,"mainplots_cov.csv"),row.names = 1,header=T)
MPc <-t(MPc)
cover <- round(colSums(MPc,na.rm = T))
MP <- rbind(MP,cover)

rownames(MP)[rownames(MP) == "33"] <- "Richness"
rownames(MP)[rownames(MP) == "34"] <- "Total_Cover"


tail(MP)
#####

# load tab with envrmt parameters
list.files(envrmt$path_org)

dat <- read.csv(file.path(envrmt$path_org,"001_mainplot_data.csv"),header=T,sep="\t")
# clean up cols
dat <-dat[,-6:-9]
dat <-dat[,-9:-13]
class(dat[4,4])
dat<-t(dat)

# convert to df for binding
df_dat <-as.data.frame(dat)
row.names(df_dat) <- row.names(dat)
colnames(df_dat) <- dat[1,]
df_dat <- df_dat[-1,]

df_mp <-as.data.frame(MP)

#bind
df <-rbind(df_mp,df_dat)
df <-t(df)
df<-as.data.frame(df)

#subset envrmt parameters
hlay <-as.numeric(as.character(df$Herb_layer))
slay <-as.numeric(as.character(df$Shrub_layer))
tlay <-as.numeric(as.character(df$Tree_layer))

ttyp <-as.numeric(as.character(df$treetyp_num))
tcla <-as.numeric(as.character(df$treeclass_num))

#subset cover and richness
covt <-as.numeric(as.character(df$cover))
rich <-as.numeric(as.character(df$richness))

#########################################################################################
# perform cor.tests for richness
cor.test(rich,hlay)
cor.test(rich,slay)
cor.test(rich,tlay)
cor.test(rich,ttyp)
cor.test(rich,tcla)

#perform cor.test for total cover
cor.test(covt,hlay)
cor.test(covt,slay)
cor.test(covt,hlay)
cor.test(covt,ttyp)
cor.test(covt,tcla)

# perform cor.tests for richness
cor.test(rich,hlay, method = "spearman")
cor.test(rich,slay, method = "spearman")
cor.test(rich,tlay, method = "spearman")
cor.test(rich,ttyp, method = "spearman")
cor.test(rich,tcla, method = "spearman")

#perform cor.test for total cover
cor.test(covt,hlay, method = "spearman")
cor.test(covt,slay, method = "spearman")
cor.test(covt,hlay, method = "spearman")
cor.test(covt,ttyp, method = "spearman")
cor.test(covt,tcla, method = "spearman")


# just some corplotting (doenst shor p-value!!!)
df <- cbind(rich,covt,hlay,slay,tlay,ttyp,tcla)
cormat <- cor(df)
corrplot(cormat)

# result: either on spearman or pearson only scrublayer has a correlation