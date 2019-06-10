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
libs = c("link2GI","vegan","labdsv") 
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

# load testdataset
tdat <- read.table(file.path(envrmt$path_data,"testdata_1.txt"),header = T)
tdat

# calc ordination for differences an equality
NMDS <- metaMDS(tdat)

plot(NMDS, display="sites",type="p",main="NMDS",cex=2.0)
     plot(NMDS, type="t",main="NMDS",cex=1.5)

    
# cluster analysis
                    
vdist <- vegdist(tdat, method = "bray", binary = FALSE)
cl <- hclust(vdist, method = "ward.D")

plot(cl, hang = -1)
rect.hclust(cl,2, border ="green")
rect.hclust(cl,3, border="blue")
rect.hclust(cl,4, border="orange")
rect.hclust(cl,5, border="red")

# plot clusters with ordination

ordiplot(NMDS,type="n")
orditorp(NMDS,display="sites",cex=1, col=1)
ordihull(NMDS, ccl, lty=5,lwd=2 ,col="blue")

# lead species and importance

# set amount of clusters which are useful
count_clusters= 3
ccl <- cutree(cl, k=count_clusters)


const(tdat, ccl)
importance(tdat, ccl,show=NA)
indi <- indval(tdat,ccl)
summary(indi)



