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
libs = c("link2GI","vegan","cluster","labdsv","rgdal","stringr","ggplot2","ggpubr") 
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

# SHAPIRO.TEST HOW TO
# Shapiro-test normalverteilung (wenn p-value > 0.05 dann liegt normalverteilung vor)

shapiro.test(a)

# SHAPIRO.TEST INTERPRETATION
# From the output, the p-value > 0.05 implying that the distribution of the data are 
# not significantly different from normal distribution. In other words, we can assume the normality.


#_____________________________________________________________________________________________________
# T.TEST HOW TO
# t-test testet den Mittelwertuntewrschied (zweier) Datenreihen (vorraussetzung ist dass 
# die daten normalverteilt sind)

t.test(df$Tree_level,a)

# T-TEST INTERPRETATION
# The p-value is a number between 0 and 1 and interpreted in the following way: A small 
# p-value (typically ??? 0.05) indicates strong evidence against the null hypothesis, 
# so you reject the null hypothesis


#___________________________________________________________________________________________________
# COR.TEST HOW TO
# t is the t-test statistic value (t = -9.559),
# df is the degrees of freedom (df= 30),
# p-value is the significance level of the t-test (p-value = 1.29410^{-10}).
# conf.int is the confidence interval of the correlation coefficient at 95% (conf.int = [-0.9338, -0.7441]);
# sample estimates is the correlation coefficient (Cor.coeff = -0.87).

cor.test(df$Tree_level,a)

# COR.TEST INTERPRETATION
# The p-value of the test is 1.29410^{-10}, which is less than the significance level alpha = 0.05. 
# We can conclude that wt and mpg are significantly correlated with a correlation coefficient 
# of -0.87 and p-value of 1.29410^{-10} .
