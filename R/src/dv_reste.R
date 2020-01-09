# copy dataset for dev
df <- SU_p
df

require(mapview)

##############################################################################################

# substrate societys

unique(MP)

#define target row (anzahl row+1) saved to var
n <-nrow(df)+1

appear <- function(df){
  n <-nrow(df)+1
  for (i in 1:ncol(df)) {
    df[n,i] <- length(which(df[,i]>0))
  }
  return(df)}

df
df2 <-appear(df)

# works lengh von welche in spalte >0
length(which(df[,2]>0))
df[,2]
###################################################################################################

# dev spec richness

# copy dataset for dev
df <- MP_p
df

richness <- function(df){
  c <- ncol(df)+1
  for (i in 1:nrow(df)) {
    df[i,c] <- length(which(df[i,]>0))
  }
  return(df)
}
df2 <-richness(df)
df3 <- appear(df2)
dev.off()

test <- EP_p[,-7]
boxplot(test,las=3)
dev.off()
boxplot(EP_p,las=3)