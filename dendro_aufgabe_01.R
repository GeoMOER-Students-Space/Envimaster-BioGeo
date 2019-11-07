df <- read.csv(file = "C:/Users/Anonymouse/Desktop/WS1920/Dendroökologie/Daten zu Ue01-dhl01.csv",
               skip = 205, nrows = 164)
head(df)

library(tseries)
ts.plot(df[,2])

m <- mean(df[,2],na.rm =T)
m 

sd <- sd(df[,2], na.rm =T)
sd

df2 <- df
df2$test <- mean(df[2,2:ncol(df)], na.rm = T)
colmean1 <-mean(df[2,],na.rm=T)
colmean1
############################
col1 <-df[,1] #spalte
col1
colnames(df)

row1 <- df[1,] #zeilen
row1
rownames(df)

df$s <-sum(df[,2:ncol(df)],na.rm=T)/164


df$col <- colSums(df,na.rm=T)
col2 <- col/164
row <- rowSums(df,na.rm=T)
row2 <- row/41          
          