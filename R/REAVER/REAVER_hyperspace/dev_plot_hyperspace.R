#' Mandatory: Reaver Hyperspace
#'
#' @description Optional: performs ordination and cluster analysis to get information from a n-dimensional Hyperspace
#' prints nmds and dca ordiantions with Hierarchical Clustering and K-Means Clustering
#' @name Mandatory Reaver 
#' @export Mandatory Reaver

#' @param Mandatory if function: df - a data.frame with sites in rows and parameters in columns with numeric values.
#' @param Mandatory if function: cl - number of desired cluster, default = TRUE
#' @param Mandatory if function: indi - if TRUE the indicator parameters are printed, default = TRUE
#' @param Mandatory if function: re - if TRUE the clusters for hc and km are returned, default = FALSE

#note: v1.2 returning improved cluster quality parameters
# stats for HC only !!!

# add stress level for nmds
df <- su_c

Reaver_plot_hyperspace <-function(df,cl,indi=FALSE,re=FALSE){
  if (missing(cl)){
    stop("missing argument cl with no default")
  }
  cat(" ",sep = "\n")
  cat("### Reaver starts to reduce the ",nrow(df),"-dimensional Hyperspace ###")
  cat(" ",sep = "\n")
  #ordinations
  #dca
  dca<-decorana(df)
  #nmds
  nmds<-metaMDS(df)
  
  print(nmds$stress)
  print(nmds$grstress)
 
  #clusters#########################################################################################
  #bray ward
  vdist <- vegdist(df, method = "bray", binary = FALSE)
  cluster <- hclust(vdist, method = "ward.D")
  cutclust <- cutree(cluster, k=cl)
  #kmeans clustering
  km_cl <- kmeans(df,centers=cl,nstart=20)
  #plot
  par(mfrow=c(2,2))
  
  #plot with hc nmds
  sc<-scores(nmds)
  ordiplot(nmds,type="n",main="hc_nmds")
  orditorp(nmds,display="sites",cex=1,air=0.01)
  points(sc[,1],sc[,2],cex=2,pch=20,col=cutclust)
  ordihull(nmds, cutclust, lty=2, col="blue")
  
  #plot with km nmds 
  ordiplot(nmds,type="n",main="km_nmds")
  orditorp(nmds,display="sites",cex=1,air=0.01)
  ordihull(nmds, km_cl$cluster, lty=3, col="grey60",lwd=2)
  points(sc[,1],sc[,2],cex=2,pch=20,col=km_cl$cluster)

  #plot with hc dca points
  scd<-scores(dca)
  plot(dca,display="sites",type="n", main="hc_dca")
  orditorp(dca,display="sites",cex=1,air=0.01)
  points(scd[,1],scd[,2],cex=2,pch=20,col=cutclust)
  ordihull(dca, cutclust, lty=2, col="blue")
  
  #plot with km dca points
  plot(dca,display="sites",type="n", main="km_dca")
  orditorp(dca,display="sites",cex=1,air=0.01)
  points(scd[,1],scd[,2],cex=2,pch=20,col=km_cl$cluster)
  ordihull(dca, km_cl$cluster, lty=3, col="grey60",lwd=2)
  
 # #indicator for hc
  if (indi==TRUE){
  const_hc <-const(df, cutclust)
  import_hc <-importance(df, cutclust,show=NA)
  hc_ival <- indval(df, cutclust)
  summary(hc_ival)  
  
  const_km <-const(df, km_cl$cluster)
  import_km <-importance(df,km_cl$cluster,show=NA)
  km_ival <- indval(df, km_cl$cluster) # summarys indicator
  summary(km_ival)
  }
  ####################################################################
  if (re==TRUE){
    km <- as.data.frame(km_cl$cluster)
    hc <- as.data.frame(cutclust)
    ls <-list(hc,km)
    names(ls) <-c("hc","km")
    return(ls)
  }
}#end of fucntion
  

#'@examples
#'\dontrun{

#'}

