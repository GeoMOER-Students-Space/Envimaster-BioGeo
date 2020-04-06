#lcex= cex for the labels of points
#pcex= cex for ponits
#mcex= cex for maintitel
#acex= cex for axis
  #lwd= cluster line witdt

print_hc_nmds <-function(df,cl,lcex=1,pcex=2,mcex=1,acex=1,lwd=1, air=1,main="name"){
  #input check
  if (missing(cl)){
    stop("missing argument cl with no default")
  }

nmds<-metaMDS(df)
  # nmds stress level
print(nmds$stress)
if(nmds$stress>0.2) {warning("Stress over 0.2 !")}
if(nmds$stress>0.3) {warning("Stress over 0.3 ! Results are highly speculativ")}
# cluster
vdist <- vegdist(df, method = "bray", binary = FALSE)
cluster <- hclust(vdist, method = "ward.D")
cutclust <- cutree(cluster, k=cl)
#plot with hc nmds
sc<-scores(nmds)
ordiplot(nmds,type="n",main=main, 
         cex.main=mcex,
         cex.axis=acex,
         mar=c(1,1,1,1))
orditorp(nmds,display="sites",cex=lcex)
text(x=1,y=-1.2,cex=1,label=paste0("nmds stress level: ",round(nmds$stress,4)))
points(sc[,1],sc[,2],cex=pcex,pch=20,col=cutclust)
ordihull(nmds, cutclust, lty=2,lwd=lwd, col="blue")
}
# end of function


# test function
print_hc_nmds(MP,cl=5,main="Testing",
              lcex=1.2,
              pcex=2,
              mcex=1,
              acex=1,
              lwd=1.5)


par(mar=c(2,2,2,2))
print_hc_nmds(MP,cl=5,main="Testing",
              lcex=1,
              pcex=2,
              mcex=1,
              acex=1,
              lwd=1.5)# aussenabstand
dev.off()

####################################

# single plot
par(mar=c(2,2,2,2))
print_hc_nmds(MP,cl=5,main="Testing",
              lcex=1,
              pcex=2,
              mcex=1,
              acex=1,
              lwd=1.5)# aussenabstand

# multiplot
par(mfrow=c(2,2))
par(mar=c(2,2,2,2))

print_hc_nmds(MP,cl=5,main="Testing",
              lcex=1,
              pcex=2,
              mcex=1,
              acex=1,
              lwd=1.5)

print_hc_nmds(MP,cl=5,main="Testing",
              lcex=1,
              pcex=2,
              mcex=1,
              acex=1,
              lwd=1.5)

print_hc_nmds(MP,cl=5,main="Testing",
              lcex=1,
              pcex=2,
              mcex=1,
              acex=1,
              lwd=1.5)

print_hc_nmds(MP,cl=5,main="Testing",
              lcex=1,
              pcex=2,
              mcex=1,
              acex=1,
              lwd=1.5)
