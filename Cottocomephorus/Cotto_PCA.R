#2b-rad data exploration
#Carl St. John
#5/18/2018

library(adegenet)
library(dartR)
library(plyr)
setwd("~/Documents/round3/")
tbrad <- read.PLINK("recal.cotto2.Miss90PolyHetMaf05.plink.raw", header = T)
as.matrix(tbrad)[1:10,1:10]

### Data Prep ###
#tbrad.pca <- glPca(tbrad, n.cores = 2, parallel = TRUE)
#scatter(tbrad.pca)
#mainly separates leocottus and cottocomephorus
#remove leocottus from analysis
indNames(tbrad)
#change names
#cotto.gl <- gl.recode.ind(tbrad, ind.recode = file.path(
#  "~/Aguilar Lab/Baikal Sculpins/2b_Rad/remove_c.csv")) #already recoded, apparently
irecode.gl <- gl.recode.ind(tbrad, ind.recode = "ind.csv", recalc = T)
recode.gl <- gl.recode.pop(irecode.gl, pop.recode = "pop.csv")

#Reporting stats
library(hierfstat)
threepop.gl <- gl.recode.pop(irecode.gl, pop.recode = "pop3.csv")
threepop.gi <- gl2gi(threepop.gl)
mat.fst <- pairwise.fst(threepop.gi, res.type = "matrix")
mat.fst

### PCA ###

cotto.pca <- glPca(recode.gl, n.cores = 2, parallel = TRUE)
scatter(cotto.pca) #There may be some pop structure in C. inermis

#ggplot of pca
library(ggplot2)
cotto.dat <- as.data.frame(cotto.pca$scores[,1:2])
IndSpLoc <- as.matrix(read.csv("IndSpLoc.csv", header = F))
for(i in 1:16){
  IndSpLoc[i,2] <- paste(IndSpLoc[i,2], "_17", sep = "")
}
cotto.dat["sp"] <- as.matrix(IndSpLoc[,2])
cotto.dat["loc"] <- as.matrix(IndSpLoc[,3])

Color <- c(adjustcolor("skyblue3",alpha.f=0.7),adjustcolor("slateblue3",alpha.f=0.7),adjustcolor("firebrick3",alpha.f=0.7),adjustcolor("lightcoral",alpha.f=0.7))
cotto.plot <- ggplot(cotto.dat,aes(PC1,PC2)) + 
  geom_text(aes(label=rownames(cotto.dat),col=loc),hjust=0,vjust=0) +
  geom_point(aes(col=loc,shape=sp),size=5,alpha=0.8) +
  labs(col="Location",shape="Species", x = paste("PC1", round(sqrt(cotto.pca$eig[1]),2)),y = paste("PC2", round(sqrt(cotto.pca$eig[2]),2))) + 
  theme_gray(base_size = 14) + 
  scale_color_manual(breaks=c("SelengaFan","SmallSea","SmallSeaChannel","UshkanIsland","NorthBasin","FrolikaBay"),
                     values = c("green3","darkgreen","lightcoral","royalblue","skyblue","limegreen")) +
  scale_shape_manual(breaks=c("Cgrewingkii","Cgrewingkii_17","Cinermis","Cinermis_17"),
                     values = c(19,1,17,2)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)
cotto.plot