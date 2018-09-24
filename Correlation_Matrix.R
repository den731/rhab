## Correlation matrix

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation

p.mat<-cor.mtest(MCSUMresponseAverage)

corrtable <- round(cor(MCSUMresponseAverage, use="complete.obs"),1)

# generate graphics
library(corrplot)
setEPS()
postscript(file="matrix.eps")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot.mixed(corrtable, 
               
               diag = "l",
               lower = "number",
               lower.col = "black",
               
               tl.pos = "lt",
               tl.col="black", 
               tl.srt=45, 
               tl.cex=0.6,
               order="AOE",
               number.font =1.25,
               sig.level=0.05,
               p.mat=p.mat,
               
               insig = "blank",
               addgrid.col = "grey",
               mar=c(0,0,1,0),
               number.cex= 0.371)
dev.off()




# Full table
corrtable <- round(cor(MCSUMresponseAverage, use="complete.obs"),2)
setEPS()
postscript(file="matrixfull.eps")
corrplot(corrtable, type="lower",  cl.pos = "n", 
         method = "number",
         
         tl.pos = "ld",
         tl.col="black", 
         tl.srt=45, 
         tl.cex=0.4,
         order="AOE",
         addgrid.col = NA,
         col = "black",
         mar=c(0,0,1,0),
         number.cex= 0.371)
dev.off()