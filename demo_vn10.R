######################################################################
# Reproducibility Analysis
# For illustration, Kirby21 ventricular RAVENS images were thresholded 
# at sum > 15. In the main paper, sum > 1 was used. 
# Maskfile (vn10.mask) is provided.
######################################################################

## Call the multicore package. If multicore is not installed, set ncores = 1 
## when computing the CI and the Null distribution (as shown in the example below). 
#require(multicore)


source("I2C2_inference.R")

# load maskfile
load('mask10.RData')

## plot a few slices of the mask 
par( mfrow = c(2, 4), mar=c(1,1,1,1) )
for(j in 1:8) { 
	image( mask10[ , ,75 + 3 * j] ,axes=FALSE)
}

# load the dataset
load('vn10.RData')
ls()

# 21 subjects, xI = 21
# 2 visits per subject, xJ = 2
# ID vector: xid = rep( 1:21, each = 2 )
# Vector of visits: xvisit = rep( 1:2, (21) )
# vn: Thresholded Ventricular RAVENS Images

#### Compute I2C2 while obtaining the computation time
system.time( vn.lambda <- I2C2(vn, I = xI, J = xJ, id = xid, visit = xvisit, demean = TRUE) )
vn.lambda$lambda

#### Computing the 95% CI of I2C2
system.time( vn.ci <- I2C2.mcCI( vn, I = xI, J = xJ, id = xid, visit = xvisit, R = 100, rseed = 1, ncores = 1, demean = TRUE, ci = 0.95 ) )
x11()
hist( unlist(vn.ci) )

#### Compute the Null Distribution of I2C2
#### For input, demeanded data were used.
system.time( vn.NullDist <- I2C2.mcNulldist( vn.lambda$demean_y, I = xI, J = xJ, id = xid, visit = xvisit, R = 100, rseed = 1, ncores = 1, demean = FALSE ) )  
x11()
hist( unlist(vn.NullDist), main = 'NullDistribution' )

#### Draw the beanplot
library(beanplot)
beanplot( data.frame(VN = vn.NullDist), border = 8, ylim = c(-0.5,1), cex.main = 2, cex.axis = 1.5, main = "I2C2-Ventricular RAVENS", ll = 0.001, col = c(8, 8, "#B2DF8A") )
lines( rep(1, 2), vn.ci$CI, col = 1, lwd = 3, lty = 1)
lines( c(0.8, 1.2), rep(vn.ci$CI[1], 2), col = 1, lty = 1, lwd = 3)
lines( c(0.8, 1.2), rep(vn.ci$CI[2], 2), col = 1, lty = 1, lwd = 3)
lines(c(0.9, 1.1), rep(vn.lambda$lambda,2), col = 2, lwd = 4, lty = 1)
legend('topright',c("I2C2","95% CI","Null"),lwd=3,cex=1,col=c(2,1,8))

