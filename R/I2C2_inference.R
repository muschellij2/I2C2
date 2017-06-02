###############################################################################################
###   This file contains R codes to make inference for the estimated I2C2 
###   I2C2.mcCI provides the confidence interval by bootstrapping subjects
###   I2C2.mcCI call I2C2.rsample to conduct the bootstrap
###   I2C2.NullDist conducts permutation by calling I2C2.rpermute to obtain 
###   the null distribution of I2C2
###         2012/12/1 By Haochang, Seonjoo and Ani   
###         revised by Haochang 2013/6/5                        
################################################################################################


cat("Call I2C2 function.\n")
source('trace_only_unba.R')

I2C2.mcCI <- function(y, I=NULL, J=NULL, id = NULL, visit = NULL,  p = NULL,
		              twoway = TRUE, demean = TRUE, T = NULL, symmetric = FALSE, trun = FALSE, 
                      rseed = 1234, R = 100, ncores = 4, ci = 0.95)
{

# I2C2.mcCI                    package:????                    R Documentation
#
# Compute the Convidence Interval
#
# Description:
#
#     ‘I2C2.mcCI’ is used for computing the confidence interval of I2C2 via multicore computing.
#
#Usage:
#
#	I2C2.mcCI(y, I, J, id = NULL, visit = NULL, p = NULL, 
#		      twoway = TRUE, demean = TRUE, T = NULL, symmetric = FALSE, trun = FALSE, #trace calculation parameters
#    		  rseed = 1234, R = 100, ncores = 4, ci = 0.95, # Bootstrap Arguments
#              ...)
#
#Arguments:
# # y: Dataset (Y11, Y12, ... , YnJn)' => (IJ)-by-p matrix for balanced case, EX) (Y11, Y12, Y21, Y22, ... , YI1 , YI2)
# # I: Number of subjects
# # J: Number of repetitions
# # id: Vector of IDs, EX) c(1, 1, 2, 2, 3, 3, 4, 4, ... , I, I)
# # visit: Vector of visits, EX) (1, 2, 1, 2, 1, 2, ... , 1, 2)
# # p: dimension of oberved vectors Yij, EX) Number of voxels
# # twoway, demean, T symmetric, trun: trace calcuation parameters. See 'I2C2' 
# # rseed: Seed number
# # R: The bootstrap repetition size
# # ncores: Number of Cores
# # ci: 100*ci% The level of the Confidence Interval
#
#Author(s): 
#
#    Haochang Shou, Ani Eloyan, Seonjoo Lee, Vadim Zipunnikov, Mary Beth Nebel, 
#     Brian Ca?o, Martin Lindquist, Ciprian M. Crainiceanu     
#
#References:
#
#     Haochang Shou, Ani Eloyan, Seonjoo Lee, Vadim Zipunnikov, Mary Beth Nebel, 
#     Brian Ca?o, Martin Lindquist, Ciprian M. Crainiceanu  (2012) The image intra 
#     class correlation coe?cient for replication studies.
#
#See Also:
#
#     'I2C2', 'I2C2.NullDist'
#
#Examples:
#
#     require(multicore)
#
##     1. Either (id,visit) or (I,J) is needed for identifying clusters. 
##         If both are missing, return an error message "not enough information";
##         if only (I,J) is provided, the function generates (id,visit);
##         if only ((id,visit) is provided, the function generates (I,J);
##      Note:  currently the function only deals with balanced data, i.e, each
##             subject is measured at the same J visits.
## 
##     2. If the number of grid points p is missing, the function generates it.



############
   if(  ( is.null(id) | is.null(visit) ) && ( is.null(I) | is.null(J) )  ) {
     	    stop("Not enough information! Please provide (id, visit) or (I,J) !")
     	}

##	set up the number of multicores

	options(cores = ncores) 
 	print( paste("Number of Cores Specficed:", getOption('cores')) )

##	Set a random seed

 	set.seed(rseed) 
 	
	if( ncores == 1 ) {
	lambda <- lapply( 1:R,
	                  myfunc <- function(s){
	                  	                   l = I2C2.rsample(s + rseed, y = y, I = I, J = J, id = id, visit = visit, p = p,	                  	                                                         twoway = twoway, demean = demean,
	                  	                                     T = T, symmetric = symmetric, trun = trun
	                  	                                     );
	                  	                   return(l)
	                  	                   }
	                 )
	}
	else {
	lambda <- mclapply( 1:R,
	                    myfunc <- function(s){
	                    	                  l = I2C2.rsample(s + rseed, y = y, I = I, J = J, id = id, visit = visit, p = p,
	                    	                                    twoway = twoway, demean = demean,
	                    	                                    T = T, symmetric = symmetric, trun = trun
	                    	                                    );
	                    	                  return(l)
	                    	                  }
	                  )
    }
	
    print( as.vector( unlist(lambda) ) )
 	result <- new.env()
 	result$lambda <- as.vector( unlist(lambda) )
 	result$CI <- quantile( result$lambda, c( (1 - ci)/2,ci + (1 - ci)/2) )
 	result = as.list(result)
 	return(result)

}

I2C2.rsample <- function(s, y, I = NULL, J = NULL, id = NULL, visit = NULL, p = NULL,
		                 twoway = TRUE, demean = TRUE, T = NULL, symmetric = FALSE, trun = FALSE)
{

# I2C2.rsample                    package:????                    R Documentation
#
# Compute I2C2 for a set of Randomly Sampled Subjects (with replacement)
#
# Description:
#
#     ‘I2C2.rsample’ is used for computing I2C2 of permuted data.
#
#Usage:
#
#	I2C2.rsample(s, y, I, J, id = NULL, visit = NULL, p = NULL,
#	        	 twoway = TRUE, demean = TRUE, T = NULL, symmetric = FALSE, trun = FALSE #trace calculation parameters
#                ...)
#     
#Arguments:
# # s: Random seed, default = 1
# # y: Dataset (Y11, Y12, ... , YnJn)' => (IJ)-by-p matrix for balanced case, EX) (Y11, Y12, Y21, Y22, ... , YI1, YI2)
# # I: Number of subjects
# # J: Number of repetitions
# # id: Vector of IDs, EX) c(1, 1, 2, 2, 3, 3, 4, 4, ... , I, I)
# # visit: Vector of visits, EX) (1, 2, 1, 2, 1, 2, ... , 1, 2)
# # p: dimension of oberved vectors Yij, EX) Number of voxels
# # twoway, demean, T, symmetric, trun: trace calcuation parameters. See 'I2C2'

    	if(  ( is.null(id) | is.null(visit) ) && ( is.null(I) | is.null(J) ) ) {
     	    stop("Not enough information! Please provide (id, visit) or (I,J) !")
     	}
     	
    	if( !( is.null(I) | is.null(J) ) && ( is.null(id) | is.null(visit) ) ) {
        	id <- rep(1:I, each = J)
        	visit <- rep(1:J, I)
        }
    	
    	if( is.null(p) ) {
    		p <- dim(y)[2]
    	}
    	
    	 if( is.null(T) ) {
	     	   T = visit
	       }
	set.seed(s)
	newI=sample(unique(id),length(unique(id)),replace=TRUE)
	newid<-newX<-newT <-newvisit<-c()
	
	for (j in 1:length(unique(id))){
		tmpindx=which(id==newI[j])
		newid=c(newid, rep(j,length(tmpindx)))
		newX=rbind(newX,y[tmpindx,])
		newT=c(newT,T[tmpindx])
		newvisit=c(newvisit,visit[tmpindx])
	}
	
	lambda=I2C2(newX, id=newid, visit=newvisit, J=NULL,I=NULL, p=p, T=newT, twoway=twoway,demean=demean,symmetric=symmetric,trun=trun)$lambda  
	
	return(lambda)
}


I2C2.mcNulldist <- function(y, I = NULL, J = NULL, id = NULL, visit = NULL, p = NULL, 
			                twoway = TRUE,  demean = FALSE, T = NULL, symmetric = FALSE, trun = FALSE, 
  			                rseed = 1234, R = 500, ncores = 4)
{
	
# I2C2.mcNulldist                    package:????                    R Documentation
#
# Compute Null Distribution 
#
# Description:
#
#     ‘I2C2.mcNulldist’ is used for computing the null distribution of I2C2 via permutation and multicore computing.
#
#Usage:
#
#	I2C2.mcNulldist(y, I, J, id = NULL, visit = NULL, p = NULL, 
#		            twoway = TRUE, demean = TRUE, T = NULL, symmetric = FALSE, trun = FALSE, #trace calculation parameters
#                   rseed = 1234, R = 500, ncores = 4, # Bootstrap Arguments          
#                   ...)
#		
#Arguments:
# # y: Dataset (Y11, Y12, ... , YnJn)' => (IJ)-by-p matrix for balanced case, EX) (Y11, Y12, Y21, Y22, ... , YI1, YI2)
# # I: Number of subjects
# # J: Number of repetitions
# # id: Vector of IDs, EX) c(1, 1, 2, 2, 3, 3, 4, 4, ... , I, I)
# # visit: Vector of visits, EX) (1, 2, 1, 2, 1, 2, ... , 1, 2)
# # p: dimension of oberved vectors Yij, EX) Number of voxels
# # twoway, demean, T, symmetric, trun: trace calcuation parameters. See 'I2C2'
# # rseed: Seed number
# # R: The bootstrap repetition size
# # ncores: Number of Cores
#
#Author(s): 
#
#    Haochang Shou, Ani Eloyan, Seonjoo Lee, Vadim Zipunnikov, Mary Beth Nebel, 
#     Brian Ca?o, Martin Lindquist, Ciprian M. Crainiceanu   
#
#References:
#
#     Haochang Shou, Ani Eloyan, Seonjoo Lee, Vadim Zipunnikov, Mary Beth Nebel, 
#     Brian Ca?o, Martin Lindquist, Ciprian M. Crainiceanu  (2012) The image intra 
#     class correlation coe?cient for replication studies.
#
#See Also:
#
#     'I2C2', 'I2C2.mcCI'
#
   if(  ( is.null(id) | is.null(visit) ) && ( is.null(I) | is.null(J) )  ) {
     	    stop("Not enough information! Please provide (id, visit) or (I,J) !")
     	}
		
	options(cores = ncores)
	print( paste( "Number of Cores Specficed:", getOption('cores') ) )
	set.seed(rseed)

	if( ncores == 1 ) {

	lambda <- lapply( 1:R,
	                  myfunc <- function(s){
	                  	                   l = I2C2.rpermute(s + rseed, y = y, I = I, J = J, id = id, visit = visit, p = p,	                  	                                     twoway = twoway, demean = demean,
	                  	                                     T = T, symmetric = symmetric, trun = trun
	                  	                                     );
	                  	                   return(l)
	                  	                   }
	                 )
	}
	else {

	lambda <- mclapply( 1:R,
	                    myfunc <- function(s){
	                    	                  l = I2C2.rpermute(s + rseed, y = y, I = I, J = J, id = id, visit = visit, p = p,
	                    	                                    twoway = twoway, demean = demean,
	                    	                                    T = T, symmetric = symmetric, trun = trun
	                    	                                   );
	                    	                 return(l)
	                    	                 }
	                   )
    }

	lambda = as.vector( unlist(lambda) )
	return(lambda)
}

I2C2.rpermute <- function(s = 1, y, I = NULL, J = NULL, id = NULL, visit = NULL, p = NULL,
		                  twoway = TRUE, demean = FALSE, T = NULL, symmetric = FALSE, trun = FALSE)
{
	
# I2C2.rpermute                    package:????                    R Documentation
#
# Compute I2C2 for Permuted Data 
#
# Description:
#
#     ‘I2C2.rpermute’ is used for computing the I2C2 of the permuted data.
#
#Usage:
#
#	I2C2.rpermute(s, y, I, J, id = NULL, visit = NULL, p = NULL, 
#		          twoway = TRUE, demean = TRUE, T = NULL, symmetric = FALSE, trun = FALSE #trace calculation parameters
#                 ... )
#
#Arguments:
# # s: Random seed, Default = 1
# # y: Dataset (Y11, Y12, ..., YnJn)' => (IJ)-by-p matrix for balanced case, EX) (Y11, Y12, Y21, Y22, ..., YI1, YI2)
# # I: Number of subjects
# # J: Number of repetitions
# # id: Vector of IDs, EX) c(1, 1, 2, 2, 3, 3, 4, 4, ... , I, I)
# # visit: Vector of visits, EX) (1, 2, 1, 2, 1, 2, ... , 1, 2)
# # p: dimension of oberved vectors Yij, EX) Number of voxels
# # twoway, demean, T, symmetric, trun: trace calcuation parameters. See 'I2C2'

    	if(  ( is.null(id) | is.null(visit) ) && ( is.null(I) | is.null(J) ) ){
     	    stop("Not enough information! Please provide (id, visit) or (I,J) !")
     	}
     	
    	if( !( is.null(I) | is.null(J) ) && ( is.null(id) | is.null(visit) ) ) {
        	id <- rep( 1:I, each = J )
        	visit <- rep(1:J, I)
        }
        	

	
	set.seed(s)
	newI = sample( 1:nrow(y), replace = FALSE )
	newX = y[newI, ]	
	lambda=I2C2(newX, id=id, visit=visit, I=I, J=J, T=NULL,p=NULL, twoway=twoway, symmetric=symmetric,demean=demean,trun=trun)$lambda 
	return(lambda)
}


