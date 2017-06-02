
# I2C2.mcNulldist                    package:????                    R Documentation
#
# Compute Null Distribution
#
# Description:
#
#     ?I2C2.mcNulldist? is used for computing the null distribution of I2C2 via permutation and multicore computing.
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
I2C2.mcNulldist <- function(y, I = NULL, J = NULL, id = NULL, visit = NULL, p = NULL,
                            twoway = TRUE,  demean = FALSE, T = NULL, symmetric = FALSE, trun = FALSE,
                            rseed = 1234, R = 500, ncores = 4)
{

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
