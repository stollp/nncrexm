# IMPRESSIONISTIC: Randomly distribute points within crown radius [m] = girth[cm] * c_scale

setwd(Sys.getenv(c("TMPDIR")))          # local HD of comupting node

library(parallel)
library(foreach)
library(doMC)

registerDoMC(8)

if (Sys.info()["user"] == 'peter' )           setwd("/Users/peter/Desktop/UBELIX/")
if (Sys.info()["user"] == 'callithrix' ) setwd("/Users/callithrix/Desktop/UBELIX/")

require(MASS)
require(methods)
require(data.table) # ~/R/x86_64-pc-linux-gnu-library/3.3
require(spatstat)
require(sp)
require(plotrix)
require(geometry)
require(magic)
require(abind)
require(deldir)
require(mgcv)

args <- commandArgs(TRUE); MODE    <- args[1]
			   c_scale <- as.numeric(args[2])
			   delta_d <- as.numeric(args[3])

if (MODE != "batch" | is.na(MODE)) {

	MODE <- "interactive"
	
	 c_scale <- 0.00

	 delta_d <- 0.00	# number of points within neighbour crown increases with decreasing delta_d
				# 0 -> no point excluded, complete overlap possible

}

cat("\nMODE = ", MODE, "c_scale = ", c_scale, "delta_d = ", delta_d, "\n\n")

source("/scicore/home/baurb/stollp/EMPIRICAL/tmp/fun/BM.distribute.R")

    D <- read.table("/scicore/home/baurb/stollp/EMPIRICAL/tmp/FULL.txt", header = TRUE); print(head(D))

FIG_1 <- FALSE; cat("\n\n *** FIG_1", FIG_1, "***\n\n")

if (FIG_1) {   # figure 1

   D <- read.table("/scicore/home/baurb/stollp/EMPIRICAL/tmp/FULL_test.txt", header=TRUE); print(head(D))	# first 3 lines from FULL

   D$GBH86[1] <-  50	  	     			#* 1.5
   D$GBH86[2] <- 175					# /1.24
   D$GBH86[3] <- 200					# *1.6
   D$GBH86[4] <- 300					# /5

   D$X[1] <-  5  		# red
   D$Y[1] <-  5

   D$X[2] <- 15			# green
   D$Y[2] <-  5

   D$X[3] <-  5			# blue
   D$Y[3] <- 15

   D$X[4] <- 12.1		# yellow
   D$Y[4] <- 12.1

}

    O <- read.table("/scicore/home/baurb/stollp/EMPIRICAL/tmp/FOUI.txt", header = TRUE)			# contains all OUI's and corresponding intercepts and slopes

      	 			       	 # Replace those 8 with actual fits from Frank's data

O[O$Code07 == 'APORFALC', 'inter'] <- -0.877797; O[O$Code07 == 'APORFALC', 'slope'] <- 0.524540
O[O$Code07 == 'BACCTETR', 'inter'] <-  0.072127; O[O$Code07 == 'BACCTETR', 'slope'] <- 0.332275
O[O$Code07 == 'MALLPENA', 'inter'] <- -1.024571; O[O$Code07 == 'MALLPENA', 'slope'] <- 0.531243
O[O$Code07 == 'MALLWRAY', 'inter'] <- -0.115357; O[O$Code07 == 'MALLWRAY', 'slope'] <- 0.366985
O[O$Code07 == 'PARAMALA', 'inter'] <- -0.816199; O[O$Code07 == 'PARAMALA', 'slope'] <- 0.479244
O[O$Code07 == 'SHORFALL', 'inter'] <- -1.152925; O[O$Code07 == 'SHORFALL', 'slope'] <- 0.506520
O[O$Code07 == 'SHORJOHO', 'inter'] <- -1.378912; O[O$Code07 == 'SHORJOHO', 'slope'] <- 0.594880
O[O$Code07 == 'SHORPARF', 'inter'] <- -1.434121; O[O$Code07 == 'SHORPARF', 'slope'] <- 0.570891

print(head(O))
    
#   D <- subset(D, D$Code07 == 'BARRLANC' | D$Code07 == 'LITHNIEW'| D$Code07 == 'NEOSPHIL' | D$Code07 == 'PARAMALA' | D$Code07 == 'PENTLAXI' |
#				   D$Code07 == 'SHORJOHO' | D$Code07 == 'SHORPARF'| D$Code07 == 'SHORPAUC' | D$Code07 == 'SHORPILO' | D$Code07 == 'DIPTKERR' |
#				   D$Code07 == 'DRYPLONG' | D$Code07 == 'LITHGRAC'| D$Code07 == 'LITHLEPT' | D$Code07 == 'MICRRETI' | D$Code07 == 'OCHAAMEN' |
#				   D$Code07 == 'SCORBORN' | D$Code07 == 'SYZYLINE'| D$Code07 == 'SYZYTAWA')

d_1   <- subset(D, Plot == 'MP1')	# Period 1
d_3   <- subset(D, Plot == 'MP2');	if (FIG_1) {d_3 <- d_1; cat("\n\n *** FIG_1", FIG_1, "***\n\n")}

rm(D)

d_1_a <- d_1$GBH86 > 0 &  d_1$GBH96 >  0
d_1_d <- d_1$GBH86 > 0 & (d_1$GBH96 == 0 | is.na(d_1$GBH96))
d_1_r <- is.na(d_1$GBH86) & !is.na(d_1$GBH96)

d_3_a <- d_3$GBH86 > 0 &  d_3$GBH96 >  0
d_3_d <- d_3$GBH86 > 0 & (d_3$GBH96 == 0 | is.na(d_3$GBH96))
d_3_r <- is.na(d_3$GBH86) & !is.na(d_3$GBH96)

d_1_v <- vector('numeric', length(d_1[,1]))
d_3_v <- vector('numeric', length(d_3[,1]))

d_1_v[d_1_a] <- 1; d_1_v[d_1_d] <- 2; d_1_v[d_1_r] <- 3
d_3_v[d_3_a] <- 1; d_3_v[d_3_d] <- 2; d_3_v[d_3_r] <- 3

d_1_f <- factor(d_1_v, labels = c('P2 recruit', 'alive', 'dead', 'recruit'), levels = c(0:3))	# MP1 P1
d_3_f <- factor(d_3_v, labels = c('P2 recruit', 'alive', 'dead', 'recruit'), levels = c(0:3))	# MP2 P1

columns <- c("Plot", "Tag", "X", "Y", "F_Code07", "Code07", "GBH86", "GBH96", "GBH01", "GBH07")

d_1 <- data.table(d_1[, columns], stat = d_1_f)
d_3 <- data.table(d_3[, columns], stat = d_3_f)

d_1 <- data.table(d_1, RAD86 = d_1$GBH86/(2*pi)); d_1 <- data.frame(d_1, BA86 = pi * d_1$RAD86^2)
d_3 <- data.table(d_3, RAD86 = d_3$GBH86/(2*pi)); d_3 <- data.frame(d_3, BA86 = pi * d_3$RAD86^2)

d_1 <- subset(d_1, d_1$stat == 'alive' | d_1$stat == 'dead'); d_1 <- subset(d_1, d_1$GBH86 >= 10) # excl regressors which were not randomized
d_3 <- subset(d_3, d_3$stat == 'alive' | d_3$stat == 'dead'); d_3 <- subset(d_3, d_3$GBH86 >= 10) # excl regressors which were not randomized

#   print(dim(rbind(d_1, d_3))); stopifnot(1==2)

if (c_scale == 0) {

	cr_1 <- d_1$GBH86 * c_scale		#; cr_1 <- sqrt(((2*d_1$RAD86)^1.0)/pi)		# cf Muller_Landau et al. 2006
	cr_3 <- d_3$GBH86 * c_scale		#; cr_3 <- sqrt(((2*d_3$RAD86)^1.0)/pi)

}

#if (c_scale != 0) {			# size class fits from BAAD (Falster et al. 2015)

#	cr_1 <- d_1$GBH86		# make sure they have the right length
#	cr_3 <- d_3$GBH86

#	cr_1[d_1$GBH86 >= 10 & d_1$GBH86 <  50] <-         d_1$GBH86[d_1$GBH86 >= 10 & d_1$GBH86 <  50] * 0.063		
#	cr_3[d_3$GBH86 >= 10 & d_3$GBH86 <  50] <-         d_3$GBH86[d_3$GBH86 >= 10 & d_3$GBH86 <  50] * 0.063	

#	cr_1[d_1$GBH86 >= 50 & d_1$GBH86 < 100] <- 0.812 + d_1$GBH86[d_1$GBH86 >= 50 & d_1$GBH86 < 100] * 0.033		
#	cr_3[d_3$GBH86 >= 50 & d_3$GBH86 < 100] <- 0.812 + d_3$GBH86[d_3$GBH86 >= 50 & d_3$GBH86 < 100] * 0.033	

#	cr_1[d_1$GBH86 > 100]                   <- 1.493 + d_1$GBH86[d_1$GBH86 > 100]                   * 0.026		
#	cr_3[d_3$GBH86 > 100]                   <- 1.493 + d_3$GBH86[d_3$GBH86 > 100]                   * 0.026	

#}

if (c_scale != 0) {                     # single size fit  from Sterck et al. 2001

   cr_1 <-  -1.003 + 0.523 * sqrt(d_1$GBH86)
   cr_3 <-  -1.003 + 0.523 * sqrt(d_3$GBH86)

}


#if (c_scale != 0) {			# family fits from Sterck et al. 2001

#   cr_1 <- d_1$GBH86			  # make sure they have the right length
#   cr_3 <- d_3$GBH86

#   cr_1[d_1$F_Code07 == 'DIPT']                          <-  -1.327 + 0.560 * sqrt(d_1$GBH86[d_1$F_Code07 == 'DIPT'])
#   cr_1[d_1$F_Code07 == 'EUPH']                          <-  -0.489 + 0.440 * sqrt(d_1$GBH86[d_1$F_Code07 == 'EUPH'])
#   cr_1[d_1$F_Code07 != 'DIPT' & d_1$F_Code07 != 'EUPH'] <-  -0.480 + 0.410 * sqrt(d_1$GBH86[d_1$F_Code07 != 'DIPT' & 
#                                                                                             d_1$F_Code07 != 'EUPH'])       

#   cr_3[d_3$F_Code07 == 'DIPT']                          <-  -1.327 + 0.560 * sqrt(d_3$GBH86[d_3$F_Code07 == 'DIPT'])
#   cr_3[d_3$F_Code07 == 'EUPH']                          <-  -0.489 + 0.440 * sqrt(d_3$GBH86[d_3$F_Code07 == 'EUPH'])
#   cr_3[d_3$F_Code07 != 'DIPT' & d_3$F_Code07 != 'EUPH'] <-  -0.480 + 0.410 * sqrt(d_3$GBH86[d_3$F_Code07 != 'DIPT' &
#                                                                                             d_3$F_Code07 != 'EUPH'])

#}

#if (c_scale !=0) {			# species specific fits based on relation between OUI and Frank's data

#   cr_1 <- d_1$GBH86                     # make sure they have the right length
#   cr_3 <- d_3$GBH86

#   for (i in 1:length(cr_1)) {			# do it tree by tree

#       m <- match(d_1$Code07[i], O$Code07)	# find name in OUI_list and insert corresponding slope and inter below

#       cr_1[i] <- O[m, 'inter'] + O[m, 'slope'] * sqrt(d_1$GBH86[i])

#   }


#   for (i in 1:length(cr_3)) {			# do it	tree by	tree

#       m <- match(d_3$Code07[i], O$Code07)	# find name in OUI_list	and insert corresponding slope and inter below

#       cr_3[i] <- O[m, 'inter'] + O[m, 'slope'] * sqrt(d_3$GBH86[i])

#   }

#}

cat("\n"); print(head(data.frame(d_1, cr_1)))
cat("\n"); print(head(data.frame(d_3, cr_3)))

cat("\n"); print(summary(cr_1)); print(summary(cr_3)); cat("\n")

t_1 <- BM.distribute(d = d_1, cr = cr_1, Period = 'P1') 
t_3 <- BM.distribute(d = d_3, cr = cr_3, Period = 'P1')

if (TRUE) {
	
	BA_test <- subset(d_1, d_1$stat == 'alive' | d_1$stat == 'dead')
	BA_dist <- BM.distribute(BA_test, cr = BA_test$GBH86 * c_scale, Period = 'P1')

	cat("\nSUM BA should equal the number of points distributed within crown radius\n\n")

	cat("MP1, P1")
	cat(sprintf("%6.2f", sum(BA_test$BA86)/(100*100)/4))
	cat(sprintf("%6.2f", sum(BA_dist$S)   /(100*100)/4), "\n")

	BA_test <- subset(d_3, d_3$stat == 'alive' | d_3$stat == 'dead')
	BA_dist <- BM.distribute(BA_test, cr = BA_test$GBH86 * c_scale, Period = 'P1')

	cat("MP2, P1")
	cat(sprintf("%6.2f", sum(BA_test$BA86)/(100*100)/4))
	cat(sprintf("%6.2f", sum(BA_dist$S)   /(100*100)/4),"\n")

}

cat("\ninclude plasticity\n")

# visit each point, if bigger within delta_d, set to NA

t_t <- t_1

# ORIGINAL

   system.time(source("/scicore/home/baurb/stollp/EMPIRICAL/tmp/0_Prune.R")); 
   system.time(source("/scicore/home/baurb/stollp/EMPIRICAL/tmp/0_Prune_CALC.R"))

# TEST: identical to default: local = FALSE?

#   system.time(source("/scicore/home/baurb/stollp/EMPIRICAL/tmp/0_Prune.R", local = TRUE));
#   system.time(source("/scicore/home/baurb/stollp/EMPIRICAL/tmp/0_Prune_CALC.R", local = TRUE))

# Answer: YES, identical. So this is not the cause (20. 10. 2018)

t_1 <- t_t; names(t_1)[c(12, 13)] <- c("RAD86", "BA86")

rm(X_rel, Y_rel, biggest, covered, m_x, m_y, Den, c_L, pts, POL, NNN, res)

cat("\tMP1 P1 finished plasticity: c_scale =", c_scale, "delta_d =", delta_d)

t_t <- t_3

system.time(source("/scicore/home/baurb/stollp/EMPIRICAL/tmp/0_Prune.R", local = TRUE)); 
system.time(source("/scicore/home/baurb/stollp/EMPIRICAL/tmp/0_Prune_CALC.R", local = TRUE))

t_3 <- t_t; names(t_3)[c(12, 13)] <- c("RAD86", "BA86")

rm(X_rel, Y_rel, biggest, covered, m_x, m_y, Den, c_L, pts, POL, NNN, res)

cat("\tMP2 P1 finished plasticity: c_scale =", c_scale, "delta_d =", delta_d)

# t_1 <- subset(t_1, !is.na(t_1$S)); t_3 <- subset(t_3, !is.na(t_3$S))	# MP1 + MP2, P1

FULL_P1 <- data.table(rbind(t_1, t_3))

FULL_P1$X <- round(FULL_P1$X, 2)
FULL_P1$Y <- round(FULL_P1$Y, 2)

FULL_P1$RAD86 <- round(FULL_P1$RAD86, 1)
FULL_P1$BA86  <- round(FULL_P1$BA86, 1)

save(c_scale, delta_d, FULL_P1, file = "/scicore/home/baurb/stollp/EMPIRICAL/tmp/FULL_P1")
