# IMPRESSIONISTIC: Randomly distribute points within crown radius [m] = girth[cm] * c_scale

 DDD <- paste(getwd(), "/", sep = "")

 library(foreach)

#library(parallel)
#library(doMC)
#registerDoMC(8)

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
	
args <- commandArgs(TRUE); MODE   <- args[1]		# used if run as batch with command line arguments
			   c_scale <- as.numeric(args[2])
			   delta_d <- as.numeric(args[3])

if (MODE != "batch" | is.na(MODE)) {

	    MODE <- "interactive"
	
	 c_scale <- 1.00

	 delta_d <- 1.20		# number of points within neighbour crown increases with decreasing delta_d
							# 0 -> no point excluded, complete overlap possible

	    ppca <- 10			# points per crown in BM.distribute.R

}

cat("\nMODE = ", MODE, "c_scale = ", c_scale, "delta_d = ", delta_d, "\n")

source(paste(DDD, "fun/BM.distribute.R", sep = ""))

#    D <- read.table(paste(DDD, "FULL.txt", sep =""), header = TRUE); print(head(D))

FIG_1 <- TRUE

if (FIG_1) {   # figure 1

	if (FIG_1) cat("\n *** FIG_1", FIG_1, "***\n")

   d <- read.table(paste(DDD, "FULL_test.txt", sep = ""), header=TRUE)	#; print(head(d))

}

d_a <- d$GBH86 > 0 &  d$GBH96 >  0
d_d <- d$GBH86 > 0 & (d$GBH96 == 0 | is.na(d$GBH96))
d_r <- is.na(d$GBH86) & !is.na(d$GBH96)

d_v <- vector('numeric', length(d[,1]))

d_v[d_a] <- 1; d_v[d_d] <- 2; d_v[d_r] <- 3

d_f <- factor(d_v, labels = c('P2 recruit', 'alive', 'dead', 'recruit'), levels = c(0:3))

columns <- c("Plot", "Tag", "X", "Y", "F_Code07", "Code07", "GBH86", "GBH96", "GBH01", "GBH07")

d <- data.table(d[, columns], stat = d_f)

d <- data.table(d, RAD86 = d$GBH86/(2*pi)); d <- data.frame(d, BA86 = pi * d$RAD86^2)

d <- subset(d, d$stat == 'alive' | d$stat == 'dead')

if (c_scale == 0) {						# crown radius 0 -> non-spatial

	cr <- d$GBH86 * c_scale

}

if (c_scale != 0) {                     # single size fit  from Sterck et al. 2001

   cr <-  -1.003 + 0.523 * sqrt(d$GBH86)

}

cat("\n"); print(head(data.frame(d[, -c(1, 8:10)], cr)))

cat("\nCrown radii\n\n"); print(summary(cr)); cat("\n")

t_t <- BM.distribute(d = d, cr = cr, Period = 'P1') 

if (TRUE) {
	
	BA_test <- subset(d, d$stat == 'alive' | d$stat == 'dead')
	BA_dist <- BM.distribute(BA_test, cr = BA_test$GBH86 * c_scale, Period = 'P1')

	cat("IF number of points equal to trees ba -> SUM BA should equal\n")
	cat("        the number of points distributed within crown radius\n\n")

	cat("MP1, P1")
	cat(sprintf("%12.2f", sum(BA_test$BA86)))
	cat(sprintf("%12.2f", sum(BA_dist$S)), "\n")

}

cat("\ninclude plasticity: ")			# visit each point, if bigger within delta_d, set to NA

source(paste(DDD, "0_Prune.R",      sep = ""))
source(paste(DDD, "0_Prune_CALC.R", sep = ""))

names(t_t)[c(12, 13)] <- c("RADt1", "BAt1")

save(t_t, c_scale, delta_d, file =
	paste("FIG_1_", ppca, "ppca_c_", sprintf("%3.2f", c_scale), "_d_", sprintf("%3.2f", delta_d), sep = ""))

