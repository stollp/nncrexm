# function to 'distribute' biomass

# Impressionistische Punktverteilung innerhalb Kronenradius

BM.distribute <- function(d = NULL, cr = NULL, Period = 'P1') {
	
	set.seed(131313)
	
	n <- length(d$X)				#; print(n)
	
	x <- d$X
	y <- d$Y
	
	if (Period == 'P1') {

		size <- round(d$BA86, dig = 0)		#; print(size)
		         r <- d$RAD86

	}

	if (Period == 'P2') {

		size <- round(d$BA96, dig = 0)		#; print(size)
		         r <- d$RAD96

	}

#	size <- round(size/10, dig = 0); size[size == 0] <- 1

	# *** Equal number of points per unit crown area (ppca) ***

	if (sum(cr) != 0) {    	   # prevent only 1 line for c_scale 0, non-spatial

	  size <- round(ppca * (pi * cr^2), 0) 	# 10 points per unit crown area: number of lines in FULL from 1.2 -> 1.8 *10^6
 	  size[size == 0] <- 1	   	     		# But 'sum(BA) is smaller because especially larger trees have much less!!!

	}
												# facilitates counting
#	size <- c(ppca, ppca, ppca, ppca, ppca)		# constantly ppca points for each tree to illustrate counting

	N <- sum(size)						#; print(N)

	    Plot <- vector('character', length = N)
   	     Tag <- vector(  'numeric', length = N)
	
	       X <- vector(  'numeric', length = N)
	       Y <- vector(  'numeric', length = N)

	F_Code07 <- vector('character', length = N)
	  Code07 <- vector('character', length = N)
	   GBH86 <- vector(  'numeric', length = N)
	   GBH96 <- vector(  'numeric', length = N)
	   GBH01 <- vector(  'numeric', length = N)
	   GBH07 <- vector(  'numeric', length = N)
	    stat <- vector('character', length = N)

if (Period == 'P1') { 
	   RAD86 <- vector(  'numeric', length = N)
	    BA86 <- vector(  'numeric', length = N)
}

if (Period == 'P2') { 
	   RAD96 <- vector(  'numeric', length = N)
	    BA96 <- vector(  'numeric', length = N)
}

  	       S <- vector('numeric', length = N)
	
	index <- 1:size[1]
	
	for (i in 1:n) {
	
#		cat(i, size[i], index, "\n")

		     Plot[index] <- as.character(d$Plot[i])
		      Tag[index] <- d$Tag[i]

		 F_Code07[index] <- as.character(d$F_Code07[i])
		   Code07[index] <- as.character(d$Code07[i])
		    GBH86[index] <- d$GBH86[i]
		    GBH96[index] <- d$GBH96[i]
		    GBH01[index] <- d$GBH01[i]
		    GBH07[index] <- d$GBH07[i]
		     stat[index] <- as.character(d$stat[i])

if (Period == 'P1') {
		    RAD86[index] <- d$RAD86[i]
		     BA86[index] <- d$BA86[i]
}

if (Period == 'P2') {
		    RAD96[index] <- d$RAD96[i]
		     BA96[index] <- d$BA96[i]
}

if (TRUE) {
## random

		angle <-              runif(n = size[i], min = 0, max = 2*pi)
		  rad <- cr[i] * sqrt(runif(n = size[i], min = 0, max = 1))

		# Where ‘R’ = g[i]*c_scale is the radius of the original disk.
		# Then: x[i]=rad*cos(angle)
		#       y[i]=rad*sin(angle)
		
		X[index] <- x[i] + rad * cos(angle)		# test by putting # after + so all points have identical coordinates
		Y[index] <- y[i] + rad * sin(angle)		# OR setting c_scale = 0 -> rad = 0		o.k. 17. 06. 2014
}

if (FALSE) {
## regular

		require(sampSurf)

		if (cr[i] == 0) cr[i] <- 0.00000001

		p.c <- spCircle(radius = cr[i], centerPoint = c(x = x[i], y = y[i]))
		P.C <- spsample(p.c$spCircle, n = size[i], type = 'regular')

		X[index] <- P.C@coords[, 1]
		Y[index] <- P.C@coords[, 2]

}

		S[index] <- 1
		
		if (i < n) {
			last  <- index[length(index)] 
			index <- 1:size[i+1]
			index <- last + index

		}
		
	}
	
	if (Period == 'P1') distribute <- data.frame(Plot, Tag, X, Y, F_Code07, Code07, GBH86, GBH96, GBH01, GBH07, stat, RAD86, BA86, S)
	if (Period == 'P2') distribute <- data.frame(Plot, Tag, X, Y, F_Code07, Code07, GBH86, GBH96, GBH01, GBH07, stat, RAD96, BA96, S)

	return(distribute)

}