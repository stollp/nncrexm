# Evaluate Neighbourhood

 library(parallel)
 library(foreach)

#library(doMC)
#registerDoMC(20)

  d <- read.table("FULL_test.txt",header=TRUE)
  
 if (Period == 'P1')  {d <- subset(d, !is.na(d$GBH86)                  ); d$Code07 <- factor(d$Code07)} # neccessary if imp contains living and dead trees
 if (Period == 'P2')  {d <- subset(d, !is.na(d$GBH96) & d$GBH96 != 0   ); d$Code07 <- factor(d$Code07)} # neccessary if imp contains living and dead trees

	names(t_t)[c(12, 13)] <- c("RAD86", "BA86")

d_imp <- data.table(t_t)

if (ALTERNATIVE == 1) cat("Neighbourhood file: covered POINTS removed",   "CROWN position", CROWN)
if (ALTERNATIVE == 2) cat("Neighbourhood file: covered POINTS relocated", "CROWN position", CROWN)

d_imp <- d_imp[d_imp$stat == "alive", ]		      # only living neighbours

## ALTERNATIVE 1: completely remove covered points
  
if (ALTERNATIVE == 1)	d_imp <- subset(d_imp, !is.na(d_imp$S))

if (ALTERNATIVE == 2) {

## ALTERNATIVE 2:           relocate covered points

       d_imp   <- subset(d_imp, !is.na(d_imp$X_rel) & !is.na(d_imp$Y_rel))		# removes completely covered trees in d_imp
       d_imp$X <- d_imp$X_rel 			     			# transfer relocated points
       d_imp$Y <- d_imp$Y_rel
       d_imp$S[is.na(d_imp$S)] <- 1						# reset their S

}

BA86<- d$GBH86/(2*pi); BA86 <- pi*BA86**2        #  Girth -> Basal area
BA96<- d$GBH96/(2*pi); BA96 <- pi*BA96**2
BA01<- d$GBH01/(2*pi); BA01 <- pi*BA01**2
BA07<- d$GBH07/(2*pi); BA07 <- pi*BA07**2

DLOG <- 100; BALOG <- DLOG/(2*pi); BALOG <- pi*BALOG^2

#	sp <- 'SHORFALL'; Period <- 'P1'; # select species and Period

if (Period == 'P1') {		# 	no need to have separate Periods here in demo

	dir <- "nn/"			#	dir <- "nn/P1/"

	BAt1<- BA86
	BAt2<- BA96
}

if (Period == 'P2') {

	dir <- "nn/"			#	dir <- "nn/P2/"

	BAt1<- BA96
	BAt2<- BA07
}

  d <- data.frame(d, BA86, BA96, BA01, BA07, BAt1, BAt2) # as control

  R <- 1:20

xup <- 100		# plot dimensions
yup <- 400

	border <- 1
	
	W <- d$X >= border & d$X <= xup-border &
	     d$Y >= border & d$Y <= yup-border & d$Code07 == sp &
	     !is.na(d$BAt1) & d$BAt1 <= BALOG  & d$BAt1 !=0 				# excl recruits as targets

	OUT <- data.frame(d$Plot[W],d$Tag[W],d$X[W],d$Y[W],d$BAt1[W],d$BAt2[W], 
		d$Tag[W], d$Tag[W], d$Tag[W], d$Tag[W], d$Tag[W], d$Tag[W], d$Tag[W], d$Tag[W], d$Tag[W], d$Tag[W], 
		d$Tag[W], d$Tag[W], d$Tag[W], d$Tag[W], d$Tag[W], d$Tag[W], d$Tag[W], d$Tag[W], d$Tag[W])
		
	names(OUT)[ 1] <- 'Plot'
	names(OUT)[ 2] <- 'Tag'
	names(OUT)[ 3] <- 'x'
	names(OUT)[ 4] <- 'y'
	names(OUT)[ 5] <- 'BAt1'
	names(OUT)[ 6] <- 'BAt2'

	names(OUT)[ 7] <- 'nos'

 	names(OUT)[ 8] <- 'nn_c'			# Number of neighbours
 	names(OUT)[ 9] <- 'nn_C'	
 	names(OUT)[10] <- 'nn_h'	
 	names(OUT)[11] <- 'nn_H'	

	names(OUT)[12] <- 'oa_c'			# sum(BA) of neighbours
 	names(OUT)[13] <- 'oa_C'	
 	names(OUT)[14] <- 'oa_h'	
 	names(OUT)[15] <- 'oa_H'	

	names(OUT)[16] <- 'la_c'			# sum(BA/dist) of neighbours
 	names(OUT)[17] <- 'la_C'	
 	names(OUT)[18] <- 'la_h'	
 	names(OUT)[19] <- 'la_H'	

	names(OUT)[20] <- 'sa_c'			# sum(BA/dist^2) of neighbours
 	names(OUT)[21] <- 'sa_C'	
 	names(OUT)[22] <- 'sa_h'	
 	names(OUT)[23] <- 'sa_H'	

 	names(OUT)[24] <- 'Hegyi_C'		# con, larger and smaller
 	names(OUT)[25] <- 'Hegyi_H'		# het, larger and smaller

	d_MP1 <- d[d$Plot == 'MP1', c('Code07', 'Plot', 'Tag', 'X', 'Y', 'BAt1')]
	d_MP2 <- d[d$Plot == 'MP2', c('Code07', 'Plot', 'Tag', 'X', 'Y', 'BAt1')]

	d_imp_MP1 <- d_imp[d_imp$Plot == 'MP1', ]
	d_imp_MP2 <- d_imp[d_imp$Plot == 'MP2', ]

	out.NN <- foreach (r = 1:20, .packages = c("data.table"), .combine = 'rbind') %dopar% {
	
	W <- OUT$x >= r -20 & OUT$x <= xup-r &				# -15 to avoid Tag -1 being dropped, need at least 2 focals
	     OUT$y >= r -20 & OUT$y <= yup-r					# see below

	out <- OUT[W, ]
									# if 0, no targts within border (SHORPILO, r15)
	n_MP1 <- (length(OUT$Tag[OUT$Plot == 'MP1']) * sum(W[OUT$Plot == 'MP1'])) > 0 # some sp have no targs on either MP1 or MP2
	n_MP2 <- (length(OUT$Tag[OUT$Plot == 'MP2']) * sum(W[OUT$Plot == 'MP2'])) > 0

	i_MP1 <- 1 : length(out$Tag[out$Plot == 'MP1'])			# separate function calls for MP1 and MP2 should
	i_MP2 <- (i_MP1[length(i_MP1)] + 1) : length(out$Tag)	# speed up by reducing data.frames from 7 to 2.5 min !

	# cat("\n\n", n_MP1, i_MP1, n_MP2, i_MP2, "\n\n")

	if (n_MP1) ggg_1 <- foreach(i = i_MP1, .combine = 'rbind') %do% eval_nn(out.Tag = out[i, 'Tag'], d = d_MP1, d_imp = d_imp_MP1, r = r, Period = Period, CROWN = CROWN)
	if (n_MP2) ggg_2 <- foreach(i = i_MP2, .combine = 'rbind') %do% eval_nn(out.Tag = out[i, 'Tag'], d = d_MP2, d_imp = d_imp_MP2, r = r, Period = Period, CROWN = CROWN)

	if ( n_MP1 &  n_MP2) ggg <- rbind(ggg_1, ggg_2)
	if ( n_MP1 & !n_MP2) ggg <-       ggg_1
	if (!n_MP1 &  n_MP2) ggg <-              ggg_2

	out[,  'nos'] <- unlist(ggg[, 'nr_species']) 	# line 155 only produces 'ggg' with more than one dim if at least 2 focals, but here, I only need 1
													# Thus, remove all [1] if used with more than one focal tree
																						
	out[, 'nn_c'] <- unlist(lapply(ggg[, 'group'], FUN = function(x) sum(x == 'con')))  # living neighbours
	out[, 'nn_C'] <- unlist(lapply(ggg[, 'group'], FUN = function(x) sum(x == 'CON')))
	out[, 'nn_h'] <- unlist(lapply(ggg[, 'group'], FUN = function(x) sum(x == 'het')))
	out[, 'nn_H'] <- unlist(lapply(ggg[, 'group'], FUN = function(x) sum(x == 'HET')))

	out[, 'oa_c'] <- unlist(lapply(ggg[, 'group'], FUN = function(x) sum(x == 'con')))
	out[, 'oa_C'] <- unlist(lapply(ggg[, 'group'], FUN = function(x) sum(x == 'CON')))
	out[, 'oa_h'] <- unlist(lapply(ggg[, 'group'], FUN = function(x) sum(x == 'het')))
	out[, 'oa_H'] <- unlist(lapply(ggg[, 'group'], FUN = function(x) sum(x == 'HET')))

	out[, 'la_c'] <- mapply(ggg[, 'S'], FUN = function(x, gr, dist) sum(x[gr == 'con']/dist[gr == 'con']), gr = ggg[, 'group'], dist = ggg[, 'dist'])
	out[, 'la_C'] <- mapply(ggg[, 'S'], FUN = function(x, gr, dist) sum(x[gr == 'CON']/dist[gr == 'CON']), gr = ggg[, 'group'], dist = ggg[, 'dist'])
	out[, 'la_h'] <- mapply(ggg[, 'S'], FUN = function(x, gr, dist) sum(x[gr == 'het']/dist[gr == 'het']), gr = ggg[, 'group'], dist = ggg[, 'dist'])
	out[, 'la_H'] <- mapply(ggg[, 'S'], FUN = function(x, gr, dist) sum(x[gr == 'HET']/dist[gr == 'HET']), gr = ggg[, 'group'], dist = ggg[, 'dist'])

	out[, 'sa_c'] <- mapply(ggg[, 'S'], FUN = function(x, gr, dist) sum(x[gr == 'con']/dist[gr == 'con']^2), gr = ggg[, 'group'], dist = ggg[, 'dist'])
	out[, 'sa_C'] <- mapply(ggg[, 'S'], FUN = function(x, gr, dist) sum(x[gr == 'CON']/dist[gr == 'CON']^2), gr = ggg[, 'group'], dist = ggg[, 'dist'])
	out[, 'sa_h'] <- mapply(ggg[, 'S'], FUN = function(x, gr, dist) sum(x[gr == 'het']/dist[gr == 'het']^2), gr = ggg[, 'group'], dist = ggg[, 'dist'])
	out[, 'sa_H'] <- mapply(ggg[, 'S'], FUN = function(x, gr, dist) sum(x[gr == 'HET']/dist[gr == 'HET']^2), gr = ggg[, 'group'], dist = ggg[, 'dist'])

	out[, 'Hegyi_C'] <- mapply(ggg[, 'S'], FUN = function(x, gr, ta.si) sum(x[gr == 'CON']/ta.si), gr = ggg[, 'group'], ta.si = out[, 'BAt1'])
	out[, 'Hegyi_H'] <- mapply(ggg[, 'S'], FUN = function(x, gr, ta.si) sum(x[gr == 'HET']/ta.si), gr = ggg[, 'group'], ta.si = out[, 'BAt1'])

#	rm(ggg, ggg_1, ggg_2, n_MP1, n_MP2, i_MP1, i_MP2)					# used to save memory

	out.NN <- data.frame(r = rep(r , length(out[, 1])), out)

}

file <- sprintf("%2s%8s", dir, sp); cat(" ", file)		# %3s if P1

save(out.NN, file = file); cat("\n")
