require(compiler)						# cf EOF

eval_nn <- function(out.Tag, d, d_imp, r, Period, CROWN) {

# evaluate neighbourhoods of focal trees

       t <- match(out.Tag, d$Tag)		# find Tag. Using 'd' instead of 'd_imp' ensures that all,
										# targes - even completely covered ones - are used as focals.
										
 targ_co <- as.character(d$Code07[t])
 targ_nr <- d$Tag[t]
 targ_x  <- d$X[t]
 targ_y  <- d$Y[t]
 targ_si <- d$BAt1[t]

if (CROWN) {		# if TRUE, take CROWN position, i.e. centroid of uncovered points instead of stem coordinates

   tmp_tmp <- d_imp[d_imp$Tag == targ_nr & !is.na(d_imp$S), ]

   if (dim(tmp_tmp)[1] > 0) {

      targ_x <- mean(tmp_tmp$X)			# take mean of those not covered by bigger neighbours
      targ_y <- mean(tmp_tmp$Y)

#	cat(r, targ_nr, targ_x, targ_y, "\n")

#		points(targ_x, targ_y, pch = 21, bg = 'white', cex = 1, col = 'black')

      }

}

neighbours <-      d_imp[     d_imp$X %between% c(targ_x - r, targ_x + r) ]
neighbours <- neighbours[neighbours$Y %between% c(targ_y - r, targ_y + r) ]

neighbours <- neighbours[neighbours$Tag != targ_nr, ]

# restrict to circular neighbourhood
dist <- sqrt((targ_x - neighbours$X)^2 + (targ_y - neighbours$Y)^2)

neighbours <- cbind(neighbours[dist < r], dist = dist[dist < r])

if (length(neighbours$Tag) > 0) {

	nr_species <- nlevels(factor(neighbours$Code07))			# Number of neigbhouring species

	group <- vector('numeric', length = length(neighbours$dist))

		if (Period == 'P1') {
			group[neighbours$Code07==targ_co & neighbours$BA86<=targ_si] <- 1	# smaller or equal con
			group[neighbours$Code07==targ_co & neighbours$BA86> targ_si] <- 2	#           bigger CON
			group[neighbours$Code07!=targ_co & neighbours$BA86<=targ_si] <- 3	# smaller or equal het
			group[neighbours$Code07!=targ_co & neighbours$BA86> targ_si] <- 4	#           bigger HET
		}

		if (Period == 'P2') {
			group[neighbours$Code07==targ_co & neighbours$BA96<=targ_si] <- 1
			group[neighbours$Code07==targ_co & neighbours$BA96> targ_si] <- 2
			group[neighbours$Code07!=targ_co & neighbours$BA96<=targ_si] <- 3
			group[neighbours$Code07!=targ_co & neighbours$BA96> targ_si] <- 4
		}

	     group <- factor(group, levels=c(1:4), labels = c('con', 'CON', 'het', 'HET'))

	neighbours <- cbind(neighbours, group)

}

if (length(neighbours$Tag) == 0) {			# if no neighbours, append to cols and rename
	
	nr_species <- 0
	
	neighbours <- cbind(as.data.frame(neighbours), as.data.frame(neighbours)[, c(13, 14)])
	
	names(neighbours)[15] <- 'distance'
	names(neighbours)[16] <- 'group'
	
	group <- neighbours[,16]
	
	}

	list(neighbours = neighbours, group = group, nr_species = nr_species, 
		          S = neighbours$S, dist = neighbours$dist)

}

# Precompile

eval_nn <- cmpfun(eval_nn)