# self-pruning is prevented because sizes are identical	within crown
# takes neighbours up to k_max'th order into account (cf. origianl)

names(t_t)[c(12, 13)] <- c("RADt1", "BAt1")

T_T <- ppp(t_t$X, t_t$Y, c(-50, 150), c(-50, 450))

#OLD Mac Desktop: , check  = FALSE
#					method = 'interpreted'

prune <- function(t_t = t_t, T_T = T_T, k = k) {
		
	nn <- data.frame(t_t, si = t_t$BAt1[nnwhich(T_T, k = k)],
                         nn_dist =       nndist(T_T, k = k))

	LOG <- nn$nn_dist < delta_d & nn$si > nn$BAt1 & !is.na(t_t$S)

#	if (sum(LOG) >  0) t_t$S[LOG]  <- NA  	# set to NA if bigger neighbour within delta_d

}

k_max <- 50

test <- foreach (k = 1:k_max) %dopar% prune(t_t, T_T, k) # yields test[[k_max]], each one LOG

TEST <- test[[1]]    # if one of them is TRUE, S should be set to NA

for (k in 2:k_max) TEST <- TEST | test[[k]]	# if one of them is TRUE -> S <- NA 

t_t$S[TEST] <- NA

cat("k = ", k, paste("(max = ",k_max,")", sep = ""), "\n")