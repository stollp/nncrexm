# calculate relocations, those with is.na(S) will have relacated coordinates within countour of those without bigger neighbours

X_rel <- round(t_t$X, 2)
Y_rel <- round(t_t$Y, 2)

for (tag in unique(t_t$Tag)) {
		
	biggest <- length(t_t$X[t_t$Tag == tag &  is.na(t_t$S)]) == 0	# no bigger neighbours
	covered <- length(t_t$X[t_t$Tag == tag & !is.na(t_t$S)]) <= 5	# +/- completely covered
	
#	cat(tag, biggest, covered, "\n")

	if ( covered) {X_rel[t_t$Tag == tag] <- NA; Y_rel[t_t$Tag == tag] <- NA}

	if (!biggest & !covered) {

	   m_x <-  mean(t_t$X[t_t$Tag == tag & !is.na(t_t$S)])	# center on mean x/y of uncovered points
	   m_y <-  mean(t_t$Y[t_t$Tag == tag & !is.na(t_t$S)])

	   x_r <- range(t_t$X[t_t$Tag == tag])					# adjust lim to crown radius
	   y_r <- range(t_t$Y[t_t$Tag == tag])
		    
	   x_d <- x_r[2] - x_r[1]
	   y_d <- y_r[2] - y_r[1]
			     
	   if (x_d >= y_d) m_d <- x_d							# take the bigger one
	   if (y_d >  x_d) m_d <- y_d
				      
	   if (m_d < 5) m_d <- 5	

 	   Den <- kde2d(t_t$X[t_t$Tag == tag & !is.na(t_t$S)], t_t$Y[t_t$Tag == tag & !is.na(t_t$S)], n = 50, 
	                lims = c(c(m_x - m_d, m_x + m_d), c(m_y - m_d, m_y + m_d)))

	   c_L <- contourLines(Den, levels = c(0, 0.001, 0.01, 0.05, .1, 0.25, 0.5, 1))

	   if (length(c_L) > 0) {

	     c_L <- contourLines(Den, levels = c_L[[1]]$level)  # select lowest level for contour around points without bigger neighbour

		   pts <- coordinates(data.frame(x = c_L[[1]]$x, y = c_L[[1]]$y)); pts <- rbind(pts, pts[1,])	# close polygon

#		   		lines(pts, col = d$col[tag], lwd = 2)

		   POL <- SpatialPolygons(list( Polygons(list(Polygon(pts)), 1)))	

		   NNN <- sum(is.na(t_t$S[t_t$Tag == tag]))

#		   cat(tag, "\n")

		   set.seed(131313)

		   res <- try(spsample(POL, NNN, type = 'random'), silent = TRUE)

		   # if iteration converged, sample within contour to relocate points with bigger neighbours within delta_d

#		   print(class(res))

		   if (class(res) != "try-error") {set.seed(131313); REM <- spsample(POL, NNN, type = 'random')}
		   if( class(res) == "try-error") {                  REM <- data.frame(x = rep(NA, NNN), y = rep(NA, NNN))
		       		     		  		     cat(tag); print(class(res)); cat(length(c_L))}

		   if (NNN == 1) {REM <- data.frame(REM); names(REM) <- c('x','y')}	 # col name coords if only 1, x/y if > 1

		   X_rel[t_t$Tag == tag & is.na(t_t$S)] <- round(REM$x, 2)
		   Y_rel[t_t$Tag == tag & is.na(t_t$S)] <- round(REM$y, 2)

	   }

     }

}
	
	t_t <- data.frame(t_t, X_rel, Y_rel)
