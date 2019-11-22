#	Outcoment lines 42 in 0_Prune_CALC.R but comment line 73 -> 0_Prune_CALC_GRAPH.R

	source("fun/gw.R"); require(plotrix); require(data.table); require(MASS); require(sp)

	if (!dir.exists("nn")) dir.create("nn")		# will contain file with neighbourhood information

#	load(file = "FIG_1_c_0.00_d_0.00",  			verbose = FALSE)	# non-spatial (c_scale = 0)

# 	for non-spatial (c_0.00), I have only the version with number of points proportional to BA

																		#     spatial (c_scale = 1)
									# completely covered
#	load(file = "FIG_1_100ppca_c_1.00_d_0.00",  verbose = FALSE)		# 100 point per unit crown area for each tree
	load(file = "FIG_1_100ppca_c_1.00_d_1.20",  verbose = FALSE)		# ficilitates counting

#	load(file = "FIG_1_10ppca_c_1.00_d_0.00",  verbose = FALSE)		# 10 point per unit crown area
#	load(file = "FIG_1_10ppca_c_1.00_d_1.20",  verbose = FALSE)		# ficilitates counting

	ALTERNATIVE = 1			# 1: removed   2: relocated	<- completely covered points are either removed or relocated		
	      CROWN = TRUE		# F: stem      T: crown     <- relevant coordinates of focal's positions

     FULL <- TRUE			# TRUE: all 4 panels in 1 Figure, FALSE: single figures for each panel

if ( FULL) {gw(2, 2, 8, 8, ps = 10, title = '1'); min.x <- 0; min.y = 0; max.x <- 25; max.y <- 25}
if (!FULL) {gw(1, 1,       ps = 10, title = '1'); min.x <- 0; min.y = 0; max.x <- 25; max.y <- 25}

#	max.x <- 15; max.y <- 15

#	min.x <-  5.0 - 5  ; max.x =  5.0 + 5  ; min.y <-  5 - 5  ; max.y <-  5 + 5    # focus tag 1
#	min.x <- 12.1 - 2.5; max.x = 12.1 + 2.5; min.y <- 10 - 2.5; max.y <- 10 + 2.5  # focus tag 5

d <- read.table("FULL_test.txt", header = TRUE, stringsAsFactors = FALSE); n.tr <- dim(d)[1]

#	d$col[d$Code07 == 'MALLWRAY'] <- 'black'

 BA <-                  pi * (d$GBH86/(2*pi))^2
 cr <-  -1.003 + 0.523 * sqrt(d$GBH86)												# crown radii

  d <- data.frame(d, cr = round(cr, 1), BA = round(BA, 1), CA = round(pi * cr^2 , 1)); cat("\n")
  
  cat("Original data file\n\n"); print(d[, -c(1, 5, 8:10)])

  d.2 <- d	# to be used for relocated

t_t$X     <- round(t_t$X,     1);	t_t$Y     <- round(t_t$Y,     1)
t_t$X_rel <- round(t_t$X_rel, 1);	t_t$Y_rel <- round(t_t$Y_rel, 1)
t_t$RADt1 <- round(t_t$RADt1, 2);	t_t$BAt1  <- round(t_t$BAt1,  1)

cat("\nImpressionistic data file: c_scale", c_scale, "delta_d", delta_d, "\n\n")

print(data.table(t_t[, -c(1, 5, 8, 9, 10)]))

plot(0, 0, asp = 1,  pch = 16, col = 'white',     main = 'a) Classic neigbhourhood model',
		  xlim = c(min.x, max.x), ylim = c(min.y, max.y), xlab = 'X [m]', ylab = 'Y [m]')

			draw.circle(x = 5, y = 5,  radius = c(5, 10, 15, 20), lwd = 1)		# 5, 10 & 15 m neighbourhoods
# 			draw.circle(d$X[5], d$Y[5],         c(   10, 15),     lty = 3)		# 5, 10 & 15 m neighbourhoods

			stem.size <- c(1, 2, 2, 3, 1)

			for (i in 1:n.tr)	points(d$X[i], d$Y[i], pch = 21, bg = d$col[i], cex = stem.size[i], col = 'black')

			text(10,       4, '5 m')
			text(15 - .25, 4, '10 m')
			text(20 - .25, 4, '15 m')
			text(25 - .25, 4, '20 m')

if (!FULL) gw(1, 1, ps = 10, title = '2')

plot(0, 0, asp = 1,  pch = 16, col = 'white', main = bquote(bold("b) Impressionistic, "~Delta*paste("d = ", sep = "")~.(0))), 
		  xlim = c(min.x, max.x), ylim = c(min.y, max.y), xlab = '', ylab = '')

			for (i in 1:n.tr) draw.circle(d$X[i], d$Y[i], d$cr[i], lwd = 2, border = d$col[i])		# crowns

			for (i in 1:n.tr) points(t_t$X[t_t$Tag == d$Tag[i]], 				# ALL points = DELTA_D = 0
	        				         t_t$Y[t_t$Tag == d$Tag[i]],   pch = 16, col = d$col[i], cex =  1, lwd = 1)


#			for (i in 1:n.tr) points(t_t$X[t_t$Tag == d$Tag[i] & t_t$S == 1], 				# uncovered points
#	        				         t_t$Y[t_t$Tag == d$Tag[i] & t_t$S == 1],   pch = 16, col = d$col[i], cex =  1, lwd = 1)

#			for (i in 1:n.tr) points(t_t$X[t_t$Tag == d$Tag[i] & is.na(t_t$S)], 				# covered points
#	        				         t_t$Y[t_t$Tag == d$Tag[i] & is.na(t_t$S)], pch = 21, col = d$col[i], cex = .7, lwd = 1)

			draw.circle(5, 5,            c(5, 10, 15, 20), lwd = 1)					# 5, 10 & 15 m neighbourhoods
#			draw.circle(d$X[5], d$Y[5],  c(   10, 15    ), lty = 3)					# 5, 10 & 15 m neighbourhoods

			for (i in 1:n.tr)	points(d$X[i], d$Y[i], pch = 21, bg = d$col[i], cex = stem.size[i], col = 'black')


if (!FULL) gw(1, 1, ps = 10, title = '3')

plot(0, 0, asp = 1, pch = 16,  col = 'white',     main = bquote(bold("c) Removed, "~Delta*paste("d = ", sep = "")~.(delta_d))),
		  xlim = c(min.x, max.x), ylim = c(min.y, max.y), xlab = '', ylab = '')

							# draw circular crown only for 'biggest'
			for (i in 1:n.tr) if (sum(is.na(t_t$S[t_t$Tag == d$Tag[i]])) == 0) draw.circle(d$X[i], d$Y[i], d$cr[i], lwd = 2, border = d$col[i])		# crowns

			source("0_Prune_CALC_GRAPH.R")

			for (i in 1:n.tr) points(t_t$X[t_t$Tag == d$Tag[i] & t_t$S == 1], 				# uncovered points only
	        				         t_t$Y[t_t$Tag == d$Tag[i] & t_t$S == 1],   pch = 16, col = d$col[i], cex =  1, lwd = 1)

			draw.circle(5, 5,            c(5, 10, 15, 20), lwd = 1)					# 5, 10 & 15 m neighbourhoods
#			draw.circle(d$X[5], d$Y[5],           15     , lty = 3)					# 5, 10 & 15 m neighbourhoods

			for (i in 1:n.tr)	points(d$X[i], d$Y[i], pch = 21, bg = d$col[i], cex = stem.size[i], col = 'black')

if (CROWN)	for (i in 1:n.tr)	points(mean(t_t$X[t_t$Tag == d$Tag[i] & !is.na(t_t$S)]),
 									   mean(t_t$Y[t_t$Tag == d$Tag[i] & !is.na(t_t$S)]), 
 									   pch = 21, bg = 'white', cex = stem.size[i], col = 'black')


			ALTERNATIVE <- 1; source("1_Loop.R"); d$col <- as.character(d$col); d <- d.2

if (!FULL) gw(1, 1, ps = 10, title = '4')

plot(0, 0, asp = 1,   pch = 16,  col = 'white',   main = bquote(bold("d) Relocated, "~Delta*paste("d = ", sep = "")~.(delta_d))),
		  xlim = c(min.x, max.x), ylim = c(min.y, max.y), xlab = '', ylab = '')

							# draw circular crown only for 'biggest'
			for (i in 1:n.tr) if (sum(is.na(t_t$S[t_t$Tag == d$Tag[i]])) == 0) draw.circle(d$X[i], d$Y[i], d$cr[i], lwd = 2, border = d$col[i])		# crowns

			source("0_Prune_CALC_GRAPH.R")

			for (i in 1:n.tr) points(t_t$X    [t_t$Tag == d$Tag[i] & t_t$S == 1], 			# uncovered points
	        				         t_t$Y    [t_t$Tag == d$Tag[i] & t_t$S == 1],   pch = 16, col = d$col[i], cex =  1, lwd = 1)

			for (i in 1:n.tr) points(t_t$X_rel[t_t$Tag == d$Tag[i] & is.na(t_t$S)], 			# relocated covered points
	        				         t_t$Y_rel[t_t$Tag == d$Tag[i] & is.na(t_t$S)], pch = 21, col = d$col[i], cex = .7, lwd = 1)

			draw.circle(5, 5,            c(5, 10, 15, 20), lwd = 1)					# 5, 10 & 15 m neighbourhoods
#			draw.circle(d$X[5], d$Y[5],           15     , lty = 3)					# 5, 10 & 15 m neighbourhoods

			for (i in 1:n.tr)	points(d$X[i], d$Y[i], pch = 21, bg = d$col[i], cex = stem.size[i], col = 'black')

if (CROWN)	for (i in 1:n.tr)	points(mean(t_t$X_rel[t_t$Tag == d$Tag[i]]),
 									   mean(t_t$Y_rel[t_t$Tag == d$Tag[i]]), 
 									   pch = 21, bg = 'white', cex = stem.size[i], col = 'black')


			ALTERNATIVE <- 2; source("1_Loop.R")