gw <- function(r = 1, c = 1, w = 8, h = 8, ps = 18, title = 'graph', device = 'Desktop') {

if (!Sys.getenv("RSTUDIO") == 1) {

   # Utilit function to handle different graphics devices

   Benutzer <- Sys.info()["user"]  # get user name

   # Screen devices

   if (device == 'Desktop') {
      getOption("device")(width = w, height = h, pointsize = ps, title = title)		# open graphics device on Mac
   }

   if (device == 'PC') {
      getOption("device")(width = w, height = h, pointsize = ps, title = title)	# open graphics device on Windows PC
      # if error: change to get(getOption("device"))
   }

   # File or print devices. Note: have to be closed using graphics.off()

   if (device == 'JPEG') {
      jpeg(paste("/Users/", Benutzer, "/Desktop/Draft.jpg", sep = ""), width = w, height = h, pointsize = ps, quality = 100, units = 'in', res = 576)
   }

   if (device == 'PDF') {
       pdf(paste("/Users/", Benutzer, "/Desktop/Draft.pdf", sep = ""), width = w, height = h, pointsize = ps)
   }

   if (device == 'PS') {
postscript(paste("/Users/", Benutzer, "/Desktop/Draft.ps", sep = ""),  width = w, height = h, pointsize = ps, horizontal = TRUE)
   }
	
   onefile <- TRUE
	
   if (device == 'Cairo') {
   		cairo_pdf(filename = if(onefile) paste("/Users/", Benutzer, "/Desktop/Draft.pdf",     sep = "") 
				     	    else paste("/Users/", Benutzer, "/Desktop/Draft%03d.pdf", sep = ""),
          width = w, height = h, pointsize = ps,
          onefile = FALSE, family = "sans", bg = "white",
          antialias = c("default", "none", "gray", "subpixel"))
    }

    if (device == 'svg') {
   		svg(filename = if(onefile) paste("/Users/", Benutzer, "/Desktop/Draft.svg",     sep = "") 
			       	      else paste("/Users/", Benutzer, "/Desktop/Draft%03d.svg", sep = ""),
          width = w, height = h, pointsize = ps,
          onefile = FALSE, family = "sans", bg = "white",
          antialias = c("default", "none", "gray", "subpixel"))
    }

}

    # Additional graphics parameters
	
	par(mfrow = c(r, c))		# devide graphics window, fill by col

	par(tck = 0.025, las = 1)	# inside tick marks (0.01)
	par(bty = "n" )	       		# omit box
	par(mgp = c(2.5, 0.25, 0))	# margin line for axis labels (default = c(3, 1, 0))
	par(mar = c(4, 5, 2, 1))	# default c(5, 4, 4, 2) + 1

}
