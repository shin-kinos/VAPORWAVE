
	library( ggplot2 )
	library( gganimate )
	library( gifski )
	library( ggimage )

	###########################################################
	# S e t  R a n d o m i s e d  V a l u e .
	###########################################################
	print( "R a n d o m i z a t i o n .", quote = FALSE )

	sign <- sample( c( "+", "-" ), 10 * 10, replace = TRUE )
	#print( sign )
	random_val <- rnorm( mean = 1, sd = 1, n = 10 * 10 )
	#print( random_val )

	transition_val <- data.frame( sign, random_val )
	#print( transition_val )

	transition_val$random_val <- ifelse( transition_val$sign == "-", -1 * abs( transition_val$random_val ), abs( transition_val$random_val ) )
	print( transition_val$random_val )

	###########################################################
	# S e t  I n i t i a l  V a l u e s .
	###########################################################
	value <- rep( 1 : 10, length = 100 )

	# T e m p o r a r y  V a l u e  F o r  R e c u r s i v e  C a l l .
	tmp <- value + transition_val$random_val

	value <- append( value, tmp )
	#print( value )

	###########################################################
	# T r a n s i t i o n .
	###########################################################
	for ( i in 1 : 48 ) {
		tmp <- tmp + transition_val$random_val
		value <- append( value, tmp )
	}

	#print( value )

	###########################################################
	# M a k e  D a t a  F r a m e  F o r  H e a t  M a p .
	###########################################################
	axis_x <- rep( 1 : 10, each = 10, length = 100 * 50 )
	#print( axis_x )

	axis_y <- rep( 1 : 10, length = 100 * 50 )
	#print( axis_y )

	# T r a n s i t i o n  N u m b e r .
	trans <- rep( 1 : 50, each = 100, length = 100 * 50 )

	dat <- data.frame( axis_x, axis_y, value, trans )
	#print( dat )

	###########################################################
	# g g p l o t .
	###########################################################
	plot <- ggplot( data = dat, aes( x = axis_x, y = axis_y, fill = value ) ) +
	        # H e a t M a p .
	        geom_tile( linetype = 1, lwd = 0.5, color = "white" ) +
	        scale_fill_gradient( high = "#0CECDD", low = "#FF00C8" ) +

	        # T e x t A n n o t a t i o n " V a p o r W a v e " .
	        annotate( "text", label = "V a p o r W a v e .", x = 5.5, y = 7.5, fontface = "italic", colour = "white"  , size = 15, alpha = 1.0 ) +
	        annotate( "text", label = "V a p o r W a v e .", x = 5.6, y = 7.4, fontface = "italic", colour = "magenta", size = 15, alpha = 0.3 ) +
	        annotate( "text", label = "V a p o r W a v e .", x = 5.4, y = 7.6, fontface = "italic", colour = "cyan"   , size = 15, alpha = 0.3 ) +

	        # I n s e r t A C o m p u t e r I m a g e .
	        geom_image( aes( x = 7.5, y = 3.0, image = "nobodyhere.png" ), size = 0.70 ) +
	        #annotate( "text", label = "nobody here", x = 8.0, y = 4.0, colour = "white", size = 1.0 ) +

	        # T r a n s i t i o n F o r G g a n i m a t e .
	        transition_time( trans )

	animate( plot , duration = 1, fps = 5, width = 500, height = 480 )
	anim_save( "VaporWave.gif" )
