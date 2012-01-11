gen.dat.2.3.3 <- function( n = 200, pct = 0.5 ){
	n.blue <- floor( n * pct )
	n.orange <- n - n.blue

	foo <- function( target, n, mean.x = 0, mean.y = 0 ){
		tmp.x <- sample( rnorm( 10, mean.x ), n, replace = T )
		tmp.y <- sample( rnorm( 10, mean.y ), n, replace = T )
		
		x <- rnorm( n, 0, 1/5 ) + tmp.x
		y <- rnorm( n, 0, 1/5 ) + tmp.y

		data.frame( target = target, x = x, y = y )
	}

	blue   <- foo( "blue",   n.blue,   mean.x = 1 )
	orange <- foo( "orange", n.orange, mean.y = 1 )

	return( rbind( blue, orange ) )
}

tmp <- gen.dat.2.3.3()
plot( tmp$x, tmp$y, col = tmp$target )

