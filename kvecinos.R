gen.dat.2.3.3 <- function( n = 200, pct = 0.5 ){
	n.blue <- floor( n * pct )
	n.orange <- n - n.blue

	foo <- function( target, n, mean.x = 0, mean.y = 0 ){
		indices <- sample( 10, n, replace = T )
		tmp.x <- rnorm( 10, mean.x )[ indices ]
		tmp.y <- rnorm( 10, mean.y )[ indices ]
		
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


require(class)
min.x<-min(tmp$x)
min.y<-min(tmp$y)
max.x<-max(tmp$x)
max.y<-max(tmp$y)
test.x<-seq(min.x,max.x,0.01)
test.y<-seq(min.y,max.y,0.01)
test<-expand.grid(test.x,test.y)
knn.res<-knn(train=tmp[,2:3],test=test,cl=tmp[,1],k=1)
dev.new()
plot(test,col=knn.res)
points(tmp$x, tmp$y, col = as.integer(tmp$target)+2)
