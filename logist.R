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


# ajustamos un modelo de regresión logística múltiple a los datos
modelo.glm <- glm(target~x+y,binomial,data=tmp)
predicho <- predict(modelo.glm,type="response")

# dibujamos la superficie ajustada
require(grDevices)
x <- seq(min(tmp$x),max(tmp$x),length=10)
y <- seq(min(tmp$x),max(tmp$x),length=10)

# la función f , predice las probabilidades de ser "naranja" para las secuencias x e y 
f <- function(x,y){p<-predict(modelo.glm,newdata=data.frame(x=x,y=y),type="response")}
# con outer se obtiene f para las combinaciones de de x e y , 
z <- outer(x,y,f) 
# dibujamos el gráfico en perspectiva, se puede jugar con los valores de phi, theta. se guarda en res, porque
# luego se utiliza para dibujar los puntos
res <- persp(x,y,z,theta =140 , phi = 15, expand = 0.5,ticktype="simple", col = "lightblue",zlab="Probabilidad")
# creamos dos vectores lógicos que nos dicen cuando es un punto azul y cuando naranja
azules <- tmp$target == "blue"
naranjas <- tmp$target == "orange"

# dibujamos los puntos azules y naranjas, y el segmento hasta la superficie ajustada en el gráfico creado por persp,
# por eso en trans3d se usa pmat=res
puntos1_abajo <- trans3d(tmp$x[azules], tmp$y[azules],0, pmat=res)
puntos2_abajo <- trans3d(tmp$x[naranjas], tmp$y[naranjas],predicho[naranjas], pmat=res)
puntos1_arriba <- trans3d(tmp$x[azules], tmp$y[azules],predicho[azules], pmat=res)
puntos2_arriba <- trans3d(tmp$x[naranjas], tmp$y[naranjas],1, pmat=res)
points(puntos1_abajo,col="blue")

segments(
x0 = puntos1_abajo$x,
y0 = puntos1_abajo$y,
x1 = puntos1_arriba$x,
y1 = puntos1_arriba$y,
lty = "solid",
col = "blue"
)
points(puntos2_arriba,col="orange")

segments(
x0 = puntos2_arriba$x,
y0 = puntos2_arriba$y,
x1 = puntos2_abajo$x,
y1 = puntos2_abajo$y,
lty = "solid",
col = "orange" 
)

# Si adhieren esto ... pueden jugar un poco con las perspectivas ... (Patricio Fuenmayor)

require(rgl)

open3d()
res <- surface3d(x, y, z, col="green", back="lines")

points3d(tmp$x[azules], tmp$y[azules],0, pmat=res)
points3d(tmp$x[naranjas], tmp$y[naranjas],predicho[naranjas],
pmat=res)
points3d(tmp$x[azules], tmp$y[azules],predicho[azules], pmat=res)
points3d(tmp$x[naranjas], tmp$y[naranjas],1, pmat=res) 

