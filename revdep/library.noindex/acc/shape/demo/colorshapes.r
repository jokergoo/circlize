# Demo for shapes
par(ask=TRUE)

##drapecol
 persp(volcano,theta = 135, phi = 30, col = drapecol(volcano),main="drapecol,femmecol")

##intpalette
 intpalette(c("white","black"),n=10)
 grey(seq(1,0,length.out=10))
 image(matrix(nrow=1,ncol=100,data=1:100),main="intpalette",
 col=intpalette(c("red","blue"),numcol=100))
 image(matrix(nrow=1,ncol=100,data=1:100),main="intpalette",
 col=intpalette(c("red","blue","yellow"),numcol=100))
 image(matrix(nrow=1,ncol=100,data=1:100),main="intpalette", 
 col=intpalette(c("red","blue","yellow","green"),numcol=100))
 image(matrix(nrow=1,ncol=100,data=1:100),main="intpalette", 
 col=intpalette(c("red","blue","yellow","green","black"),numcol=100))


##shadepalette
 shadepalette(n=10,"white","black")
 image(matrix(nrow=1,ncol=100,data=1:100),col=shadepalette(100,"red","blue"),
       main="shadepalette")   
# rotatexy
#----------------
xy <- matrix(ncol=2,data=c(1:5,rep(1,5)))
plot(xy,xlim=c(-6,6),ylim=c(-6,6),type="b",pch=16,main="rotatexy",col=1)
for ( i in 1:5) points(rotatexy(xy,mid=c(0,0),angle=60*i),col=i+1,type="b",pch=16)
points(0,0,cex=2,pch=22,bg="black")

legend("topright",legend=60*(0:5),col=1:6,pch=16,title="angle")
legend("topleft",legend="midpoint",pt.bg="black",pt.cex=2,pch=22,box.lty=0)

x <- seq(0,2*pi,pi/100)
y <- sin(x)
cols <- intpalette(c("blue","green","yellow","red"),n=250)
cols <- c(cols,rev(cols))
plot(x,y,type="l",ylim=c(-3,3),main="rotatexy",col=cols[1],lwd=2,xlim=c(-1,7))
for (i in 2:500) lines(rotatexy(cbind(x,y),angle=0.36*i),col=cols[i],lwd=2)

x <- seq(0,2*pi,pi/100)
y <- sin(x*2)

cols <- intpalette(c("red","yellow","black"),n=250)
cols <- c(cols,rev(cols))
plot(x,y,type="l",ylim=c(-4,5),main="rotatexy, asp=TRUE",col=cols[1],lwd=2,xlim=c(-1,7))
for (i in 2:500) lines(rotatexy(cbind(x,y),angle=0.36*i,asp=TRUE),col=cols[i],lwd=2)


cols <- femmecol(1000)
plot(x,y,xlim=c(-1,1),ylim=c(-1,1),main="rotatexy",col=cols[1],type="n")
for (i in 2:1000) {xy<-rotatexy(c(0,1),angle=0.36*i, mid=c(0,0));
                    points(xy[1],xy[2],col=cols[i],pch=".",cex=2)}

# ellipses
#----------------
emptyplot(c(-1.5,1.5))
cols <- rainbow(10)
SS   <- rev(seq(0.1,1,0.1))
plotellipse(rx=1.5,ry=0.6)
plotellipse(rx=1.2,ry=0.4)
for (i in 1:length(SS)) plotellipse(1,SS[i],col=cols[i],type="n")
title("plotellipse")
plotellipse(rx=1.5,ry=0.6,angle=0,from=pi,to=2*pi,arrow=TRUE,arr.pos=0.5)
plotellipse(rx=1.2,ry=0.4,from=pi,to=2*pi)

# 
emptyplot(c(-1.,1.))
col  <- shadepalette(endcol="darkblue",n=100)
filledellipse (rx1=1,ry1=0.8,col=col,lcol="black")
plotellipse(rx=1,ry=0.35 ,angle=0,from=pi,to=2*pi,lwd=1,mid=c(0,0.))
plotellipse(rx=1.1,ry=0.6 ,angle=0,from=1.28*pi,to=1.72*pi,lwd=1,mid=c(0,-0.1))
plotellipse(rx=0.9 ,ry=0.32,angle=0,from=pi,to=2*pi,lwd=1,mid=c(0,0.35))
plotellipse(rx=0.65,ry=0.25,angle=0,from=pi,to=2*pi,lwd=1,mid=c(0,0.6))
plotellipse(rx=0.3 ,ry=0.1,angle=0,from=pi,to=2*pi,lwd=1,mid=c(0,0.75))

plotellipse(rx=0.8,ry=0.0,angle=90,from=pi,to=2*pi,lwd=1)
plotellipse(rx=0.8,ry=0.35,angle=90,from=pi,to=2*pi,lwd=1)
plotellipse(rx=0.8,ry=0.6,angle=90,from=pi,to=2*pi,lwd=1)
plotellipse(rx=0.8,ry=0.8,angle=90,from=pi,to=2*pi,lwd=1)

plotellipse(rx=0.8,ry=0.35,angle=-90,from=pi,to=2*pi,lwd=1)
plotellipse(rx=0.8,ry=0.6,angle=-90,from=pi,to=2*pi,lwd=1)
plotellipse(rx=0.8,ry=0.8,angle=-90,from=pi,to=2*pi,lwd=1)
title("plotellipse, filledellipse")

#
emptyplot(c(-1,1))
col  <- c(rev(greycol(n=50)),greycol(50))
filledellipse(rx1=1,rx2=0.5,dr=0.01,col=col)
title("filledellipse")

#
emptyplot(c(-1,1),c(-1,1))
filledellipse(col=col)
title("filledellipse")

# 
color <-graycol(n=50)
dr    <- 0.05
emptyplot(xlim=c(-2,2),ylim=c(-2,2),col=color[length(color)])

filledellipse(rx1=1,mid=c(1,1)  ,col=shadepalette(endcol="darkblue") ,dr=dr) 
filledellipse(rx1=1,mid=c(-1,-1),col=shadepalette(endcol="darkgreen"),dr=dr)
filledellipse(rx1=1,mid=c(1,-1) ,col=shadepalette(endcol="darkred")   ,dr=dr)
filledellipse(rx1=1,mid=c(-1,1) ,col=shadepalette(endcol="darkviolet"),dr=dr)
filledellipse(rx1=1,col=color,dr=dr)
title("filledellipse")

#
color <-gray(seq(1,0.3,length.out=50))
emptyplot(xlim=c(-2,2),ylim=c(-2,2),col=color[length(color)])

filledellipse(rx1=2,ry1=0.4,col=color,angle=45,dr=dr)
filledellipse(rx1=2,ry1=0.4,col=color,angle=-45,dr=dr)
filledellipse(rx1=2,ry1=0.4,col=color,angle=0,dr=dr)
filledellipse(rx1=2,ry1=0.4,col=color,angle=90,dr=dr)
title("filledellipse")

example(getellipse)

# cylinder
#----------------
example(cylindersegment)

example(filledcylinder)
# shape
#----------------
emptyplot(c(-1.,1.))
xy <- getellipse(rx=1,ry=0.4)
filledshape(xyouter=xy,xyinner=c(0,1.),col=femmecol(100))
title("filledshape")


example(filledshape)

emptyplot(col="darkgrey",main="filledshape")
filledshape(matrix(nc=2,runif(100)),col=shadepalette(200,"darkred","darkblue"))

# rectangle
#----------------
example(filledrectangle)

# multigonal
#----------------

color <-shadepalette(grey(0.3),"blue",n=50)
emptyplot(c(-1,1))
filledmultigonal(rx=0.25,ry=0.25,col=shadepalette(grey(0.3),"blue",n=50),nr=3,mid=c(0,0),angle=0)
filledmultigonal(rx=0.25,ry=0.25,col=shadepalette(grey(0.3),"darkgreen",n=50),nr=4,mid=c(0.5,0.5),angle=90)
filledmultigonal(rx=0.25,ry=0.25,col=shadepalette(grey(0.3),"orange",n=50),nr=5,mid=c(-0.5,-0.5),angle=-90)
filledmultigonal(rx=0.25,ry=0.25,col="black",nr=6,mid=c(0.5,-0.5),angle=180)
filledmultigonal(rx=0.25,ry=0.25,col="white",lcol="black",nr=7,mid=c(-0.5,0.5),angle=270)
title("filledmultigonal")

example(filledmultigonal)


# Pipes
xlim <- ylim <- c(-20,20)
emptyplot(xlim)  
box()

color  <- graycol(n=5)
color  <- c(rev(color),color)
maxlen <- 15
maxrad <- 1.0

dr    <- 0.01
angle <- 0
mid   <- c(0,0)
ry    <- 0

numpipes <- 100
for (i in 1:numpipes)
{

rnd    <- runif(1)       # jump length
length <- maxlen*rnd
newpos <- rotatexy(mid+c(length+2*maxrad,0),angle,mid)

                         # check if within boundaries
if (! (newpos[1] > xlim[1] & newpos[1] < xlim[2] & 
       newpos[2] > ylim[1] & newpos[2] < ylim[2])) length <- 0

                         # rectangle
mid <- rotatexy(mid+c(length/2,0),angle,mid)             
filledrectangle(wy=maxrad,wx=length,col=color,angle=angle,mid=mid)
mid <- rotatexy(mid+c(length/2,0),angle,mid)
                          
rnd    <- runif(1)        # new angle
                                dangle <- 0
if (rnd <= 0.3333)              dangle <- 90
if (rnd >0.3333 & rnd < 0.6666) dangle <- - 90

piangle <- angle / 180 * pi

if (dangle == 90)  # left turn
 {
 mid <- rotatexy(mid+c(0,maxrad/2),angle,mid)
 filledcircle(r1=maxrad,r2=ry,col=color,mid=mid,dr=dr,from= piangle-pi/2,to= piangle)
 mid <- rotatexy(mid+c(maxrad/2,0),angle,mid)
 } 

if (dangle ==-90)  # right turn 
{
 mid <- rotatexy(mid-c(0,maxrad/2),angle,mid)
 filledcircle(r1=maxrad,r2=ry,col=color,mid=mid,dr=dr,from= piangle,to= piangle+pi/2)
 mid <- rotatexy(mid+c(maxrad/2,0),angle,mid)
}
angle <- angle + dangle 
if (angle >= 360) angle <- angle - 360
if (angle < 0)    angle <- angle + 360

}
title("filledrectangle,filledcircle")


# Arrowhead
#----------------

emptyplot(c(0,1))

Arrowhead(x0=runif(10),y0=runif(10),angle=runif(10)*360,
arr.length=0.4,arr.type="circle",arr.col="green")

Arrowhead(x0=runif(10),y0=runif(10),angle=runif(10)*360,
arr.length=0.6,arr.type="curved",arr.col="red")

Arrowhead(x0=runif(10),y0=runif(10),angle=runif(10)*360,
arr.length=0.6,arr.type="triangle",arr.col="blue")

title("Arrowhead")

# Arrows
#----------------
example(Arrows)
xlim <- c(-5 ,5)
ylim <- c(-10,10)
plot(0,type="n",xlim=xlim,ylim=ylim)
x0<-runif(100,xlim[1],xlim[2])
y0<-runif(100,ylim[1],ylim[2])
x1<-x0+runif(100,-1,1)
y1<-y0+runif(100,-1,1)
size <- 0.6 
ang  <- runif(100,-360,360)   
Arrows(x0,y0,x1,y1,arr.length=size,code=2,arr.type="curved",arr.col="white")
title("Arrows")

plot(0,type="n",xlim=xlim,ylim=ylim)
Arrows(x0,y0,x1,y1,arr.length=size,code=2,arr.type="triangle",arr.col="yellow")
title("Arrows")

plot(0,type="n",xlim=xlim,ylim=ylim)
x0<-runif(100,xlim[1],xlim[2])
y0<-runif(100,ylim[1],ylim[2])
x1<-x0+runif(100,-1,1)
y1<-y0+runif(100,-1,1)
size <- 0.6 
ang  <- runif(100,-360,360)   
Arrows(x0,y0,x1,y1,arr.length=size,code=2,arr.type="circle",arr.col="darkblue")
title("Arrows")

# Lotka-Volterra competition model
r1    <- 3              # parameters
r2    <- 2
K1    <- 1.5
K2    <- 2
alf12 <- 1
alf21 <- 2

Ax  <- c(0,K2/alf21)
Ay  <- K2 - alf21* Ax
By  <- c(0,K1/alf12)
Bx  <- K1 - alf12* By
xlim   <- range(c(Ax, Bx))
ylim   <- range(c(Ay, By))

plot  (x=Ax,y=Ay, type="l",lwd=3,   # 1st isocline
     main="Arrows",sub="Model from Soetaert and Herman, 2008. book",
       xlab="N1",ylab="N2",xlim=xlim,ylim=ylim)
lines (Bx,By, lty=2,lwd=3)            # 2nd isocline


gx <- seq(0,1.5,len=40)
gy <- seq(0,2,len=40)
N  <- as.matrix(expand.grid(x=gx,y=gy))

dN1 <- r1*N[,1]*(1-(N[,1]+alf12* N[,2])/K1)
dN2 <- r2*N[,2]*(1-(N[,2]+alf21* N[,1])/K2)
dt  <- 0.0001
Arrows(N[,1],N[,2],N[,1]+dt*dN1,N[,2]+dt*dN2,arr.len=0.15, lcol=grey(0.4),arr.type="triangle")
points(x=c(0,0,1.5,0.5),y=c(0,2,0,1),pch=22,cex=2,bg=c("white","black","black","grey"))

example(colorlegend)

# Scientific example of use of filledellipse: uses values
BW     <- 10           # mmol/m3,       oxygen concentration in surrounding water
Da     <- 0.5          # cm2/d          effective diffusion coefficient in organism
R      <- 0.005        # cm             radius of organism 
Q      <- 250000       # nM/cm3/d       oxygen consumption rate per volume per day

# concentration in spherical organism body
sphere   <- function(Da,Q,BW,R,r)  BW+Q/(6*Da)*(r^2-R^2)

# distance in organism body
color <- femmecol(100)

zlim=c(0,BW)
emptyplot(c(-3.3,3.3),col=color[length(color)],main="Oxygen concentration in spherical organism",
sub="Model from Soetaert and Herman, 2008. book")
R      <- 0.005
values <- cbind(rr<-seq(0,R,length=50),sphere(Da,Q,BW,R,rr))

filledellipse(rx1=R*100,mid=c(-2,2),values=values,zlim=zlim,col=color,lcol="black",lwd=1)

R      <- 0.0075
values <- cbind(rr<-seq(0,R,length=50),sphere(Da,Q,BW,R,rr))
filledellipse(rx1=R*100,mid=c(0,2),values=values,zlim=zlim,col=color,lcol="black",lwd=1)

R      <- 0.010
values <- cbind(rr<-seq(0,R,length=50),sphere(Da,Q,BW,R,rr))
filledellipse(rx1=R*100,mid=c(2,2),values=values,zlim=zlim,col=color,lcol="black",lwd=1)
          
R      <- 0.015
values <- cbind(rr<-seq(0,R,length=100),sphere(Da,Q,BW,R,rr))
filledellipse(rx1=R*100,mid=c(-1,-1),values=values,zlim=zlim,col=color,lcol="black",lwd=1)

segments(-1.5,-3,-0.5,-3,lwd=2,col="black")
text(-1,-2.8,"10 microm")

colorlegend(zlim=zlim,posy=c(0.05,0.5),posx=c(0.7,0.73),font=2,
main="micromol/l",main.cex=1.2)

