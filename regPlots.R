## Standard normal density
plot(dnorm, xlim=c(-3.5,3.5), xlab="y")

## Some wireframe plots of this density "smeared" along the line y=1+x
library(lattice)
xx <- seq(1, 5, length=200)
yy <- seq(-1,8, length=200)
grid <- expand.grid(x=xx,y=yy)
grid$z <- apply(grid, 1, function(x) { dnorm(x[2], 1+x[1], 1) })

## At near x=1
wireframe(z~x*y, data=grid[grid$x < 1.05,], xlim=c(1,5), cex=4)
## At near x=2
wireframe(z~x*y, data=grid[(grid$x < 2.025)&(grid$x>1.975),], xlim=c(1,5))
## At near x=3
wireframe(z~x*y, data=grid[(grid$x < 3.025)&(grid$x>2.975),], xlim=c(1,5))
## At near x=4
wireframe(z~x*y, data=grid[(grid$x < 4.025)&(grid$x>3.975),], xlim=c(1,5))
## At near x=5
wireframe(z~x*y, data=grid[(grid$x < 5.025)&(grid$x>4.975),], xlim=c(1,5))

## A full wireframe plot from x=1 to x=5 along y=1+x
xx <- seq(1,5, length=20)
yy <- seq(-1,8, length=200)
grid <- expand.grid(x=xx,y=yy)
grid$z <- apply(grid, 1, function(x) { dnorm(x[2], 1+x[1], 1) })
wireframe(z~x*y, data=grid, xlim=c(1,5))


## Get a finer grid to make a nicer level plot.
xx <- seq(-1,8, length=400)
yy <- seq(-1,8, length=400)
grid <- expand.grid(x=xx,y=yy)
grid$z <- apply(grid, 1, function(x) { dnorm(x[2], 1+x[1], 1) })

## A basic level plot of the "true" regression model y = 1 + x + N(0,1)
levelplot(z~x*y, grid, cuts=100, pretty=T, colorkey=F,
          col.regions=terrain.colors(400, 0.35))
## This lets you add stuff to the plot, using the functions llines and
## lpoints. This is one of the least obvious things about the lattice
## package.
trellis.focus("panel", 1, 1, highlight=F)

## The same level plot with the "true" regression line.
llines(c(-1,8), c(0, 9), col="black", lty=2)

## The "design" of the regression model from which data will be
## simulated is going to be 21 points equally spaced from 1 to 5.
x = seq(1,5,0.2)
n=length(x)

## This plot points out where the x design points are, which are fixed
## and the same for each simulated dataset.
lpoints(0~x, pch="|", col=1)

## The first set of simulated responses y. Try other seeds if you
## like.
set.seed(1)
y=1+x+rnorm(n,0,1)

## Add the points to the model. It can be pointed out here that these
## points come right from the model itself.
lpoints(y~x, pch=20)

## Here we can point out the (observed) "true" residuals, i.e.,
## distances from observed y valued and their true values on the true
## regression lines. These come from the 21 independent random
## variables.
for(i in 1:n) {
    llines(c(x[i],x[i]),
           c(1+x[i], y[i]), col="red", lwd=1.5)
}

## Of course in real life we don't know the "true" line. We can only
## estimate it from the data (unless you are a Bayesian or some other
## lunatic but forget them.) Here is a plot with the estimated
## regression line along with the "fitted" values, or y-hats - both in
## green.

lpoints(fitted.values(lm(y~x))~x, pch=20, col="green")
llines(c(1,5), as.vector(t(coefficients(lm(y~x))) %*%
                         matrix(c(1,1,1,5),2,2)), col="green")


## The plot is getting a bit busy! The last thing to add are the
## "estimated" residuals, which are the distances between the observed
## and fitted lines. Jiggle them by 0.02 to make the two lines visible
## next to each other.

for(i in 1:n) {
    llines(c(x[i]+0.02,x[i]+0.02),
           c(fitted.values(lm(y~x))[i], y[i]), lwd=1.5, col="green")
}


## To point out that the fitted values and estimated residuals (not as
## observed numbers, but as random variables) are *not* independent,
## let's clean up the plot and show what happens when one point is
## changed (of course it changes everything else).

levelplot(z~x*y, grid, cuts=100, pretty=T, colorkey=F,
          col.regions=terrain.colors(400, 0.35))
trellis.focus("panel", 1, 1, highlight=F)

lpoints(y~x, pch=20)

lpoints(fitted.values(lm(y~x))~x, pch=20, col="green")
llines(c(1,5), as.vector(t(coefficients(lm(y~x))) %*%
                         matrix(c(1,1,1,5),2,2)), col="green")
for(i in 1:n) {
    llines(c(x[i]+0.02,x[i]+0.02),
           c(fitted.values(lm(y~x))[i], y[i]), lty=1, col="green")
}

## Subtract, say, 3 from the last observed y value
y[n] = y[n] - 3

## Show the new point
lpoints(x[n],y[n], pch=20, col="orange")

## Of course, the fitted line changes
llines(c(1,5), as.vector(t(coefficients(lm(y~x))) %*%
                         matrix(c(1,1,1,5),2,2)), col="orange")

## Along with the fitted values and observed residuals.
for(i in 1:n) {
    llines(c(x[i],x[i]),
           c(fitted.values(lm(y~x))[i], y[i]), lty=1, col="orange")
}

## This shows why you really do have to go to the trouble of using the
## Hat matrix and so on to compute the variance of the fitted values
## and estimated residuals on the way to getting Studentized residuals
## etc.


## This next plot shows a bunch of randomly generated fitted
## regression lines superimposed on "The Model", which demonstrates
## that regression is more variable in the extremes and less near
## (x-bar, y-bar). This helps to understand the existence of the (x_0
## - x_bar)^2 term in the usual formula for confidence interval for
## the mean response etc.

levelplot(z~x*y, grid, cuts=100, pretty=T, colorkey=F,
          col.regions=terrain.colors(400, 0.35))
trellis.focus("panel", 1, 1, highlight=F)

for(i in 1:100) {
    y=1+x+rnorm(n,0,1)
    b1=(sum(x*y)-sum(x)*sum(y)/n)/(sum(x*x)-sum(x)^2/n)
    b0=mean(y)-b1*mean(x)
    llines(c(1,5), c(b0 + b1*1, b0 + b1*5))
}


