rm(list=ls())

library(tidyverse)





# Proving Functionality ---------------------------------------------------



n <- 50
x <- -n:n
y <- x^3
plot(x, y)
lines(spline(x, y))
lines(spline(x, y, n = 201), col = 2)
z <- lines(spline(x,y))
plot(x,z)

Cubes <- tibble(x,y)

Cubic <- Cubes %>% 
  ggplot(aes(x, y)) +
  geom_line()
Cubic

# Test Code ---------------------------------------------------------------


op <- par(mfrow = c(2,1), mgp = c(2,.8,0), mar = .1+c(3,3,3,1))
n <- 9
x <- 1:n
y <- rnorm(n)
plot(x, y, main = paste("spline[fun](.) through",n,"points"))
lines(spline(x, y))
lines(spline(x, y, n = 201), col = 2)

y <- (x-6)^2
plot(x, y, main = "spline(.) -- 3 methods")
lines(spline(x, y, n = 201), col = 2)
lines(spline(x, y, n = 201, method = "natural"), col = 3)
lines(spline(x, y, n = 201, method = "periodic"), col = 4)
legend(6,25, c("fmm","natural","periodic"), col=2:4, lty=1)

f <- splinefun(x, y)
ls(envir = environment(f))
splinecoef <- eval(expression(z), envir = environment(f))
curve(f(x), 1, 10, col = "green", lwd = 1.5)
points(splinecoef, col = "purple", cex = 2)
par(op)


par(mfrow=c(2,2))
curve(x^3-3*x, -2, 2)
curve(x^2-2, add = TRUE, col = "violet")

curve(sin, -pi, 3*pi)
plot(cos)
chippy <- function(x) sin(cos(x)*exp(-x/2))
curve(chippy, -8, 7, n=2001)

for(ll in c("","x","y","xy"))
  curve(log(1+x), 1,100, log=ll, sub=paste("log=",ll))


# References --------------------------------------------------------------


# https://www.math.ucla.edu/~anderson/rw1001/library/base/html/curve.html
# https://www.math.ucla.edu/~anderson/rw1001/library/base/html/splinefun.html
# https://www.math.ucla.edu/~anderson/rw1001/library/base/html/par.html
