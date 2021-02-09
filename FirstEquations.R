rm(list=ls())

library(tidyverse)


t <- seq(-pi,pi,0.001)
x <- 16*0.25*(3*sin(t) - sin(3*t))
y <- 13*cos(t) - 5*cos(2*t) - cos(4*t)


# Letters -----------------------------------------------------------------

# Plot I----

point1 <- 0
point2 <- 0
I <- tibble(point1, point2)

IPlot <- I %>% 
  ggplot(aes(point1, point2)) +
  theme_classic() +
  geom_errorbar(ymin=-1, ymax=1, width=0.2, size=1, color="blue") +
  expand_limits(x=c(-.5,.5), y=c(-1.5,1.5)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
IPlot


# Plot L ------------------------------------------------------------------

x2 <- seq(1,100,.1)
y2 <- 1/x2
L <- tibble(x2,y2)

LPlot <- L %>% 
  ggplot(aes(x2,y2)) +
  theme_classic() +
  geom_line() +
  expand_limits(x=c(-90,200), y=c(0,1.1)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
LPlot


# Plot O ------------------------------------------------------------------

x3 <- seq(-3, 3, 0.1)
y3.1 <- sqrt(9-(x3^2))
y3.2 <- -sqrt(9-x3^2)
O <- tibble(x3, y3.1, y3.2)

OPlot <- O %>% 
  ggplot(aes(x3 ,y3.1)) +
  geom_line() +
  ggplot(aes(x3, y3.2)) +
  geom_line()
OPlot

# Hearts ------------------------------------------------------------------

Heart1 <- tibble(x,y)

Heart1Plot <- Heart1 %>% 
  ggplot(aes(x,y)) +
  theme_classic() +
  geom_line(color = "pink") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  coord_polar(start = pi)
Heart1Plot


r <- 2 - 2*sin(t) + sin(t)*(sqrt(abs(cos(t))))/(sin(t)+1.4)

Heart2 <- tibble(t,r)

Heart2Plot <- Heart2 %>% 
  ggplot(aes(t,r)) +
  theme_classic() +
  geom_polygon(fill = "pink") +
  coord_polar(start = pi/2) +
  scale_fill_manual(values = "pink") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
Heart2Plot

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
# https://ggplot2.tidyverse.org/reference/coord_polar.html
# https://stackoverflow.com/questions/39773933/how-to-get-a-really-periodic-polar-surface-plot-with-ggplot
# http://sape.inf.usi.ch/quick-reference/ggplot2/geom_errorbar
# https://www.lenfisherscience.com/36-all-you-need-is-love-mathematical-style/
# http://www.sthda.com/english/wiki/ggplot2-axis-scales-and-transformations