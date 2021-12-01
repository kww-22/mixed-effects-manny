library(tidyverse)
set.seed(20211006)
y <- seq(1,10,10/200)
x <- y + rnorm(length(y), sd = 0.5)
y <- y + 1
m <- lm(y ~ 1 + x)
resids <- resid(m)
r_max <- which(resids == max(resids))

par(pty = 'm',
    mar = c(2,2,1,1))
plot(x,y,
     frame.plot = F,
     ylab = '',
     xlab = '',
     xlim = c(-1,10),
     ylim = c(-2,10),
     yaxt = 'n',
     xaxt = 'n',
     pch = 21,
     bg = alpha('black', 0.3),
     col = alpha('black', 0.3))
my_points <- curve(cbind(1,x) %*% coef(m), add = T, lwd = 3, lty = 2)
abline(v = 0, h = 0)
lines(c(my_points$x[75],my_points$x[75]),c(my_points$y[75],my_points$y[85]), lwd = 3)
lines(c(my_points$x[75],my_points$x[85]),c(my_points$y[85],my_points$y[85]), lwd = 3)
arrows(x[r_max],coef(m)[[1]]+coef(m)[[2]]*x[r_max],x[r_max],0.99*y[r_max], length = 0.1, lwd =3)
text(0.92*my_points$x[75],(my_points$y[75]+my_points$y[85])/2, expression(beta[1]), cex = 1.5)
text(0.97*my_points$x[75],(my_points$y[75]+my_points$y[85])/2,"{", cex = 2.8)
text(0.02*max(my_points$x),coef(m)[1]/2,"}", cex = 2.8)
text(0.055*max(my_points$x),coef(m)[1]/2, expression(beta[0]), cex = 1.5)
text((my_points$x[75]+my_points$x[85])/2,1.05*my_points$y[85],"1", cex = 1.3)
# text(0.25*max(my_points$x),0.75*max(my_points$y),bquote(y[i]==beta[0]+beta[1]*x[i]+epsilon[i]),adj=0.5, cex = 1.5)
text(0.95*x[r_max],(coef(m)[[1]]+coef(m)[[2]]*x[r_max]+y[r_max])/2,"{", cex = 2.7)
text(0.85*x[r_max],(coef(m)[[1]]+coef(m)[[2]]*x[r_max]+y[r_max])/2,bquote(epsilon[i]), cex = 1.5)
text(0.5*max(my_points$x),-0.75,"x")
text(-0.75,0.5*max(my_points$y),"y")
