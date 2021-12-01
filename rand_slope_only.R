library(tidyverse)
iv <- seq(1,10,10/50)
y_1 <- 2 + 1.3*iv + rnorm(length(iv), sd = 0.5)
y_2 <- 2 + 0.7*iv + rnorm(length(iv), sd = 0.5)

m_1 <- lm(y_1 ~ 1 + iv)
m_2 <- lm(y_2 ~ 1 + iv)

par(pty = 'm',
    mar = c(2,2,1,1),
    family = 'serif')
plot(iv,y_1,
     frame.plot = F,
     ylab = '',
     xlab = '',
     xlim = c(-1,11),
     ylim = c(-2,11),
     yaxt = 'n',
     xaxt = 'n',
     pch = 21,
     bg = alpha('black', 0.3),
     col = alpha('red', 0.7))
points(iv,y_2,pch=22,bg = alpha('black', 0.3), col = alpha('blue', 0.7))
abline(v = 0, h = 0)
my_points_y1 <- curve(cbind(1,x) %*% coef(m_1), add = T, lwd = 3, lty = 2)
my_points_y2 <- curve(cbind(1,x) %*% coef(m_2), add = T, lwd = 3, lty = 2)

points(0.75*max(iv),0.25*max(y_2), pch = 21, bg = alpha('black', 0.3), col = alpha('red', 0.7))
points(0.75*max(iv),0.15*max(y_2),pch=22,bg = alpha('black', 0.3), col = alpha('blue', 0.7))
text(0.775*max(iv),0.25*max(y_2),'Participant 1',adj=0)
text(0.775*max(iv),0.15*max(y_2),'Participant 2',adj=0)

lines(c(my_points_y1$x[45],my_points_y1$x[45]),c(my_points_y1$y[45],my_points_y1$y[55]), lwd = 3)
lines(c(my_points_y1$x[45],my_points_y1$x[55]),c(my_points_y1$y[55],my_points_y1$y[55]), lwd = 3)
lines(c(my_points_y2$x[75],my_points_y2$x[75]),c(my_points_y2$y[75],my_points_y2$y[85]), lwd = 3)
lines(c(my_points_y2$x[75],my_points_y2$x[85]),c(my_points_y2$y[85],my_points_y2$y[85]), lwd = 3)
text(0.92*my_points_y1$x[45],(my_points_y1$y[45]+my_points_y1$y[55])/2,"{", cex = 3.8)
text(0.74*my_points_y1$x[45],(my_points_y1$y[45]+my_points_y1$y[55])/2, expression(beta["1(P1)"]), cex = 1.5)
text(0.97*my_points_y2$x[75],(my_points_y2$y[75]+my_points_y2$y[85])/2,"{", cex = 2.1)
text(0.88*my_points_y2$x[75],(my_points_y2$y[75]+my_points_y2$y[85])/2, expression(beta["1(P2)"]), cex = 1.5)
text(1,1.5,expression(beta["0(P2)"]), adj = 0, cex = 1.5)
arrows(0.95,1.5,0.05,0.975*coef(m_2)[[1]], lwd = 3, length = 0.15)
text(-0.75,3.5,expression(beta["0(P1)"]), adj = 0.5, cex = 1.5)
arrows(-0.75,3.1,-0.05,1.025*coef(m_1)[[1]], lwd = 3, length = 0.15)
text(0.5*max(my_points_y1$x),-0.75,"x")
text(-0.75,0.5*max(my_points_y1$y),"y")