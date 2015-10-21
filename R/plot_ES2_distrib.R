grid = seq(0,0.3,0.00001)

x = dlnorm(grid,log(2.81E-02),log(2.69))
#x = dlnorm(grid)
plot(grid,x,type="l",xlim=c(0,0.3))
grid(6)
abline(v=0.278,col="red",lty=2)
abline(v=2.81E-02,col="green",lty=2)
