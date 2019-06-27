library(gamm4)

# Make up some data
n <- 10
j <- 3
alpha <- rnorm(n)
ID <- rep(1:n, j)
x <- runif(n*j)
y <- rnorm(n*j, mean=x + alpha[ID], sd=0.5)
df <- data.frame(y=y, x=x, ID=factor(ID))

x <- runif(100)
fac <- factor(sample(1:20,100,replace=TRUE))
eta <- x^2*3 + as.numeric(fac)/20
y <- rpois(100,exp(eta))
fit <- gamm4(y~s(x),family=poisson,random=~(1|fac))
visreg(fit$gam, "x")
visreg(fit$gam, "x", type="contrast")
