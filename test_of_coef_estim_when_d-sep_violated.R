

gen.data <- function(n.obs=1000, coef1=runif(n=1, min=-1, max=1), coef2=runif(n=1, min=-1, max=1)){


    ## This graph contains 3 nodes, with a collider at x2.
    x1 <- rnorm(n.obs)
    x3 <- rnorm(n.obs)
    x2 <- rnorm(n.obs) + x1*coef1 + x3*coef2

    return(data.frame(x1, x2, x3)) 
   
}

run.regression <- function(dataset=gen.data()){

    model.correct <- lm(x1~x2, data=dataset)
    model.false <- lm(x1~x2+x3, data=dataset)

    return(list(model.correct=model.correct, model.false=model.false))
}

## calculates the  difference between the non-dsep violating regression coeff/sd and the violating one.
compare.models <- function(list.models){

    ##True model regression coefficent between x1 and x2 minus incorrect model coef. between x1 and x2.
    regress.diff <- (list.models[[1]]$coefficients[2]-list.models[[2]]$coefficients[2])

    ##True model sd coef between x1 and x2 minus incorrect model sd coef. between x1 and x2.
    sd.diff <- (summary(list.models[[1]])$coefficients[2,2]-summary(list.models[[2]])$coefficients[3,2])
    
    return(list(regress.diff=regress.diff, sd.diff=sd.diff))
}

##Coef of exactly one:

results.vec <- replicate(compare.models(run.regression(gen.data(10000, 1, 1))), n=1000)

pdf("simple_lin_results.pdf")

par(mfrow=c(2,2))

hist((unlist(results.vec[1,])), main="regression coef. diff\n coef both 1")
abline(v=0, col="red")

hist((unlist(results.vec[2,])), main="sd coef. diff\n coef both 1")
abline(v=0, col="red")




## Positive coef. only.


results.vec <- replicate(compare.models(run.regression(gen.data(10000, coef1=runif(n=1, min=0, max=1), coef2=runif(n=1, min=0, max=1)))), n=1000)


hist((unlist(results.vec[1,])), main="regression coef. diff,\n pos. coef only", xlab="diff. between correct and\n incorrect model coef.")
abline(v=0, col="red")

hist((unlist(results.vec[2,])), main="sd coef. diff", xlab="diff. between correct and\n incorrect model coef.")
abline(v=0, col="red")


## Negitive coef. only.


results.vec <- replicate(compare.models(run.regression(gen.data(10000, coef1=runif(n=1, min=-1, max=0), coef2=runif(n=1, min=-1, max=0)))), n=1000)

##par(mfrow=c(2,1))

hist((unlist(results.vec[1,])), main="regression coef. diff,\n neg. coef only", xlab="diff. between correct and\n incorrect model coef.")
abline(v=0, col="red")

hist((unlist(results.vec[2,])), main="sd coef. diff,\n neg. coef only", xlab="diff. between correct and\n incorrect model coef.")
abline(v=0, col="red")


## Both Positive and negative equally likely only.


results.vec <- replicate(compare.models(run.regression(gen.data(10000, coef1=runif(n=1, min=-1, max=1), coef2=runif(n=1, min=-1, max=1)))), n=1000)

##par(mfrow=c(2,1))

hist((unlist(results.vec[1,])), main="regression coef. diff,\n both pos. and neg.", xlab="diff. between correct and\n incorrect model coef.")
abline(v=0, col="red")

hist((unlist(results.vec[2,])), main="sd coef. diff,\n both pos. and neg.", xlab="diff. between correct and\n incorrect model coef.")
abline(v=0, col="red")

dev.off()
