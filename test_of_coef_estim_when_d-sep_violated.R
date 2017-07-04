

gen.data <- function(n.obs=1000){


    ## This graph contains 3 nodes, with a collider at x2.
    x1 <- rnorm(n.obs)
    x3 <- rnorm(n.obs)
    x2 <- rnorm(n.obs) + x1 + x3

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


results.vec <- replicate(compare.models(run.regression(gen.data(10000))), n=1000)

par(mfrow=c(2,1))

hist((unlist(results.vec[1,])), main="regression coef. diff")
abline(v=0, col="red")

hist((unlist(results.vec[2,])), main="sd coef. diff")
abline(v=0, col="red")

