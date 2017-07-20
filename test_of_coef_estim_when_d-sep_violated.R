
##Possibly swap this bit out with one that can read in a dag from a
##file.
gen.data <- function(n.obs=1000, coef1=runif(n=1, min=-1, max=1), coef2=runif(n=1, min=-1, max=1)){


    ## This graph contains 3 nodes, with a collider at x2.
    x1 <- rnorm(n.obs)
    x3 <- rnorm(n.obs)
    x2 <- rnorm(n.obs) + x1*coef1 + x3*coef2

    x4 <- rnorm(n.obs) + x3*coef2

##    x4 <- rnorm(n.obs)+x3*.001
    return(data.frame(x1, x2, x3, x4)) 
   
}

run.regression <- function(dataset=gen.data(), cor.mod=paste("x1~", "x2",sep=""),
                           false.mod=paste("x1~", "x2+x3",sep="")){

    
    model.correct <- lm(as.formula(cor.mod), data=dataset)

    ##For use with old regression.
    ##model.false <- lm(x1~x2+x3, data=dataset)

    model.false <- lm(as.formula(false.mod), data=dataset)
    
    return(list(model.correct=model.correct, model.false=model.false))
}

## calculates the difference between the non-dsep violating regression
## coeff/sd and the violating one.
compare.models <- function(list.models, var1="x2"){
   
    ##False model divided by true model regression coefficent between
    ##x1 and x2 minus incorrect model coef. between x1 and x2.
    regress.diff <- fetch.beta(model=list.models[[2]], var=var1)/fetch.beta(list.models[[1]], var1)
    ##False model divided by true model sd coef between x1 and x2.
    sd.diff <- fetch.sd(model=list.models[[2]], var=var1)/fetch.sd(model=list.models[[1]], var=var1)

    
    return(list(regress.diff=regress.diff, sd.diff=sd.diff))
}


fetch.beta <- function(model, var){
    return(model$coefficients[which(names(model$coefficients)==var)])
}

fetch.sd <- function(var, model){
    
    coef.mat <- data.frame(t(summary(model)$coefficients))

    ## Note, std. errors are storred on the second row of the matrix
    return(coef.mat[2, which(names(coef.mat)==var)])
}

##Coef of exactly one:

results.vec <- replicate(compare.models(run.regression(gen.data(10000, 1, 1))), n=1000)

pdf("simple_lin_results_box.pdf")

par(mfrow=c(2,2))

boxplot(unlist(results.vec[1,]), main="regression coef. diff\n coef both 1", xlab="incorrect model coef/correct model coef.")
abline(h=1, col="red")

boxplot((unlist(results.vec[2,])), main="sd coef. diff\n coef both 1", xlab="incorrect model sd/correct model sd.")
abline(h=1, col="red")


##Coef of exactly negative one:

results.vec <- replicate(compare.models(run.regression(gen.data(10000, -1, -1))), n=1000)

boxplot((unlist(results.vec[1,])), main="regression coef. diff\n coef both -1", xlab="incorrect model coef/correct model coef.")
abline(h=1, col="red")

boxplot((unlist(results.vec[2,])), main="sd coef. diff\n coef both -1", xlab="incorrect model sd/correct model sd.")
abline(h=1, col="red")




## Positive coef. only.


results.vec <- replicate(compare.models(run.regression(gen.data(10000, coef1=runif(n=1, min=0, max=1), coef2=runif(n=1, min=0, max=1)))), n=1000)


boxplot((unlist(results.vec[1,])), main="regression coef. diff,\n pos. coef only", xlab="incorrect model coef/correct model coef.")
abline(h=1, col="red")

boxplot((unlist(results.vec[2,])), main="sd coef. diff,\n pos. coef only", xlab="incorrect model sd/correct model sd.")
abline(h=1, col="red")


## Negitive coef. only.


results.vec <- replicate(compare.models(run.regression(gen.data(10000, coef1=runif(n=1, min=-1, max=0), coef2=runif(n=1, min=-1, max=0)))), n=1000)

##par(mfrow=c(2,1))

boxplot((unlist(results.vec[1,])), main="regression coef. diff,\n neg. coef only", xlab="incorrect model coef/correct model coef.")
abline(h=1, col="red")

boxplot((unlist(results.vec[2,])), main="sd coef. diff,\n neg. coef only", xlab="incorrect model sd/correct model sd.")
abline(h=1, col="red")


## Both Positive and negative equally likely only.


results.vec <- replicate(compare.models(run.regression(gen.data(10000, coef1=runif(n=1, min=-1, max=1), coef2=runif(n=1, min=-1, max=1)))), n=1000)

##par(mfrow=c(2,1))

boxplot((unlist(results.vec[1,])), main="regression coef. diff,\n both pos. and neg.", xlab="incorrect model coef/correct model coef.")
abline(h=1, col="red")

boxplot((unlist(results.vec[2,])), main="sd coef. diff,\n both pos. and neg.", xlab="incorrect model sd/correct model sd.")
abline(h=1, col="red")

dev.off()

##TODO: This example needs a different model than the previous
##functions. Need to create a d-sep situation where we use a predictor
##to predict values of an unrelated variable (e.g., in x->y,<-z->w,
##where z is not observed (so can't condition on). I.E a different
##generating graph.

compare.mse <- function(n.obs=1000, coef1=runif(n=1, min=-1, max=1), coef2=runif(n=1, min=-1, max=1)){
    data.train <- gen.data(n.obs, coef1, coef2)

    data.test <- gen.data(n.obs, coef1, coef2)
    
    models <- run.regression(data.train)

##    new.data <- data[sample(1:nrow(data), replace=TRUE),]
    
    model.1.mse <- mean((data.test$x1-predict(models$model.correct, newdata=data.test))^2)
    model.2.mse <- mean((data.test$x1-predict(models$model.false, newdata=data.test))^2)
    

    return(model.2.mse/model.1.mse)
}

summary(replicate(n=1000, compare.mse()))

##TODO: Note, even though the fit seems better with the erroneous
##variable, if the goal of the small area estimation is to guide
##policy (i.e., predict the effect of a change of
##policy/intervention), violating d-sep will cause a problem here even
##though the model seems to predict well in the non-intervention case.

##Also note: it appears that the closer the d-sep violating (and
##otherwise unrelated variable) is the more likely the mse is harmed by
##the almost indep variable being included.



##TODO: try changing the edges being compared to x1 and x3 (where
##there shouldn't be an edge connecting the two).
