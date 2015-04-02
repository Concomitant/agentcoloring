library(MASS)

library(gtools)

agnu = function(x) {
  TRUE
}

ibeta = function(x,a,b){ pbeta(x,a,b)*beta(a,b) }

op = function(x, y, r) {
  for(i in r){
    if(i==x) {
      return(TRUE);
    }
    if(i==y){
      return(FALSE);
    }
  }
}

pibeta = function(x,a,b){
  ibeta(x,a,b)/beta(a,b)
}

sel = function (a, r) {
  for(i in r) {
    for(j in a) {
      if(i==j) {
        return(j);
      }
    }
  }
}

selvec = function(grid, prefs) {
  choices = vector(mode='numeric', length=dim(grid)[1])
  for(i in 1:length(choices)){
    choices[i] = sel(grid[i],prefs)
  }
  choices;
}

totalEvidence = function(choices, bowls, prefs) {
  for(i in 1:dim(prefs)[1]) {
    x[i] = admit(choices, bowls, prefs[i,])
  }
  x
}

MLE = function(evidence,n) {
  x = evidence
  (x+1)/(n+2)
}

admit = function(choices, bowls, pref) {
  x <- 0
  for(i in 1:length(choices)) {
    x <- x + ev(choices[i],bowls[i,],pref)
  }
  x;
}

ev = function(choice, bowl, pref) {
  if(choice == sel(bowl,pref)) {
    1;
  }  else {
    0;
  }
}

#We need to specify that the budget vector always has the maximum length,
#even though many of the entries might be zero. If there are 3 objects, a budget
#containing only 2 would be: (1,0,3), for example. This will necessitate care when
#generating the budget sets, simply a random draw is not good enough. They need to be sorted!

adjustmodel <- function(budget, model) {
  x <- rep(0, length(budget));
  n <- length(budget);
  consistent <- matrix(0,factorial(n),n);
  preferences <- permutations(n, n);
  
  for(i in seq(length(model)) ) {
    
    #This makes a binary vector, 1 if the choice of that element is 'consistent with' the relation
    
    consistent[i,] <- ifelse(budget == sel(budget, preferences[i,]),1,0);
    #Now we multiply the vector by the probability of that particular relation, and add it to x--increasing
    #the probability that that particular element is chosen. This is moving to a choice model from a preference
    #model
    x <- x + model[i]*consistent[i,];
  }
    
    #print(consistent);
    x;
}

#We generated this with a .7 probability for (RSB), .3 probability for (SBR)
data_r_b <- c(5,5,0);
data_r_s <- c(8,0,2);
data_s_b <- c(10,0,0);
model_r_b <- c(.7,.3,0);
model_r_s <- c(.7,0,.3);
model_s_b <- c(1,0,0);
#We threw out the models that can not *actually* explain the data. chisq.test throws an error because it tries to
#divide by the zero probabilities--we can still calculate with the nonzero probabilities
#Data for the first simulation
choices[1:5] = 1
choices[6:10] = 2
choices[11:18] = 1
choices[19:21] = 3
choices[21:30] = 1
MLE(totalEvidence(choices,budgets,preferences),30)
#                         RBS     RSB     BRS     BSR     SRB     SBR
# in paper calculated as:0.75000 0.75000 0.50000 0.18750 0.28125 0.03125, 
#We normalize Pr(RSB) and Pr(SBR)
#c(.75, .03125)/(.75+.03125), and provide an adjusted choice model fro those probabilities:
model = c(0, .96, 0, 0, 0, .04)
for(i in 1:4) {print(adjustmodel(unique(budgets)[i,],model))}
#The output needs to be parsed a little bit, the 0 probabilities are problematic otherwise(mentioned above).
#Similarly, an assignment of 1.0 probability to the *exact* value than the data is not a test of anything!
#So the test comes to a sum of 2 chi-squareds for the choce models over two pairs:
#55.1042 + 6.6667 is approximately 61.7709
#Conclusion

