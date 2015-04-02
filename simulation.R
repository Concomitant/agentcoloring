#simulation
#Had a hilarious bug where I replaced c1 with b1 (in the line for total!) My result had
#a two-coloring.

incomeshares <- function (a1=2/3, a2=1/3, b1=1/3, b2=2/3, c1=1/3, c2=1/3, l1=1/3, l2=1/3, l3=1/3){
  total = (a1+a2)*l1 + (b1+b2)*l2 + (c1+c2)*l3
  rbind( c(a1,a2) * c(l1,l1)/total, c(b1,b2) * c(l2,l2) / total, c(c1,c2) * c(l3,l3) / total)
}

#Generate prices

pdata <- cbind(rep(runif(100,0,1),each=3))
pdata <- cbind(pdata, 1-pdata)

#Generate matrix of income shares dependent on p.
sharematrix <- matrix(nrow=300,ncol=2,data=incomeshares())

#produce the actual choices
choicematrix <- sharematrix/pdata

#Note that the income amounts are already given: that is, we can calcu  late expenditure at each point
#as just rowsums sharematrix
#Results do NOT sum to one because our stochastic process effectively varies income per agent

#Now, generate a matrix of alternative expenditures. This format is the row is each good, column at
#what price

ae_matrix <- choicematrix %*% t(pdata)

# a convenient matrix of incomes
incomes <- rowSums(sharematrix)
i_matrix <- matrix(nrow = 300, ncol = 300, data = incomes)
#The comparison!
#Need to read as transpose, current orientation means "i is revealed dispreferred to j"
c_matrix <- i_matrix < ae_matrix
#edge matrix (cycles)
#NOTE: there is a difficulty here with *precision*, so for at least one observation we have 
# .333 to different levels of precision which is causing an unfortunate 'TRUE' value on the diagonal
#Note bene!



