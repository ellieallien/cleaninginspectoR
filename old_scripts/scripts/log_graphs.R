data <- reachR:::read.csv.auto.sep("./LBY_SMEB.csv")
data1 <- reachR:::

loggedv <- log(as.numeric(data$can.you.estimate.how.much.money.your.household.spends.on.food..in.total..in.a.typical.month.))  
    
names(data)
hist(as.numeric(data$can.you.estimate.how.much.money.your.household.spends.on.food..in.total..in.a.typical.month.), breaks = 50)
hist(loggedv, breaks = 20) 

plot(density(price),log="x", breaks = 12)
