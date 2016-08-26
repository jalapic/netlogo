## Example 2 - Sheep/Wolf Predation

library(rJava)
library(RNetLogo)
library(ggplot2)

setwd("C:/Program Files/NetLogo 5.3.1/app") #path where netlogo.jar file is stored - ymmv

nl.path <- getwd()
NLStart(nl.path)
model.path <- file.path("models", "Sample Models", "Biology","Wolf Sheep Predation.nlogo")
NLLoadModel(file.path(nl.path, model.path))


NLCommand("setup")
NLDoCommand(150, "go")

# set of turtles
animals <- NLGetAgentSet(c("xcor", "ycor"), "turtles") #can use this to get patches or links also

#extent of world coordinates
x.minmax <- NLReport("(list min-pxcor max-pxcor)")
y.minmax <- NLReport("(list min-pycor max-pycor)")

plot(animals, xlim = x.minmax, ylim = y.minmax, xlab = "x", ylab = "y")

#only keep turtles of specific type
sheep <- NLGetAgentSet(c("xcor", "ycor"),"sheep")
wolves <- NLGetAgentSet(c("xcor", "ycor"),"wolves")

plot(sheep, xlim = x.minmax, ylim = y.minmax, xlab = "x", ylab = "y", col="blue",pch=16)
par(new=T)
plot(wolves, xlim = x.minmax, ylim = y.minmax, xlab = "x", ylab = "y", col="red")


#getting data of total sheep/wolves by tick.

NLCommand("setup")
animal.df <- NLDoReport(150, "go", c("count sheep", "count wolves"),
                        as.data.frame = T, df.col.names = c("sheep","wolves"))

#reshape
animal.df <- data.frame(tick=1:150,reshape2::melt(animal.df))

ggplot(animal.df, aes(x=tick, y=value, group=variable, color=variable)) +
  geom_path(lwd=1) 



NLQuit()








### Run in parallel ----

library(parallel)
processors <- detectCores()
cl <- makeCluster(processors)



# the initialization function
prepro <- function(dummy, gui, nl.path, model.path) {
  library(RNetLogo)
  NLStart(nl.path, gui=gui)
  NLLoadModel(model.path)
}


simfun <- function(wr) {
    NLCommand("set wolf-reproduce ", wr, "setup")
    res <- NLDoReport(200, "go", c("count sheep", "count wolves"), 
                      as.data.frame = T, 
                      df.col.names = c("sheep","wolves"))
    ret <- data.frame(tick=1:200,reshape2::melt(res),wr=wr)
    return(ret)
}



# the quit function
postpro <- function(x) {
  NLQuit()
}


### Start netlogo
gui <- TRUE
nl.path <- Sys.getenv("NETLOGO_PATH", "C:/Program Files/NetLogo 5.3.1/app")
model.path <- file.path("models", "Sample Models", "Biology","Wolf Sheep Predation.nlogo")

# load NetLogo in each processor/core
invisible(parLapply(cl, 
                    1:processors, 
                    prepro, 
                    gui=gui,
                    nl.path=nl.path, 
                    model.path=model.path)
)


### Vary wolf-reproduce variable

wr <- seq(0, 10, .2)
result.par <- parSapply(cl, wr, simfun) # runs the simfun function over  clusters varying by density
result.par

#bind results together
results.par.dt <- data.table::rbindlist(apply(result.par,2, cbind.data.frame))



library(ggplot2)
ggplot(results.par.dt, aes(x=tick, y=value, group=wr, color=wr)) + 
  facet_wrap(~variable, scales = "free_y") +
  geom_path(lwd=1)

ggplot(results.par.dt, aes(x=tick, y=value, group=factor(variable), color=factor(variable))) + 
  facet_wrap(~wr, scales="free_y") +
  geom_path(lwd=1)




# Quit NetLogo in each processor/core
invisible(parLapply(cl, 1:processors, postpro))

# stop cluster
stopCluster(cl)







