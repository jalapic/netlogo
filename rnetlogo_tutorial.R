### Using RNetlogo

#References
# Jan C Thiele, 2014, R Marries NetLogo: Introduction to the RNetLogo Package, J. Stat Software. 58:2


#Installation
install.packages("RNetLogo") #should also install rJava - make sure your R and Java versions match.


#Starting
library(rJava)
library(RNetLogo)
setwd("C:/Program Files/NetLogo 5.3.1/app") #path where netlogo.jar file is stored - ymmv

#Options

#1. Run Netlogo with GUI - can interact with but can't run multiple runs.
#2. Run Netlogo without GUI - can't interact with but can run multiple runs.


######## 1. NetLogo with GUI

# start NetLogo with GUI by typing:
nl.path <- getwd()
NLStart(nl.path)  # a NetLogo window should open.  - can be used as any other window with exception it can't be closed by clicking.



### Example 1. Forrest Fire Model

# you could load this in normal NetLogo way i.e. File-->Open
# but in R...

model.path <- file.path("models", "Sample Models", "Earth Science", "Fire.nlogo")

NLLoadModel(file.path(nl.path, model.path))




# We can send messages to NetLogo command center:
NLCommand("print \"Hello NetLogo, I called you from R.\"")


# Using NetLogo commands in R
NLCommand("set density 77") # adjust slider
NLCommand("setup")  #set up 
NLCommand("go")  #increment by 1 tick


# Run multiple commands at once
density.in.r <- 88
NLCommand("set density ", density.in.r, "setup", "go")


# Run for multiple ticks
NLCommand("set density ", density.in.r, "setup")
NLDoCommand(100, "go")
NLReport("ticks") #this is shown and can be stored in R
NLDoCommand(100, "go")
totalticks  = NLReport("ticks") 
totalticks


# Store data after each tick
NLCommand("set density 60") # adjust slider to 60% density
NLCommand("setup")
burned <- NLDoReport(100, "go", "(burned-trees / initial-trees) * 100")
print(unlist(burned)) 
plot(1:100, unlist(burned))


# Note: No continuous-go function in R 
# use one of the loop functions (NLDoCommand, NLDoCommandWhile, NLDoReport, NLDoReportWhile)



### e.g. Run until all no more are burned.

NLCommand("setup")

#' while
#' 1st arg: "any? turtles" reporter is TRUE
#' 2nd arg: the procedure "go" will be executed.
#' 3rd arg: store the reporters "ticks" and "percent burned" (from calculation)
#' 4th arg: as a dataframe
#' 5th arg: define column names of dataframe

burned <- NLDoReportWhile("any? turtles", 
                          "go",
                          c("ticks", "(burned-trees / initial-trees) * 100"),
                          as.data.frame = TRUE, 
                          df.col.names = c("tick", "percentburned"))

plot(burned, type = "s")


### Percent burned over densities 57% - 60%
density <- c(57:60)
burned <- list()
for(i in seq_along(density)){
NLCommand("set density ", density[i], "setup")
burned[[i]] <- NLDoReportWhile("any? turtles", 
                          "go",
                          c("ticks", "(burned-trees / initial-trees) * 100"),
                          as.data.frame = TRUE, 
                          df.col.names = c("tick", "percentburned"))

}

burned.dt <- data.table::rbindlist(Map(cbind, burned, density=57:60))

library(ggplot2)
ggplot(burned.dt, aes(x=tick, y=percentburned, group=factor(density), 
                      color=factor(density))) +
  geom_path(lwd=1) + theme_bw()




### Run over all densities getting percent burned

#nb. move speed slider to "faster" extreme to make sims run faster.
#also untick "view updates" to increase speed.

# will go until burning complete
sim <- function(density) {
    NLCommand("set density ", density, "setup")
    NLDoCommandWhile("any? turtles", "go");
    ret <- NLReport("(burned-trees / initial-trees) * 100")
    return(ret)
 }

#will take a couple of minutes
d <- seq(1, 100, 1)
pb = sapply(d, sim)
plot(d, pb, xlab = "density", ylab = "percent burned")


# Replicate the simulation
rep.sim <- function(density, rep) {
  lapply(density, function(x) replicate(rep, sim(x)))
 }

#will take about 10 minutes !
d <- seq(55, 65, 1)
res <- rep.sim(d, 20)  #replicate sim 20 times for each d
boxplot(res, names = d, xlab = "density", ylab = "percent burned")



# To exit:
NLQuit()


#### Note - to reopen the GUI we need to restart the R session.




##### NetLogo Parallelization  ----

library(rJava)
library(RNetLogo)
setwd("C:/Program Files/NetLogo 5.3.1/app") #path where netlogo.jar file is stored - ymmv

# load the parallel package
library(parallel)

# detect the number of cores available
processors <- detectCores()
processors

# create a cluster
cl <- makeCluster(processors)
cl


### When using parallelization, everything has to be done for every processor separately.
# Therefore, make functions:

# the initialization function
prepro <- function(dummy, gui, nl.path, model.path) {
  library(RNetLogo)
  NLStart(nl.path, gui=gui)
  NLLoadModel(model.path)
}


simfun <- function(density) {
  
  sim <- function(density) {
    NLCommand("set density ", density, "setup")
    NLDoCommandWhile("any? turtles", "go");
    ret <- NLReport("(burned-trees / initial-trees) * 100")
    return(ret)
  }
  
  lapply(density, function(x) replicate(20, sim(x)))
}



# the quit function
postpro <- function(x) {
  NLQuit()
}


### Start Cluster
#run the initialization function in each processor, which will open as many NetLogo windows as we have processors


# set variables for the start up process
# adapt path appropriate (or set an environment variable NETLOGO_PATH)
gui <- TRUE
nl.path <- Sys.getenv("NETLOGO_PATH", "C:/Program Files/NetLogo 5.3.1/app")
model.path <- "models/Sample Models/Earth Science/Fire.nlogo"


# load NetLogo in each processor/core
invisible(parLapply(cl, 
                    1:processors, 
                    prepro, 
                    gui=gui,
                    nl.path=nl.path, 
                    model.path=model.path)
)


### Run over these 11 densities
d <- seq(55, 65, 1)
result.par <- parSapply(cl, d, simfun) # runs the simfunfunction over  clusters varying by density
result.par

burned.df <- data.frame(density=rep(55:65,each=20), pctburned=unlist(result.par))

library(ggplot2)
ggplot(burned.df, aes(x=factor(density), y=pctburned)) + geom_boxplot(alpha=.1) + geom_point()





# Quit NetLogo in each processor/core
invisible(parLapply(cl, 1:processors, postpro))

# stop cluster
stopCluster(cl)





