# install.packages("devtools")
 library("devtools")
# devtools::install_github("klutometis/roxygen")
# library(roxygen2)

#setwd("z:/")
#create("rPerfFunc2")

setwd("z:/rPerfFunc")
document()

install_github('rPerfFunc','cquigley')

#cquigley@CQ MINGW64 ~
#  $ git init Z:/git/rPerfFunc
#Reinitialized existing Git repository in Z:/git/rPerfFunc/.git/
  


setwd("z:/git")

library(devtools)
# setup testing framework
use_testthat()

# update NAMESPACE and other docs
document()

# run tests
test()


create("z:/git", check = FALSE,rstudio = TRUE)


setwd("z:")

namespace_roclet()



library(roxygen2)
setwd("z:/git")
roxygenize("rPerfFunc")

#$ git push origin master -f
