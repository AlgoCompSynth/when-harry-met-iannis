chooseCRANmirror()
setRepositories()
update.packages(ask=FALSE)
install.packages(c(
  "Rcmdr",
  "animation",
  "audio",
  "rgl",
  "rpanel",
  "seewave",
  "signal",
  "sound",
  "tcltk2",
  "tuneR"
))

# execute R Commander to get the dependencies installed
library(Rcmdr)
