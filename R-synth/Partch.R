rm(list=ls()); gc() # cleanup

# define the scales
Partch.scale.up <- c(
  1, 81/80, 33/32, 21/20, 16/15, 12/11, 
  11/10, 10/9, 9/8, 8/7, 7/6, 32/27, 
  6/5, 11/9, 5/4, 14/11, 9/7, 21/16, 
  4/3, 27/20, 11/8, 7/5, 10/7, 16/11, 
  40/27, 3/2, 32/21, 14/9, 11/7, 8/5, 
  18/11, 5/3, 27/16, 12/7, 7/4, 16/9, 
  9/5, 20/11, 11/6, 15/8, 40/21, 
  64/33, 160/81, 2
)
Partch.scale.down <- 1.0/Partch.scale.up

# convert ratios to cents and vice versa -- "pitch" uses semitones
cents <- function(ratio) {
  1200*log(ratio)/log(2)
}
ratio <- function(cents) {
  exp(cents*log(2)/1200)
}

library(seewave)

# get the sheep bleat
sheep <- loadSample("sheep.wav")

# get the middle half-second steady-state
sheep1 <- cutw(sheep, from=1, to=1.5, Sample=TRUE)
sheep.scale <- Silence(dur=1, rate=44100, bits=16, channels=1)

for (ratio in Partch.scale.up) { # ascending bleats
  bleat <- pitch(sheep1, 0.01*cents(ratio))
  play(bleat)
  sheep.scale <- appendSample(sheep.scale, bleat)
}
play(sheep.scale)
