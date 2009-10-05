# load library
library(seewave)

s <- stereo(Sine(440,1),Sine(330,1))
play(s)
play(panorama(s,30))  # now right and left tones are closer to the center
play(panorama(s,10))  # now even closer
play(panorama(s,0))   # now both at the center, the same as setChannels(s,1)
play(panorama(s,-30)) # again wider, but both sides switched
play(panorama(s,-50)) # the same as mirror(s)

# play the data files
data(orni)
spectro(orni, f=22050, osc=TRUE, listen=TRUE)
savewav(orni, f=22050)

data(peewit)
spectro(peewit, f=22050, osc=TRUE, listen=TRUE)
savewav(peewit, f=22050)

data(sheep)
spectro(sheep, f=8000, osc=TRUE, listen=TRUE)
savewav(sheep, f=8000)

data(tico)
spectro(tico, f=22050, osc=TRUE, listen=TRUE)
savewav(tico, f=22050)

pellucens=loadSample("pellucens.wav")
sheep=loadSample("sheep.wav")
orni=loadSample("orni.wav")
peewit=loadSample("peewit.wav")
tico=loadSample("tico.wav")

# call the song of Oecanthus pellucens coming attached to Seewave
data(pellucens)
spectro(pellucens, f=11025, osc=TRUE, listen=TRUE)
savewav(pellucens, f=11025)

# store in a new object the sampling frequency to be repeatedly used
f<-11025

# cut a section and store it in a new object
natural<-cutw(pellucens, f=f, from=2.15, to=3.15)

# synthesis of a 2300 Hz chirp with a -315 Hz FM, a duration of 0.03 s and a sine-like shape amplitude envelope
s1<-synth(d=0.03, f=f, cf=2300, fm=c(0,0,-315), shape="sine", listen=TRUE)

# similar synthesis than s1 but at 2300*1.9 = 4370 Hz
s2<-synth(d=0.03, f=f, cf=2300*1.9, fm=c(0,0,-315), shape="sine", listen=TRUE)

# addition of s1 and 0.12*s2 to follow relative amplitude between the frequency bands; amplitude normalisation
s3<-s1+(0.12*s2); s4<-s3/max(s3)

# add a short silence period and repetition of the chirp
s5<-addsilw(s4, f=f, d=0.015); s6<-repw(s5, f=f, times=20)

# add a fade in effect with cosine-like shape
s7<-fadew(s6, f=f, din=0.25, shape="cos")

# paste natural and synthetic sounds in a new object
result1<-pastew(s7,pellucens,f=f)

# insert a 0.25 s silence period between natural and synthetic sounds
result2<-addsilw(result1, f=f, at=1, d=0.25)

# display a spectrogram with labels
spectro(result2, f=f, wl=256, ovlp=95, osc=TRUE, palette=rev.gray.colors.1)
mtext(c("natural","synthetic"), side=3, at=c(0.2,0.7), line=1.5, font=3)
listen(result2, f=f)

# pure tone
synth(f=22050,d=1,cf=4000,plot=TRUE, listen=TRUE)

# pure tone with sinusoid-like overall shape
synth(f=22050,d=1,cf=4000,shape="sine",plot=TRUE,osc=TRUE, listen=TRUE)

# pure tones with am
synth(f=22050,d=1,cf=4000,am=c(50,10),plot=TRUE,osc=TRUE, listen=TRUE)

# pure tone with +2000 Hz linear fm 
synth(f=22050,d=1,cf=4000,fm=c(0,0,2000),plot=TRUE, listen=TRUE)

# pure tone with sinusoidal fm
# (maximum excursion of 1000 Hz, frequency of 10 Hz)
synth(f=22050,d=1,cf=4000,fm=c(1000,10,0),plot=TRUE,wl=256,ovlp=75, listen=TRUE)

# pure tone with sinusoidal am
# (maximum excursion of 1000 Hz, frequency of 10 Hz)
# and linear fm (maximum excursion of 1000 Hz)
synth(f=22050,d=1,cf=4000,fm=c(1000,10,1000),plot=TRUE,wl=256,ovlp=75, listen=TRUE)

# the same with am
synth(f=22050,d=1,cf=4000,am=c(50,10),
    fm=c(1000,10,1000),plot=TRUE,wl=256,ovlp=75,osc=TRUE, listen=TRUE)

# the same with am and a triangular overall shape 
synth(f=22050,d=1,cf=4000,shape="tria",am=c(50,10),
    fm=c(1000,10,1000),plot=TRUE,wl=256,ovlp=75,osc=TRUE)   

# more complex sound
F1<-synth(f=22050,cf=2000,d=1,fm=c(500,5,0), listen=TRUE)
F2<-synth(f=22050,a=0.8,cf=4000,d=1,fm=c(500,5,0), listen=TRUE)
F3<-synth(f=22050,a=0.6,cf=6000,d=1,fm=c(500,5,0), listen=TRUE)
F4<-synth(f=22050,a=0.4,cf=8000,d=1,fm=c(500,5,0), listen=TRUE)
final1<-F1+F2+F3+F4
spectro(final1,f=22050,wl=512,ovlp=75,scale=FALSE, listen=TRUE)

data(pellucens)
pellu1<-cutw(pellucens,f=22050,from=0,to=1,plot=FALSE)

# without interpolation
zc(pellu1,f=22050,threshold=5,pch=20)

# with interpolation
zc(pellu1,f=22050,threshold=5,interpol=20,pch=20)

# a way to plot with a line and to filter low frequencies
pellu2<-zc(pellu1,f=22050,threshold=5,interpol=20,plot=FALSE)
pellu3<-na.omit(pellu2[,2])
pellu4<-pellu3[pellu3>3]
plot(x=seq(0,nrow(pellu1)/22050,length.out=length(pellu4)),
    y=pellu4,type="l",xlab="Time(s)",ylab="Frequency(kHz)")

# generate a sound with sine and linear frequency modulations
a<-synth(d=1, f=8000, cf=1500, fm=c(200,10,1000))

# plot on a single graphical device the instantaneous frequency and phase
op<-par(mfrow=c(2,1))
ifreq(a,f=8000,main="Instantaneous frequency")
ifreq(a,f=8000,phase=TRUE,main="Instantaneous phase")
par(op)

# meanspec demos
data(orni)

# compute the mean spectrum of the whole time wave
meanspec(orni,f=22050)

# compute the mean spectrum of a time wave section (from 0.32 s to 0.39 s)
meanspec(orni,f=22050,from=0.32,to=0.39)

# different window lengths
op<-par(mfrow=c(3,1))
meanspec(orni,f=22050,wl=256)
title("wl=256")
meanspec(orni,f=22050,wl=1024)
title("wl=1024")
meanspec(orni,f=22050,wl=4096)
title("wl=4096")
par(op)

# different overlap values (almost no effects here...)
op<-par(mfrow=c(3,1))
meanspec(orni,f=22050)
title("ovlp=0")
meanspec(orni,f=22050,ovlp=50)
title("ovlp=50")
meanspec(orni,f=22050,ovlp=95)
title("ovlp=95")
par(op)

# use of flim to zoom in
op<-par(mfrow=c(2,1))
meanspec(orni,f=22050)
title("zoom in")
meanspec(orni,f=22050,wl=512,flim=c(4,6))
par(op)

# comparaison of spectrum and mean spectrum
op<-par(mfrow=c(2,1))
spec(orni,f=22050)
title("spec()")
meanspec(orni,f=22050)
title("meanspec()")
par(op)

