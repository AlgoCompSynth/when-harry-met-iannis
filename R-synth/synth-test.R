# pure tone
synth(f=22050,d=5,cf=392,plot=TRUE, listen=TRUE)

# pure tone with sinusoid-like overall shape
synth(f=22050,d=5,cf=392,shape="sine",plot=TRUE,osc=TRUE, listen=TRUE)

# pure tones with am
synth(f=22050,d=5,cf=392,am=c(50,10),plot=TRUE,osc=TRUE, listen=TRUE)

# pure tone with +2000 Hz linear fm 
synth(f=22050,d=5,cf=392,fm=c(0,0,2000),plot=TRUE, listen=TRUE)

# pure tone with sinusoidal fm

# (maximum excursion of 1000 Hz, frequency of 10 Hz)
synth(f=22050,d=5,cf=392,fm=c(1000,10,0),plot=TRUE,wl=256,ovlp=75, listen=TRUE)

# pure tone with sinusoidal am

# (maximum excursion of 1000 Hz, frequency of 10 Hz)

# and linear fm (maximum excursion of 1000 Hz)
synth(f=22050,d=5,cf=392,fm=c(1000,10,1000),plot=TRUE,wl=256,ovlp=75, listen=TRUE)

# the same with am
synth(f=22050,d=5,cf=392,am=c(50,10),
    fm=c(1000,10,1000),plot=TRUE,wl=256,ovlp=75,osc=TRUE, listen=TRUE)

# the same with am and a triangular overall shape 
synth(f=22050,d=5,cf=392,shape="tria",am=c(50,10),
    fm=c(1000,10,1000),plot=TRUE,wl=256,ovlp=75,osc=TRUE, listen=TRUE)   

# more complex sound
F1<-synth(f=22050,cf=2000,d=5,fm=c(500,5,0))
F2<-synth(f=22050,a=0.8,cf=392,d=5,fm=c(500,5,0))
F3<-synth(f=22050,a=0.6,cf=6000,d=5,fm=c(500,5,0))
F4<-synth(f=22050,a=0.4,cf=8000,d=5,fm=c(500,5,0))
final1<-F1+F2+F3+F4
spectro(final1,f=22050,wl=512,ovlp=75,scale=FALSE, listen=TRUE)
