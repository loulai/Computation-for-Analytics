import soundfile as sf 
import sounddevice as sd 

# Program makes sound softer

kiss, samplerate = sf.read('sounds/Kiss.aiff') #load Kiss.aiff into kiss variable
kiss = kiss * .1 # reduce amplitude (by dividing by 10) to make suiter
sd.play(kiss, samplerate) # start playing musix
sd.wait() # wait until music finishes before exiting