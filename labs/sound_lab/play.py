# Program plays a short soundclip
# Note: program hardcodes sound clip location

import soundfile as sf 
import sounddevice as sd 

kiss, samplerate = sf.read("sounds/Kiss.aiff") # load soundfile into kiss variable
sd.play(kiss, samplerate) # start playing music
sd.wait() # wait until music finishes before exiting

# To run:
# $python play.py