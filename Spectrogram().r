# Spectrogram()

# Input = a soundfile stored on the user's computer (or a numeric vector containing the equivalent information therein)

# Output = if plot=FALSE, returns a matrix (with various attributes) containing the result of the spectrographic analysis; if plot=TRUE, nothing is returned, and instead a filled-contour plot of the spectrogram is produced

# -------------------------------------------------------------------------------

# This script is released under the Berkeley Software Distribution (BSD) license ( http://opensource.org/licenses/BSD-3-Clause ):

#Copyright (c) 2013, Aaron Albin

# Modified from the spectrogram() function in the 'phonTools' package of R by Santiago Barreda
# http://cran.r-project.org/web/packages/phonTools/index.html

# All rights reserved.

#Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
#Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
#Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
#Neither the name of the <ORGANIZATION> nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

#THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# -------------------------------------------------------------------------------

Spectrogram = function( # Begin argument list

# [1]
Audio,

# The 'Audio' argument contains the sound from which the spectrogram was made.
# It can be any of the following four kinds of objects:

# An object of class 'sound', created with the 'loadsound()' function from the package 'phonTools'
# An object of class 'Wave', created with the 'readWave()' function from the package 'tuneR'
# An object of class 'Sample', created with the 'loadSample()' function from the package 'sound'
# An object of class 'audioSample', created with the 'load.wave()' function from the package 'audio'

# Or, in a more compact form:
#_CLASS_______FUNCTION_____PACKAGE__
# sound       loadsound()  phonTools
# Wave        readWave()   tuneR
# Sample      loadSample() sound
# audioSample load.wave()  audio

# It can also be a numeric vector (representing a sequence of samples taken from a sound wave) if and only if the SamplingFrequency argument is specified (i.e. not NULL).

# [2]
SamplingFrequency=NULL,

# SamplingFrequency. The sampling frequency/rate of the sound in Hertz.
# Only necessary to specify if 'Audio' is of class 'numeric'; if it is of class 'sound', 'Wave', 'Sample', or 'audioSample', this is ignored (hence does not need to be specified).

# The default value for SamplingFrequency is set to 22050 in phonTools 'Spectrogram' function, but it is safer to make the user always specify it if they want to use a numeric vector for the Audio object.

# [3]
WindowLength = 5,
# The desired length (in milliseconds) of the analysis window used to create the spectrogram
# The default is 5 milliseconds in spectrogram() from package 'phonTools'

# [4]
FrequencyResolution = 4,
# Set this to any positive integer, with higher numbers mean better resolution.
# Specifically, for any integer X provided, 1/X the analysis bandwidth (as determined by the number of samples in the analysis window) will be used.
# Note that this greatly impacts the processing time, so adjust with care!

# Either both of the following can be left as NULL, or one (not both!) can be specified:
# [5]
TimeStepSize = NULL, # Number of milliseconds that the window will be moved for each adjacent analysis
nTimeSteps = NULL, # The overall total number of time steps
# If both of the above are left at NULL, nTimeSteps=400 will be used.
# Note that also this greatly impacts the processing time, so adjust with care!

# [6]
Preemphasis = TRUE,
# Should pre-emphasis be applied (to all frequency regions, i.e. with a dummy frequency cutoff of 0)?
# In other words, should the spectral slope at all frequencies increase by 6 dB per octave using a single-pole filter?
# This affects both the underlying spectrogram matrix as well as the plot thereof.

# [7a]
DynamicRange = 70,
# The default for spectrogram() in package 'phonTools' was 50, whereas Praat's is 70.
# 70 seems to be preferable because you can see more things without distorting what's important.
# Values less than this many dB below the maximum are 'clipped' to that value.
# If this is set to NULL, no such clipping occurs.
## This affects both the underlying spectrogram matrix as well as the plot thereof.

# [7b]
Omit0Frequency = FALSE,
# The frequency band at 0 Hz is usually at very low values (e.g. -400 to -300 dB).
# Select TRUE to omit this frequency band from the resulting spectrogram (both the matrix and the plot thereof).

# [8a]
WindowType = "kaiser",
# A character string indicating the desired type of window function to be applied to the signal
# All of the following types are supported:
# * "rectangular" / "square"
# * "blackman"
# * "hann" / "hanning" (i.e. sine-squared)
# * "hamming" (i.e. raised sine-squared)
# * "cosine" / "sine"
# * "bartlett"
# * "gaussian"
# * "kaiser"
# (Note that all names are in lowercase.)

# [8b]
WindowParameter = NULL,
# This is only relevant if the WindowType is set to "gaussian" or "kaiser"; it will be ignored in all other cases
# For those two kinds of window function, this specifies the relevant parameter behind the gaussian/kaiser function.
# If 'WindowParameter' is set to NULL for these two window types, the following defaults will be used:
# - kaiser: 3 (not 2!)
# - gaussian: 0.4

# [9]
plot = TRUE,
# Whether the spectrogram should be plotted or not
# If FALSE, no spectrogram is plotted, and instead, a matrix is returned containing the magnitude at each bin center

# ^-^-^-^-^-^-^ Plotting parameters ^-^-^-^-^-^-^
#(All of the following will do nothing if 'plot' is set to FALSE.)

# [10]
col = NULL,
# At present, you can use this argument in four ways:
# - If you leave this at NULL, the color map will be DarkBlue-Blue-Cyan-Yellow-Orange-Red-Brown
# - If you set this to "alternate", the color map will be Black-Red-Orange-Yellow-White
# - You can also set this to "greyscale" to have things mapped onto a continuum from black to white.
# - Finally, you can also provide a custom vector of colors to use.

# [11]
nColorLevels = NULL,
# The number of color gradations to be used for the spectrogram.
# If this is left at NULL, the decibel range of the spectrogram (as determined by the DynamicRange) will be used such that there is a unique color for each decibel.

# [12a]
xlim = NULL,
# If left NULL, this will be set to the full time range of the soundfile

# [12b]
ylim = NULL,
# If left NULL, this will go up to 5000 Hz - a generally useful range for analyzing speech

# [13]
# Main title, x axis label, and y axis label
main = "",
xlab = "Time (ms)",
ylab = "Frequency (Hz)"

){ # Begin function definition

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# STEP ONE: Check to make sure everything in the input is acceptable

AudioClass = class(Audio)

AcceptableClasses = c("sound","Wave","Sample","audioSample","numeric")
AudioClassAcceptable = as.logical( sum( AudioClass == AcceptableClasses ) )
if(!AudioClassAcceptable){
stop("The 'Audio' argument must be one of the following classes:\n       sound, Wave, Sample, audioSample, numeric")
}else{

if( (AudioClass=="numeric" & is.null(SamplingFrequency)) ){
stop("Must specify SamplingFrequency.")
}else{ # i.e. if 'Audio' is one of the four classes, or if Audio is numeric and the SamplingFrequency is specified (... then proceed)

if( AudioClass!="numeric" # If the Audio object is one of the four audio classes (sound/Wave/Sample/audioSample), ...
    & !is.null(SamplingFrequency) ){ # and if they provide a superfluous SamplingFrequency, ...
# ...then issue a warning that it will be ignored:
warning("Specified SamplingFrequency ignored; the one associated with Audio object has been used instead.")
} # End check whether to issue a warning

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# STEP TWO: Samples and SamplingFrequency 

# Now store the actual samples themselves in a variable 'Samples' (doing stereo-to-mono conversion as necessary)
# Also, store the sample rate in a variable 'SamplingFrequency' (over-writing the NULL input to the function as necessary)

if( AudioClass=="numeric"){
Samples = Audio # As-is (since it must be numeric by this point in the code)
# SamplingFrequency has already been specified as an argument to the function (as enforced above)
} # End 'if class of input Audio object is 'numeric'

# /------------------------------------\
# | Notes on stereo-to-mono conversion:|
# \------------------------------------/
# | spectrogram() in package 'phonTools' and specgram() in package 'signal' require the input to be mono (in one way or another)
# | spectro() in package 'seewave' allows stereo files as input but throws away the second channel.
# | However, here, information from both channels will be used, in order not to throw away information
# | There are two ways of doing so: either summing and re-normalizing the two channels, or averaging the two channels.
# | (See http://www.mathworks.com/matlabcentral/newsreader/view_thread/44379 for discussion.)
# | The latter will be done here, due to (1) its conceptual simplicity, (2) this is what is implemented 'tuneR:::mono', and (3) this appears to be the standard: http://dsp.stackexchange.com/questions/2484/converting-from-stereo-to-mono-by-averaging
# | This will be consistently implemented as '(object@right + object@left)/2', following the mono() function in 'tuneR'.

if( AudioClass=="sound"){ # From 'phonTools' package
# Since, at present, phonTools only supports mono soundfiles (i.e. a 'sound' object is by definition mono), so no checking of stereo/mono status or stereo-to-mono conversion is necessary
Samples = as.numeric( Audio$sound )
SamplingFrequency = Audio$fs
} # End 'if class of input Audio object is 'sound'

if( AudioClass=="Wave"){ # From 'tuneR' package
IsStereo = attributes(Wave)$stereo
if(IsStereo){ Samples = ( attributes(Audio)$left + attributes(Wave)$right ) / 2
       }else{ Samples = attributes(Audio)$left # Recall that 'attributes(Wave)$right' is 'numeric(0)' if a mono file is being dealt with
} # End 'if/else is stereo'
SamplingFrequency = attributes(Audio)$samp.rate
} # End 'if class of input Audio object is 'Wave'

if( AudioClass=="Sample"){ # From 'sound' package
IsStereo = ( nrow(Audio)==2 ) # Recall the stereo-mono distinction is encoded via the differing number of rows in the Sample object
if(IsStereo){ Samples = ( Audio$sound[1,] + Audio$sound[2,] ) / 2
       }else{ Samples = Audio$sound[1,]
} # End 'if/else is stereo'
SamplingFrequency = Audio$rate
} # End 'if class of input Audio object is 'Sample'

if( AudioClass=="audioSample"){ # From 'audio' package
IsStereo = is.matrix(Audio) # Recall that a stereo audioSample is stored as a matrix (and a mono one is not)
if(IsStereo){ Samples = ( Audio[1,] + Audio[2,] ) / 2
       }else{ Samples = as.numeric( Audio )
} # End 'if/else is stereo'
SamplingFrequency = Audio$rate
} # End 'if class of input Audio object is 'audioSample'

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Convert the window length that the user specified into the actual number of samples to be used.

nSamplesInWindow = ceiling( (SamplingFrequency/1000) * WindowLength ) # 1000 because 'WindowLength' is defined above as being in milliseconds
# 'nSamplesInWindow' needs to be odd. So if it is even (i.e. if the remainder of dividing it by 2 is 0), add +1 to make it odd
if(nSamplesInWindow%%2==0){ nSamplesInWindow = nSamplesInWindow + 1 }

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Run checks on the settings for the timestep (i.e. the number of samples advanced from one analysis to the next)

# Stop computation if the user has provided both 'TimeStepSize' *and* 'nTimeSteps':
if( !is.null(TimeStepSize) & !is.null(nTimeSteps) ){
stop("You can only specify either TimeStepSize *or* nTimeSteps, not both.")
}else{

# If the 'TimeStepSize' and 'nTimeSteps' are both set to NULL, use an nTimeSteps of 400 (as per the default in spectrogram() from the 'phonTools' package)
if( is.null(TimeStepSize) & is.null(nTimeSteps) ){ nTimeSteps = 400 }

# If the user manually specified 'TimeStepSize' (i.e. a number of milliseconds)
if(!is.null(TimeStepSize)){ TimeStep = floor( TimeStepSize/1000 * SamplingFrequency ) }

# If the user manually specified 'nTimeSteps' (i.e. an integer value)
if(!is.null(nTimeSteps)){ TimeStep = floor( length(Samples)/nTimeSteps ) }

# NOTE: Keep in mind that the units of 'TimeStep' is *samples*, not milliseconds.

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# If the user selected to apply preemphasis...
if(Preemphasis==TRUE){

PreemphasizedSamples = as.numeric( filter(Samples, c(1, -1), method = "convolution", sides = 1) ) # filter() is a low-level function from the 'stats' package

# The filter() function leaves the first sample as NA, so just fill this in with the original value
PreemphasizedSamples[1] <- Samples[1]

# Now, over-write the original samples object with the pre-emphasized samples
# In the function that this code will ultimately reside in, there is no danger of re-running these lines of code multiple times and thereby cyclically distort the Samples data
Samples = PreemphasizedSamples
} # End 'if applying pre-emphasis'

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Create the necessary padding

HalfWindowSize = floor(nSamplesInWindow/2)
ZeroPadding = rep(0, HalfWindowSize)
ZeroPaddedSamples = c(ZeroPadding, Samples, ZeroPadding)
to = length(ZeroPaddedSamples) - HalfWindowSize*2 # 'Twice half' because it was rounded above
LeftEdgeLocations = seq(from=1, to=to, by=TimeStep)

AnalysisBandwidth = SamplingFrequency/nSamplesInWindow
FrequencyResolutionFactor = AnalysisBandwidth/FrequencyResolution # 'FrequencyResolutionModifier' is an argument to the function. Default=4.
nSamplesPadding = (SamplingFrequency - FrequencyResolutionFactor * nSamplesInWindow)/FrequencyResolutionFactor
# If 'nSamplesPadding' ends up being negative, clip it to 0 instead.
if(nSamplesPadding < 0){ nSamplesPadding = 0 }

nSamplesInPaddedWindow = nSamplesInWindow + nSamplesPadding

## Create an adjusted version of 'nSamplesPadding' that is constrained to be an odd number
#if(nSamplesPadding%%2==0){ # i.e. if the remainder is 0 (hence even)
#nSamplesPadding_Adjusted = nSamplesPadding + 1
#}else{ # i.e. if the remainder is 1 (hence already odd)
#nSamplesPadding_Adjusted = nSamplesPadding
#} # End 'if/else 'nSamplesPadding' is odd

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Create a windowing function to apply to the signal
Windowing = function(CurrentLeftEdge){ # CurrentLeftEdge = LeftEdgeLocations[4]

# Extract out the target samples for the current analysis
TargetSamples = ZeroPaddedSamples[CurrentLeftEdge:(CurrentLeftEdge + nSamplesInWindow - 1)]

# Append zeroes to the end based on the adjusted padding
#TrailingZeroesAppended = c(TargetSamples, rep(0, times=nSamplesPadding_Adjusted))
times = FrequencyResolution*length(TargetSamples) - length(TargetSamples)
TrailingZeroesAppended = c(TargetSamples, rep(0, times=times))

IntegerSequence = 0:(nSamplesInWindow-1) # Technically not needed for 'rectangular', but include here nonetheless
if(WindowType == "rectangular"|
   WindowType == "square"     ){ WindowFunction = rep(1, nSamplesInWindow) }
if(WindowType == "blackman"   ){ WindowFunction = (7938/18608) - (9240/18608) * 
                                                           cos((2 * pi * IntegerSequence)/(nSamplesInWindow - 1)) +
                                                           (1430/18608) *
                                                           cos((4 * pi * IntegerSequence)/(nSamplesInWindow - 1)) }
if(WindowType == "hann"       |
   WindowType == "hanning"    ){ WindowFunction = 0.5 * (1 - cos((2 * pi * IntegerSequence)/(nSamplesInWindow - 1))) }
if(WindowType == "hamming"    ){ WindowFunction = 0.54 - 0.46 * cos((2 * pi * IntegerSequence)/(nSamplesInWindow - 1)) }
if(WindowType == "cosine"     |
   WindowType == "sine"       ){ WindowFunction = sin((IntegerSequence * pi)/(nSamplesInWindow - 1)) }
if(WindowType == "bartlett"   ){ WindowFunction = (2/(nSamplesInWindow - 1)) *
                                                          ((nSamplesInWindow - 1)/2 - abs(IntegerSequence - (nSamplesInWindow - 1)/2)) }
if(WindowType == "gaussian"   ){ if(is.null(WindowParameter)){ WindowParameter = 0.4 }
                                         WindowFunction = exp(-0.5 * ((IntegerSequence - (nSamplesInWindow - 1)/2)/
										                              (WindowParameter * (nSamplesInWindow - 1)/2))^2)
                                       }
if(WindowType == "kaiser"     ){ if(is.null(WindowParameter)){ WindowParameter = 3 } # Note that the default argument for spectrogram() in phonTools is 3, whereas the default value for windowfunc() in phonTools is 2.
                                         WindowFunction = besselI(WindowParameter * pi * sqrt(1 - (2 * (IntegerSequence)/(nSamplesInWindow - 1) - 1)^2),0)/
										                  besselI(WindowParameter * pi, 0)
                                       }

# Now use this window function to do the core computations of the spectrogram
WindowedSamples = TrailingZeroesAppended * WindowFunction
MeanSubtracted = WindowedSamples - mean(WindowedSamples)
FFT = fft(MeanSubtracted)[1:(nSamplesInPaddedWindow/2 + 1)]
AbsoluteValueSquared = abs(FFT)^2
LogScaled = log(AbsoluteValueSquared, 10) * 10
return(LogScaled)
} # End function 'Windowing()'

# Now produce a matrix containing the magnitude at each bin center.
UnnormalizedSpectrogramMatrix = t( sapply(X=LeftEdgeLocations, FUN=Windowing) )
# Transpose in order to get time to extend across the rows and frequency to extend across the columns.

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Post-processing

# Normalize the spectrogram matrix by making the highest value to be 0.
SpectrogramMatrix = UnnormalizedSpectrogramMatrix - max(UnnormalizedSpectrogramMatrix)

# Clip very low values to be within the dynamic range
if(!is.null(DynamicRange)){
NegativeDecibelThreshold = -1 * DynamicRange # 'DynamicRange' is assumed to be positive. Perhaps eventually enforce this with 'stop()'.
SpectrogramMatrix[which(SpectrogramMatrix < NegativeDecibelThreshold)] = NegativeDecibelThreshold
} # End 'if DynamicRange is non-NULL'

# Determine the time and frequency values for every row and column in the matrix
TimeSequence = (LeftEdgeLocations + floor(nSamplesInWindow/2)) * (1000/SamplingFrequency)
FrequencySequence = (0:(nSamplesInPaddedWindow/2)) * (SamplingFrequency/nSamplesInPaddedWindow)

if( Omit0Frequency ){
SpectrogramMatrix = SpectrogramMatrix[,-1]
FrequencySequence = FrequencySequence[-1]
# Leave TimeSequence alone because that is is unrelated to the omission of the 0-frequency band.
} # End 'if omitting the 0-frequency band'

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# If requested, return the matrix object
if(plot==FALSE){

# Add the times as row names and frequencies as column names
rownames(SpectrogramMatrix) = as.numeric(round(TimeSequence, 2))
colnames(SpectrogramMatrix) = as.numeric(round(FrequencySequence, 2))
# Technically the rounding factor of 2 could be made into a customizable parameter of the overall function, but that is kind of low-level and not immensely useful so just leave as-is for now.

# For reconstructability purposes, include *all* the arguments/parameters used in the creation of the spectrogram as attributes to the matrix object
attributes(SpectrogramMatrix) = append( attributes(SpectrogramMatrix), list(SamplingFrequency=SamplingFrequency,
                                                                            WindowLength=WindowLength,
                                                                            FrequencyResolution=FrequencyResolution,
                                                                            TimeStepSize=TimeStepSize,
                                                                            nTimeSteps=nTimeSteps,
                                                                            Preemphasis=Preemphasis,
                                                                            DynamicRange=DynamicRange,
                                                                            Omit0Frequency=Omit0Frequency,
                                                                            WindowType=WindowType,
                                                                            WindowParameter=WindowParameter))
return(SpectrogramMatrix)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# If requested, plot the spectrogram
}else{ # i.f. is plot==TRUE

if(is.null(col)){ ColorGenerator = colorRampPalette(c("dark blue", "blue", "cyan", "yellow", "orange", "red", "brown")) }else{
if(identical(col,"alternate")){ ColorGenerator = colorRampPalette(c("black", "red", "orange", "yellow", "white")) }else{
if(identical(col,"greyscale")){ ColorGenerator = colorRampPalette(c("white", "black")) }else{
ColorGenerator = colorRampPalette( col ) # By this point in the code, it is assumed that the user has supplied a vector of colors to create the spectrogram
}}} # End 'if/else' for greyscale, alternate, and NULL

AmplitudeRange = range(SpectrogramMatrix, finite = TRUE) # 'finite=TRUE' excludes values of positive/negative infinity
MinimumAmplitude = AmplitudeRange[1]
MaximumAmplitude = AmplitudeRange[2] # Should always be zero due to the normalization in the post-processing

# If nColorLevels is set to NULL, over-ride this with the default of one color per decibel in the amplitude range
if( is.null( nColorLevels ) ){
MinimumAmplitude = AmplitudeRange[1]
MaximumAmplitude = AmplitudeRange[2] # Should always be zero due to the normalization in the post-processing
nColorLevels = abs(MinimumAmplitude) - abs(MaximumAmplitude) # In the original code from spectrogram() in package 'phonTools', the number from the line of code above is multiplied by 1.2
} # End 'if nColorLevels' is NULL

ColorLevels = seq(from=MinimumAmplitude, to=MaximumAmplitude, length.out=nColorLevels+1) # Add 1 so that a color is assigned to both endpoints of the range
ColorPalette = ColorGenerator(round(nColorLevels)) # Rounded in case the user provides a decimal number, or if DynamicRange is set to a non-whole number.

# If xlim is NULL, set it to the full time range of the soundfile
if( is.null(xlim) ){ xlim = range(TimeSequence) }

# if ylim is NULL, set it to cap out at 5000 by default
if( is.null(ylim) ){ ylim = c( min(FrequencySequence), 5000) }

# If the user provides a maximum ylim value that is beyond the Nyquist frquency (i.e. half of the sampling frequency), clip it to the Nyquist frequency
MaximumFrequency = ylim[which.max(ylim)]
if( MaximumFrequency > (SamplingFrequency/2) ){ ylim[which.max(ylim)] = SamplingFrequency/2 }

# Set up the plot
plot.new()
plot.window(xlim=xlim, ylim=ylim, xaxs="i", yaxs="i")

# The following version of filled.contour(), with the period at the front, omits the color bar to the right of the normal filled.contour() function
.filled.contour(x=TimeSequence, y=FrequencySequence, z=SpectrogramMatrix, levels=ColorLevels, col=ColorPalette)

# Add all the other typical dressings to the plot
box()
Axis(TimeSequence, side=1)
Axis(FrequencySequence, side=2)
title(main=main, xlab=xlab, ylab=ylab)

} # End 'if/else plot==TRUE'

} # End 'if/else the user simultaneously specified both TimeStepSize *and* nTimeSteps'

} # End 'if/else using numeric vector but did not specify sampling rate'

} # End 'if/else class for 'Audio' argument is one of the acceptable classes

} # End definition of function 'Spectrogram()'

# End script
