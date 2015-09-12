# Spectrogram()

# Input: Any of the following three options:
#        (A) a path to a soundfile saved somewhere on your computer
#        (B) an R object containing the information contained in this soundfile (imported into R using the 'audio', 'phonTools', 'tuneR', or 'sound' package)
#        (C) a numeric vector containing the information inside the soundfile
# Note: Option (A) requires at least one of the following packages to be installed: 'audio', 'phonTools', 'tuneR', or 'sound'.

# Output: if plot=FALSE, returns a matrix containing the result of the analysis, with attributes indicating all the non-NULL argument specifications that influenced the result of the analysis;
#         if plot=TRUE (the default), nothing is returned, and instead a plot of the spectrogram is produced

# Usage  -  Option (A): Spectrogram("C:/Users/MyUsername/Desktop/soundfile.wav")

#           Option (B): library("audio")
#                       x = load.wave("C:/Users/MyUsername/Desktop/soundfile.wav")
#                       Spectrogram(x)

#           Option (C): x = c(3.094482e-02, 2.154541e-02, 3.213501e-02, ...) # Created using sine-waves, etc.
#                       Spectrogram(x)

# -------------------------------------------------------------------------------

Spectrogram = function( # Begin argument list

# [1]
Audio,
# The 'Audio' argument specifies the audio data from which the spectrogram is to be made.
# Three options are available for using this argument.

# Option (A):
# You can set 'Audio' to be a character string (i.e. a character vector of length 1) with the path to a soundfile saved somewhere on your computer
# Note that this requires at least one of the following packages to be installed: 'audio', 'phonTools', 'tuneR', or 'sound'.

# Option (B)
# You can also set 'Audio' to be one of the following four kinds of R objects (each of which contains an R-internal representation of the information inside a soundfile):
# - An object of class 'audioSample', created with the 'load.wave()' function from the package 'audio'
# - An object of class 'sound', created with the 'loadsound()' function from the package 'phonTools'
# - An object of class 'Wave', created with the 'readWave()' function from the package 'tuneR'
# - An object of class 'Sample', created with the 'loadSample()' function from the package 'sound'

# In table form, this can be represented as follows:
#_CLASS_______FUNCTION_____PACKAGE__
# audioSample load.wave()  audio
# sound       loadsound()  phonTools
# Wave        readWave()   tuneR
# Sample      loadSample() sound

# Option (C)
# You can also set 'Audio' to be a numeric vector (representing a sequence of samples taken from a sound wave) if and only if the SamplingFrequency argument is specified (i.e. not NULL).
# (Note that setting 'Audio' to be a 2-row or 2-column matrix (representing the two channels in a stereo soundfile) is currently unsupported.)

# [2]
SamplingFrequency=NULL,

# SamplingFrequency. The sampling frequency/rate of the sound in Hertz.
# Only necessary to specify if 'Audio' is of class 'numeric'; if it is of any other class, this is ignored (hence does not need to be specified).

# The default value for SamplingFrequency is set to 22050 in the phonTools spectrogram function, but it is safer to make the user always specify it if they want to use a numeric vector for the Audio object.

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
# If FALSE, no spectrogram is plotted, and instead, a matrix is returned containing the magnitude at each bin center.
# The column names of this matrix correspond to time, and the row names correspond to frequency.
# Note that both are fully unrounded numbers stored as a character string (e.g. "115.532879818594").

# ^-^-^-^-^-^-^ Plotting parameters ^-^-^-^-^-^-^
#(All of the following will do nothing if 'plot' is set to FALSE.)

# [10]
PlotFast=TRUE,
# If set to FALSE, the filled.contour() function will be used. This produces much better looking graphics (which is best for putting into publications), but takes considerably longer to plot.
# If set to TRUE (the default), the image() function will be used instead, with 'useRaster' set to TRUE. This makes the plotting very fast, which is optimal for when one is dynamically exploring/interacting with the signal.
# (This may not work properly if raster graphics are not supported on your device. See help("image") for details.)

# [11]
add = FALSE,
# This determines whether an entirely new plot is drawn (with all the annotation) or whether just the core image is drawn
# Careful - this should only be set to TRUE if a spectrogram has already been drawn (and therefore a graphics device / window is already open.
# Note: If 'add' is set to TRUE, the coordinate system of the pre-existing plot will be used; hence, any specifications of xlim and ylim will be ignored for the subsequent call to Spectrogram(..., add=TRUE).

# [12]
col = NULL,
# At present, you can use this argument in four ways:
# - If you leave this at NULL, the color map will be DarkBlue-Blue-Cyan-Yellow-Orange-Red-Brown
# - If you set this to "alternate", the color map will be Black-Red-Orange-Yellow-White
# - You can also set this to "greyscale"/"grayscale" to have things mapped onto a continuum from black to white.
# - Finally, you can also provide a custom vector of colors to use.

# [13a]
xlim = NULL,
# If left NULL, this will be set to the full time range of the soundfile

# [13b]
ylim = NULL,
# If left NULL, this will go from 0 to the soundfile's Nyquist frequency.
# Note that Praat and phonTools default to 5000 Hz.
# (Avoiding a fixed arbitrary number makes the user think more carefully about what they are zooming into.)

# [14]
# Main title, x axis label, and y axis label
main = "",
xlab = "Time (ms)",
ylab = "Frequency (Hz)"

){ # Begin function definition

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Check to make sure Audio and SamplingFrequency are acceptable (and compatible with each other)

AudioClass = class(Audio)
AcceptableClasses = c("character","audioSample","sound","Wave","Sample","numeric")
AudioClassAcceptable = as.logical( sum( AudioClass == AcceptableClasses ) )
if(!AudioClassAcceptable){
stop("The 'Audio' argument must be one of the following classes:\n       character, audioSample, sound, Wave, Sample, numeric")
} # End 'if class for 'Audio' argument is not one of the acceptable classes'

# If Audio is numeric, ensure that the SamplingFrequency is specified
if( (AudioClass=="numeric" & is.null(SamplingFrequency)) ){
stop("Must specify SamplingFrequency.")
} # End 'if/else using numeric vector but did not specify sampling rate'

if( AudioClass!="numeric" # If the Audio object is one of the four audio classes (sound/Wave/Sample/audioSample), ...
    & !is.null(SamplingFrequency) ){ # and if they provide a superfluous SamplingFrequency, ...
# ...then issue a warning that it will be ignored:
warning("Specified SamplingFrequency ignored; the one associated with Audio object has been used instead.")
} # End check whether to issue a warning

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Samples and SamplingFrequency 

# Now store the actual samples themselves in a variable 'Samples' (doing stereo-to-mono conversion as necessary)
# Also, store the sample rate in a variable 'SamplingFrequency' (over-writing the NULL input to the function as necessary)

# /------------------------------------\
# | Notes on stereo-to-mono conversion:|
# \------------------------------------/
# | spectrogram() in package 'phonTools' and specgram() in package 'signal' require the input to be mono (in one way or another)
# | spectro() in package 'seewave' allows stereo files as input but throws away the second channel.
# | However, here, information from both channels will be used, in order not to throw away information
# | There are two ways of doing so: either summing and re-normalizing the two channels, or averaging the two channels.
# | (See http://www.mathworks.com/matlabcentral/newsreader/view_thread/44379 for discussion.)
# | The latter will be done here, due to (1) its conceptual simplicity, (2) this is what is implemented 'tuneR:::mono', and (3) this appears to be the standard: http://dsp.stackexchange.com/questions/2484/converting-from-stereo-to-mono-by-averaging
# | This will be consistently implemented as '(object$right + object$left)/2', following the mono() function in 'tuneR'.

if( AudioClass=="character"){ # A character string containing a file path

if( length(Audio)>=2 ){ stop("'Audio' is of length >= 2.\n       If specifying a file path, the character vector must be of length 1.") }
if( !file.exists(Audio) ){ stop("No file exists at the path specified in the 'Audio' argument.") }

InstalledPackages = rownames(installed.packages())
audioInstalled =  ( "audio" %in% InstalledPackages )
phonToolsInstalled = ( "phonTools" %in% InstalledPackages )
tuneRInstalled =  ( "tuneR" %in% InstalledPackages )
soundInstalled =  ( "sound" %in% InstalledPackages )

if( !phonToolsInstalled & !tuneRInstalled & !soundInstalled & !audioInstalled ){
stop("To specify a character-string file path for the 'Audio' argument,\n       at least one of the following packages must be installed:\n       audio, phonTools, tuneR, or sound")
} # End 'if none of the four packages for handling audio are installed'

# Now only read in one required package, with the hierarchy audio > phonTools > tuneR > sound.
# Using that package, over-write the character string file path with the actual audio data.
# Finally, adjust the AudioClass variable as appropriate (so as to flow into the code below for determining the sampling frequency).
if(audioInstalled){
	require("audio")
	Audio = load.wave(Audio)
	AudioClass = "audioSample"
}else{
	if(audioInstalled){
		require("phonTools")
		Audio = loadsound(Audio)
		AudioClass = "sound"
	}else{
		if(audioInstalled){
			require("tuneR")
			Audio = readWave(Audio)
			AudioClass = "Wave"
		}else{
			if(audioInstalled){
				require("sound")
				Audio = loadSample(Audio)
				AudioClass = "Sample"
			} # End if package 'sound' is installed
		} # End if/else package 'tuneR' is installed
	} # End if/else package 'phonTools' is installed	
} # End if/else package 'audio' is installed

} # End "if Audio is of class 'character'"

if( AudioClass=="audioSample"){ # From 'audio' package
IsStereo = is.matrix(Audio) # A stereo audioSample is stored as a matrix (and a mono one is not)
if(IsStereo){ Samples = ( Audio[1,] + Audio[2,] ) / 2
       }else{ Samples = as.numeric( Audio )
} # End 'if/else is stereo'
SamplingFrequency = attributes(Audio)$rate
} # End "if Audio is of class 'audioSample'"

if( AudioClass=="sound"){ # From 'phonTools' package
# Since, at present, phonTools only supports mono soundfiles (i.e. a 'sound' object is by definition mono), so no checking of stereo/mono status or stereo-to-mono conversion is necessary
Samples = as.numeric( Audio$sound )
SamplingFrequency = Audio$fs
} # End "if Audio is of class 'sound'"

if( AudioClass=="Wave"){ # From 'tuneR' package
IsStereo = attributes(Wave)$stereo
if(IsStereo){ Samples = ( attributes(Audio)$left + attributes(Wave)$right ) / 2
       }else{ Samples = attributes(Audio)$left # Recall that 'attributes(Wave)$right' is 'numeric(0)' if a mono file is being dealt with
} # End 'if/else is stereo'
SamplingFrequency = attributes(Audio)$samp.rate
} # End "if Audio is of class 'Wave'"

if( AudioClass=="Sample"){ # From 'sound' package
IsStereo = ( nrow(Audio)==2 ) # Recall the stereo-mono distinction is encoded via the differing number of rows in the Sample object
if(IsStereo){ Samples = ( Audio$sound[1,] + Audio$sound[2,] ) / 2
       }else{ Samples = Audio$sound[1,]
} # End 'if/else is stereo'
SamplingFrequency = Audio$rate
} # End "if Audio is of class 'Sample'"

if( AudioClass=="numeric"){
Samples = Audio # As-is (since it must be numeric by this point in the code)
# SamplingFrequency has already been specified as an argument to the function (as enforced above)
} # End "if Audio is of class 'numeric'"

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
} # End 'if the user simultaneously specified both TimeStepSize *and* nTimeSteps'

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
PreemphasizedSamples = as.numeric( stats:::filter(Samples, c(1, -1), method = "convolution", sides = 1) )
# filter() is a low-level function from the 'stats' package
# The package is specified in case it the function name masked by other identically-named functions in other packages (e.g. package 'signal')
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

# First make sure the 'WindowType' argument is OK
AcceptableWindowTypes = c("rectangular", "square", "blackman", "hann", "hanning", "hamming", "cosine", "sine", "bartlett", "gaussian", "kaiser" )
WindowTypeAcceptable = as.logical( sum( WindowType == AcceptableWindowTypes ) )
if(!WindowTypeAcceptable){
stop("The 'WindowType' argument must be one of the following:\n       rectangular / square, blackman, hann / hanning, hamming, cosine / sine, bartlett, gaussian, kaiser")
} # End 'if the WindowType argument is not one of the acceptable values'

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

if( WindowType!="gaussian" & WindowType!="kaiser" & !is.null(WindowParameter) ){warning("The specification of WindowParameter was ignored.\n(WindowParameter is only used if WindowType is 'gaussian' or 'kaiser'.)")}

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
rownames(SpectrogramMatrix) = as.numeric(TimeSequence)
colnames(SpectrogramMatrix) = as.numeric(FrequencySequence)

# For reconstructability purposes, include *all* the arguments/parameters used in the creation of the spectrogram as attributes to the matrix object
# Note that any NULL attributes will NOT be included! (because of how attributes work in R in general)
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
if( identical(col,"greyscale") | identical(col,"grayscale") ){ ColorGenerator = colorRampPalette(c("white", "black")) }else{
ColorGenerator = colorRampPalette( col ) # By this point in the code, it is assumed that the user has supplied a vector of colors to create the spectrogram
}}} # End 'if/else' for greyscale, alternate, and NULL

AmplitudeRange = range(SpectrogramMatrix, finite = TRUE) # 'finite=TRUE' excludes values of positive/negative infinity
MinimumAmplitude = AmplitudeRange[1]
MaximumAmplitude = AmplitudeRange[2] # Should always be zero due to the normalization in the post-processing

# For the number of color levels, have one color per decibel in the amplitude range

MinimumAmplitude = AmplitudeRange[1]
MaximumAmplitude = AmplitudeRange[2] # Should always be zero due to the normalization in the post-processing
nColorLevels = abs(MinimumAmplitude) - abs(MaximumAmplitude) # In the original code from spectrogram() in package 'phonTools', the number from the line of code above is multiplied by 1.2

ColorLevels = seq(from=MinimumAmplitude, to=MaximumAmplitude, length.out=nColorLevels+1) # Add 1 so that a color is assigned to both endpoints of the range
ColorPalette = ColorGenerator(round(nColorLevels)) # Rounded in case the user provides a decimal number, or if DynamicRange is set to a non-whole number.

# If xlim is NULL, set it to the full time range of the soundfile
if( is.null(xlim) ){ xlim = range(TimeSequence) }

# if ylim is NULL, set it to cap out at the Nyquist frequency (i.e. the highest visible frequency given the sampling rate of the soundfile)
if( is.null(ylim) ){ ylim = range(FrequencySequence) }

# If the user provides a maximum ylim value that is beyond the Nyquist frquency (i.e. half of the sampling frequency), clip it to the Nyquist frequency
MaximumFrequency = ylim[which.max(ylim)]
if( MaximumFrequency > (SamplingFrequency/2) ){ ylim[which.max(ylim)] = SamplingFrequency/2 }

if( PlotFast == TRUE){ # i.e. if the user wants to use image(..., useRaster=TRUE)

image( x=TimeSequence, 
       y=FrequencySequence,
       z=SpectrogramMatrix,
       xlim=xlim,
       ylim=ylim,
       zlim=AmplitudeRange,
       col=ColorPalette,
       add=add,
       xaxs="i",
       yaxs="i",
       xlab=xlab,
       ylab=ylab,
       # breaks, # Leave this unspecified since it defaults to equidistant breaks, which is exactly what I want
       main=main,
       oldstyle=FALSE, # FALSE = there are color intervals of equal lengths between the limits
                       # TRUE = the midpoints of the color intervals are equally spaced, with zlim[1] and zlim[2] as midpoints
       useRaster=TRUE # As described up above, this will significantly improve processing speed (which is the whole point of this being 'PlotFast') but may not always work (depending on the device the user is sending the image to)
) # End call to 'image()'

}else{ # i.e. if the user wants to use filled.contour

# Set up the plot (only if 'add' is FALSE - the default)
if(add==FALSE){
plot.new()
plot.window(xlim=xlim, ylim=ylim, xaxs="i", yaxs="i")
} # End 'if add is FALSE'

# The following version of filled.contour(), with the period at the front, omits the color bar to the right of the normal filled.contour() function
.filled.contour(x=TimeSequence, y=FrequencySequence, z=SpectrogramMatrix, levels=ColorLevels, col=ColorPalette)

# Add all the other typical dressings to the plot (again, only if 'add' is FALSE - the default)

if(add==FALSE){
Axis(TimeSequence, side=1)
Axis(FrequencySequence, side=2)
title(main=main, xlab=xlab, ylab=ylab)
} # End 'if add is FALSE'

} # End 'if/else PlotFast is TRUE'

box() # Putting this here makes sure that there always is a box, no matter what (e.g. it doesn't disappear with repeated over-plottings)

} # End 'if/else plot==TRUE'


#########################################
# Quick synopsis of arguments:          #
# Spectrogram( Audio,                   #
#              SamplingFrequency=NULL,  #
#              WindowLength = 5,        #
#              FrequencyResolution = 4, #
#              TimeStepSize = NULL,     #
#              nTimeSteps = NULL,       #
#              Preemphasis = TRUE,      #
#              DynamicRange = 70,       #
#              Omit0Frequency = FALSE,  #
#              WindowType = "kaiser",   #
#              WindowParameter = NULL,  #
#              plot = TRUE,             #
#              PlotFast = TRUE,         #
#              add = FALSE,             #
#              col = NULL,              #
#              xlim = NULL,             #
#              ylim = NULL,             #
#              main = "",               #
#              xlab = "Time (ms)",      #
#              ylab = "Frequency (Hz)") #
#########################################

} # End definition of function 'Spectrogram()'

########################################################################
# This program is free software: you can redistribute it and/or modify #
# it under the terms of the GNU General Public License as published by #
# the Free Software Foundation, either version 3 of the License, or    #
# (at your option) any later version.                                  #
#                                                                      #
# This program is distributed in the hope that it will be useful,      #
# but without any warranty; without even the implied warranty of       #
# merchantability or fitness for a particular purpose.  See the        #
# GNU General Public License for more details.                         #
#                                                                      #
# To receive a copy of the GNU General Public License, see:            #
# http://www.gnu.org/licenses/                                         #
########################################################################

# LICENSE NOTE:

# This script was modified from the spectrogram() function in the 'phonTools' package.
# http://cran.r-project.org/web/packages/phonTools/index.html
# The latter is released under the two-clause Berkeley Software Distribution (BSD) license.
# http://opensource.org/licenses/BSD-2-Clause
# (c) 2012-2015 Santiago Barreda
