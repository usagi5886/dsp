# PlotSpectrogram()

# Input: A matrix created by the Spectrogram() function (with all of the corresponding attributes

# Output: None; a plot of the spectrogram is produced.

# Usage: x = Spectrogram("C:/Users/MyUsername/Desktop/soundfile.wav", plot=FALSE)
#        PlotSpectrogram(x)

# -------------------------------------------------------------------------------

# Note that most of this code is identical to the code at the end of the script for the Spectrogram() function itself.

PlotSpectrogram = function( # Begin argument list

# [1]
SpectrogramMatrix,
# a matrix containing the result of the analysis, with attributes indicating all the non-NULL argument specifications that influenced the result of the analysis;

# [2]
PlotFast=TRUE,
# If set to FALSE, the filled.contour() function will be used. This produces much better looking graphics (which is best for putting into publications), but takes considerably longer to plot.
# If set to TRUE (the default), the image() function will be used instead, with 'useRaster' set to TRUE. This makes the plotting very fast, which is optimal for when one is dynamically exploring/interacting with the signal.
# (This may not work properly if raster graphics are not supported on your device. See help("image") for details.)

# [3]
add = FALSE,
# This determines whether an entirely new plot is drawn (with all the annotation) or whether just the core image is drawn
# Careful - this should only be set to TRUE if a spectrogram has already been drawn (and therefore a graphics device / window is already open.
# Note: If 'add' is set to TRUE, the coordinate system of the pre-existing plot will be used; hence, any specifications of xlim and ylim will be ignored for the subsequent call to Spectrogram(..., add=TRUE).

# [4]
col = NULL,
# At present, you can use this argument in four ways:
# - If you leave this at NULL, the color map will be DarkBlue-Blue-Cyan-Yellow-Orange-Red-Brown
# - If you set this to "alternate", the color map will be Black-Red-Orange-Yellow-White
# - You can also set this to "greyscale"/"grayscale" to have things mapped onto a continuum from black to white.
# - Finally, you can also provide a custom vector of colors to use.

# [5]
xlim = NULL,
# If left NULL, this will be set to the full time range of the soundfile

# [6]
ylim = NULL,
# If left NULL, this will go from 0 to the soundfile's Nyquist frequency.
# Note that Praat and phonTools default to 5000 Hz.
# (Avoiding a fixed arbitrary number makes the user think more carefully about what they are zooming into.)

# [7]
# Main title, x axis label, and y axis label
main = "",
xlab = "Time (ms)",
ylab = "Frequency (Hz)"

){ # Begin function definition

# Reconstruct several variables available only inside the normal function call to Spectrogram()
TimeSequence = as.numeric(rownames(SpectrogramMatrix))
FrequencySequence = as.numeric(colnames(SpectrogramMatrix))
SamplingFrequency = attributes(SpectrogramMatrix)$SamplingFrequency


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


#########################################
# Quick synopsis of arguments:          #
# Spectrogram( SpectrogramMatrix,       #
#              PlotFast = TRUE,         #
#              add = FALSE,             #
#              col = NULL,              #
#              xlim = NULL,             #
#              ylim = NULL,             #
#              main = "",               #
#              xlab = "Time (ms)",      #
#              ylab = "Frequency (Hz)") #
#########################################

} # End definition of function 'PlotSpectrogram()'

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

# This script was modified from a subset of the code in the spectrogram() function in the 'phonTools' package.
# http://cran.r-project.org/web/packages/phonTools/index.html
# The latter is released under the two-clause Berkeley Software Distribution (BSD) license.
# http://opensource.org/licenses/BSD-2-Clause
# (c) 2012-2015 Santiago Barreda
