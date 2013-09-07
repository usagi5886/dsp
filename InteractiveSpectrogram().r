# InteractiveSpectrogram()

# Input - any kind of audio object recognized by the core Spectrogram() function

# Output - a TclTk interface to dynamically manipulate the plot

# NOTE: Since this script operates 'on top of' the Spectrogram() function, it is assumed that has been read into R (by 'sourcing' it, etc.)
#       Hence the commented-out first line of code: source("Spectrogram().r")

# -------------------------------------------------------------------------------

# This script is released under the Berkeley Software Distribution (BSD) license ( http://opensource.org/licenses/BSD-3-Clause ):

# Copyright (c) 2013, Aaron Albin
# All rights reserved.

#Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
#Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
#Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
#The name of the author of this software may not be used to endorse or promote products derived from this software without specific prior written permission.

#THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# -------------------------------------------------------------------------------

# source("Spectrogram().r")

InteractiveSpectrogram = function(Audio){

library(tcltk)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Initialize the arguments/variables to their default values

# Radio buttons
Preemphasis = tclVar(1) # Convert to logical
Omit0Frequency = tclVar(0) # Convert to logical
WindowType = tclVar("kaiser")
Color = tclVar("NULL")

# Sliders
WindowLength = tclVar(5)
FrequencyResolution = tclVar(4)
DynamicRange = tclVar(70)
nTimeSteps = tclVar(400)
MaximumFrequency = tclVar(Audio$rate/2)

assign(x="SuccessfullyInitialized", value=FALSE, envir=.GlobalEnv)

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# All of the following are left off of the function call because they are left at their defaults:
# SamplingFrequency=NULL,
# TimeStepSize=NULL, # Necessary because we are only manipulating 'nTimeSteps'
# Omit0Frequency = FALSE,
# plot=TRUE,
# PlotFast=TRUE,
# xlim = NULL,
# main="",
# xlab="Time (ms)",
# ylab="Frequency (Hz)",

NewPlot <- function(...) {

# Change "NULL" to NULL if specified as such for the color
ExtractedColorValue = as.character(tclObj(Color))
if( ExtractedColorValue == "NULL" ){ColorValue = NULL}else{ColorValue=ExtractedColorValue}

# Now run the main plotting function
Spectrogram(Audio=Audio,
            WindowLength = as.numeric(tclObj(WindowLength)),
            FrequencyResolution = as.numeric(tclObj(FrequencyResolution)),
            nTimeSteps = as.numeric(tclObj(nTimeSteps)),
            Preemphasis = as.logical(tclObj(Preemphasis)),
            DynamicRange = as.numeric(tclObj(DynamicRange)),
            WindowType = as.character(tclObj(WindowType)),
            add=FALSE,
            col = ColorValue,
            ylim=c(0,as.numeric(tclObj(MaximumFrequency)))
) # End call to function 'Spectrogram()'

} # End definition of function 'NewPlot()'

# The following is identical to the function above except 'add=TRUE'
Overplot <- function(...) {
ExtractedColorValue = as.character(tclObj(Color))
if( ExtractedColorValue == "NULL" ){ColorValue = NULL}else{ColorValue=ExtractedColorValue}
Spectrogram(Audio=Audio,
            WindowLength = as.numeric(tclObj(WindowLength)),
            FrequencyResolution = as.numeric(tclObj(FrequencyResolution)),
            nTimeSteps = as.numeric(tclObj(nTimeSteps)),
            Preemphasis = as.logical(tclObj(Preemphasis)),
            DynamicRange = as.numeric(tclObj(DynamicRange)),
            WindowType = as.character(tclObj(WindowType)),
            add=TRUE,
            col = ColorValue,
            ylim=c(0,as.numeric(tclObj(MaximumFrequency)))
) # End call to function 'Spectrogram()'
} # End definition of function 'Overplot()'

Overplot_Slider = function(...){ # Almost always this one
if(SuccessfullyInitialized){Overplot(...)}
} # End definition of function 'Overplot_Slider'

NewPlot_Slider = function(...){ # Currently used for y/'MaximumFrequency' only (since it currently displays the full x / the entire soundfile by default)
if(SuccessfullyInitialized){NewPlot(...)}
} # End definition of function 'NewPlot_Slider'

#<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

AdjustmentWindow <- tktoplevel()
tkwm.title(AdjustmentWindow, "Spectrogram settings")

SettingsPanel <- ttkframe(AdjustmentWindow,borderwidth=2)
LeftHalf <- ttkframe(SettingsPanel)
RightHalf <- ttkframe(SettingsPanel)

#################
# Radio buttons #
#################

#____________
# Preemphasis
PreemphasisFrame <- ttkframe(LeftHalf, relief="groove", borderwidth=2)
tkgrid(ttklabel(PreemphasisFrame, text="Apply preemphasis?"), sticky="ewn")
tkgrid( ttkradiobutton(PreemphasisFrame, command=Overplot, text="Yes", value="1", variable=Preemphasis), sticky="ewn")
tkgrid(ttkradiobutton(PreemphasisFrame, command=Overplot, text="No", value="0", variable=Preemphasis), sticky="ewn")

#___________
# WindowType

WindowTypeFrame <- ttkframe(LeftHalf, relief="groove", borderwidth=2)
tkgrid(ttklabel(WindowTypeFrame, text="Window type"), sticky="ewn")

WindowTypeData = c( square = "Square", # Rectangular
                    blackman = "Blackman",
                    hann = "Hann", # Hanning, Sine-squared
                    hamming = "Hamming", # Raised sine-squared
                    cosine = "Sine", # Sine
                    bartlett = "Bartlett",
                    gaussian = "Gaussian", # Note: WindowParameter is fixed at the default of 0.4
                    kaiser = "Kaiser" # Note: WindowParameter is fixed at the default of 3
) # End definition of 'WindowTypeData'

for( EachWindowType in 1:length(WindowTypeData) ){ # EachWindowType=1
PrettyName = as.character(WindowTypeData[EachWindowType])
InternalName = names(WindowTypeData)[EachWindowType]
RadioButton = ttkradiobutton(WindowTypeFrame, command=Overplot, text=PrettyName, value=InternalName, variable=WindowType)
tkgrid(RadioButton, sticky="ewn")
} # End 'EachWindowType' loop

#____
# Color

colFrame <- ttkframe(LeftHalf, relief="groove", borderwidth=2)
tkgrid(ttklabel(colFrame, text="Color scheme"), sticky="ewn")
tkgrid(ttkradiobutton(colFrame, command=Overplot, text="Default", value="NULL", variable=Color), sticky="ewn")
tkgrid(ttkradiobutton(colFrame, command=Overplot, text="Alternate", value="alternate", variable=Color), sticky="ewn")
tkgrid(ttkradiobutton(colFrame, command=Overplot, text="Greyscale", value="greyscale", variable=Color), sticky="ewn")

###########
# Sliders #
###########

#_____________
# nTimeSteps

nTimeStepsFrame <-ttkframe(RightHalf, relief="groove", borderwidth=2)
tkgrid( ttklabel (nTimeStepsFrame, text="Number of time steps"), sticky="ewn" )
tkgrid( tkscale(nTimeStepsFrame, command=Overplot_Slider, from=25, to=1000, showvalue=TRUE, variable=nTimeSteps, resolution=25, orient="horiz"), sticky="ewn" )

#_____________
# WindowLength

WindowLengthFrame <-ttkframe(RightHalf, relief="groove", borderwidth=2)
tkgrid( ttklabel (WindowLengthFrame, text="Window length (ms)"), sticky="ewn" )
tkgrid( tkscale(WindowLengthFrame, command=Overplot_Slider, from=5, to=50, showvalue=TRUE, variable=WindowLength, resolution=5, orient="horiz"), sticky="ewn" )

#_________________
# MaximumFrequency

MaximumFrequencyFrame <-ttkframe(RightHalf, relief="groove", borderwidth=2)
tkgrid( ttklabel (MaximumFrequencyFrame, text="Max frequency (Hz)"), sticky="ewn" )
tkgrid( tkscale(MaximumFrequencyFrame, command=NewPlot_Slider, from=1, to=Audio$rate/2, showvalue=TRUE, variable=MaximumFrequency, resolution=500, orient="horiz"), sticky="ewn" ) # Is this resolution coarse enough?

#____________________
# FrequencyResolution

FrequencyResolutionFrame <-ttkframe(RightHalf, relief="groove", borderwidth=2)
tkgrid( ttklabel (FrequencyResolutionFrame, text="Frequency resolution"), sticky="ewn" )
tkgrid( tkscale(FrequencyResolutionFrame, command=Overplot_Slider, from=1, to=10, showvalue=TRUE, variable=FrequencyResolution, resolution=1, orient="horiz"), sticky="ewn" )

#_____________
# DynamicRange

DynamicRangeFrame <-ttkframe(RightHalf, relief="groove", borderwidth=2)
tkgrid( ttklabel (DynamicRangeFrame, text="Dynamic range (dB)"), sticky="ewn" )
tkgrid( tkscale(DynamicRangeFrame, command=Overplot_Slider, from=1, to=150, showvalue=TRUE, variable=DynamicRange, resolution=1, orient="horiz"), sticky="ewn" )

tkgrid(PreemphasisFrame, sticky="ewn")
tkgrid(WindowTypeFrame, sticky="ewn")
tkgrid(colFrame, sticky="ewn")

tkgrid(nTimeStepsFrame, sticky="ewn")
tkgrid(WindowLengthFrame, sticky="ewn")
tkgrid(MaximumFrequencyFrame, sticky="ewn")
tkgrid(FrequencyResolutionFrame, sticky="ewn")
tkgrid(DynamicRangeFrame, sticky="ewn")

tkgrid(LeftHalf, RightHalf, sticky="ewn")

RevertSettings = function(){

tclvalue(Preemphasis) <- "1"
tclvalue(Omit0Frequency) <- "0"
tclvalue(WindowType) <- "kaiser"
tclvalue(Color) <- "NULL"
tclvalue(WindowLength) <- "5"
tclvalue(FrequencyResolution) <- "4"
tclvalue(DynamicRange) <- "70"
tclvalue(nTimeSteps) <- "400"
tclvalue(MaximumFrequency) <- as.character( Audio$rate/2 )

} # End definition of function 'RevertSettings()'

ResetButton <- ttkbutton(SettingsPanel, text="Reset", command=function(){RevertSettings();NewPlot()}) # Use NewPlot() in case the user has changed the MaximumFrequency parameter
tkgrid(ResetButton,columnspan=2)

tkgrid(SettingsPanel)

NewPlot()

assign(x="SuccessfullyInitialized", value=TRUE, envir=.GlobalEnv)

} # End over-arching call to function()
