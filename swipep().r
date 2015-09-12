#===============================================#
# An R implementation of SWIPE' ("swipe-prime") #
# (C) 2014  Aaron Albin   [ www.aaronalbin.com] #
#===============================================#

#########
# ABOUT #
#########

# The following is a line-by-line translation from Matlab into R of Arturo Camacho's SWIPE' pitch estimation algorithm, hosted at the following URL:
# http://www.cise.ufl.edu/~acamacho/publications/swipep.m
# SWIPE' uses only the first and prime harmonics of the signal to estimate the pitch, which is then fine-tuned using parabolic interpolation with a resolution of 1 cent.

# The following is the published journal article documenting the SWIPE' algorithm:
#Camacho, A., & Harris, J.G, (2008). A sawtooth waveform inspired pitch estimator for speech and music. Journal of the Acoustical Society of America 124(3), 1638-1652.
#http://scitation.aip.org/content/asa/journal/jasa/124/3/10.1121/1.2951592

# This, in turn, was based off of an earlier implementation (SWIPE) in Camacho's dissertation:
# Camacho, A. (2007). SWIPE: A sawtooth waveform inspired pitch estimator for speech and music. Doctoral dissertation, University of Florida, Florida.
# ( Downloadable from Camacho's homepage: http://www.cise.ufl.edu/~acamacho/english/curriculum.html#publicaciones )

# In systematic comparisons of F0 extraction algorithms (e.g. based on the EGG baseline data at http://www.cstr.ed.ac.uk/research/projects/fda/ ), SWIPE' performs equally as well as, if not better than, other available algorithms. See for example: 
# - Evanini et al 2012: http://scitation.aip.org/content/asa/journal/poma/11/1/10.1121/1.3609833
# - Tsanas et al 2014: http://scitation.aip.org/content/asa/journal/jasa/135/5/10.1121/1.4870484

# Note that SWIPE' has been implemented in C by Kyle Gorman for Linux and Mac, which is significantly faster than both Camacho's original Matlab code and this R implementation.
# For further information, see the following links: 
# - Gorman's homepage: http://www.csee.ogi.edu/~gormanky/
# - Gorman's SWIPE' homepage: http://www.ling.upenn.edu/~kgorman/c/swipe/
# - Gorman's GitHub page: https://github.com/kylebgorman/swipe/

# Note also that SWIPE' has recently been developed further into Aud-SWIPE', the corresponding C and Matlab code for which can be downloaded at:
# https://github.com/saul-calderonramirez/Aud-SWIPE-P
# (Click 'Wiki' on the right side of the page for more details.)

# The Aug-SWIPE' algorithm itself is documented in the following publications:
# - Camacho 2012: http://ieeexplore.ieee.org/xpl/login.jsp?arnumber=6310450
# - Calderón et al 2013: http://www.actapress.com/Abstract.aspx?paperId=455433

#########
# USAGE #
#########

#____________________________#
# --- [ Initialization ] --- #

# In order to run this function, swipep(), you must have the packages "e1071" and "signal" installed.

# If you are have Internet access while running your code, and if you have the "RCurl" package installed, you can easily have R read this script directly from GitHub with the following single of code:
# eval(parse(text = RCurl:::getURL("https://raw.github.com/usagi5886/dsp/master/swipep().r", followlocation=TRUE, cainfo=system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir=.GlobalEnv)

#___________________#
# --- [ Input ] --- #

# The only required arguments are a vector of samples of the signal (x) and the corresponding sampling rate (fs).
# Note that this function estimates F0 for the entire signal provided in the function 'x'.
# If it is desired that only a subset of the full time-range of that signal is analyzed, then the vector 'x' simply needs to be adjusted accordingly.

# In addition, several optional arguments can be specified for fine-tuning, namely plim, dt, dlog2p, dERBs, woverlap, and sTHR.
# If left unspecified, these optional arguments are left at their default values, which Camacho claims "have been found empirically to produce good results".
# See the beginning of the function definition for details about each of these arguments.

#____________________#
# --- [ Output ] --- #

# Returns an object of class 'data.frame' with three columns:
# 1) 'Time' = the times at which the F0 was estimated
# 2) 'Frequency' = the fundamental frequency estimate at each time point
# 3) 'Strength' = the strength of each F0 estimate

# 'Frequency' will sometimes be NaN (e.g. if dERBs is set high), but every frame should have a 'Strength' value.

#_____________________#
# --- [ Example ] --- #

# (1)

# Specify full path to a WAV file on your computer.
# Then, using the 'load.wave()' function from the 'audio' package, read in the soundfile
# ------------------------------------------
#filename = "..."
#Wave = audio:::load.wave(filename)
#x = as.numeric(Wave)
#Fs = attributes(Wave)$rate
# ------------------------------------------
# Corresponding line from Camacho's example:
# [x,Fs] = wavread(filename)

# (2)

# Now estimate the pitch of this signal every 10 ms within the range 75-500 Hz using the default resolution (i.e., 48 steps per octave), sampling the spectrum every 1/20th of ERB, using a window overlap factor of 50%, and discarding samples with pitch strength lower than 0.2.
# ------------------------------------------
#F0 = swipep(x=x, fs=Fs, plim=c(75,500), dt=0.01, dlog2p=1/500, dERBs=1/20, woverlap=0.5, sTHR=0.2 ) # Left at default: dlog2p=1/48
# ------------------------------------------
# Corresponding line from Camacho's example:
# [p,t,s] = swipep(x,Fs,[75 500],0.01,[],1/20,0.5,0.2)

# (3)

# Finally, plot the pitch trace.
# ------------------------------------------
#plot(x=F0$Time,y=F0$Frequency, xlab="Time (ms)", ylab="Pitch (Hz)")
# ------------------------------------------
# Corresponding lines from Camacho's example:
# plot(1000*t,p)
# xlabel('Time (ms)')
# ylabel('Pitch (Hz)')

####################################################################################
#function [p,t,s] = swipep(x,fs,plim,dt,dlog2p,dERBs,woverlap,sTHR)
swipep = function( # Begin argument list
####################################################################################

# The descriptions for the arguments below are adapted from Camacho's original script.

x,
# Vector of samples of the signal whose pitch is to be estimated
# In terms of R classes, this should be a numeric vector.

fs,
# The sampling frequency of the signal (in Hertz).

#if ~ exist( 'plim', 'var' ) || isempty(plim), plim = [30 5000]; end
# Pitch limits
# The pitch is searched within the range [PMIN PMAX] (in Hertz).
# Since the computational complexity is directly proportional to the number of octaves in the pitch search range, it is recommendable to restrict the search range to the expected range of pitch, if any.
# Default settings: PMIN = 30 Hz, PMAX = 5000 Hz
plim = c(30, 5000),

#if ~ exist( 'dt', 'var' ) || isempty(dt), dt = 0.001; end
# TimeStep # Size of time step (in seconds)
# The pitch of the signal is estimated every DT seconds.
# Default setting: DT = 0.001 s
dt=0.001,

#if ~ exist( 'dlog2p', 'var' ) || isempty(dlog2p), dlog2p = 1/48; end
# Frequency step size (Literally, delta/change in log2(pitch)), used to determine the candidates
# The pitch is searched with samples distributed every DLOG2P units on a base-2 logarithmic scale of Hertz.
# For better results, make DLOG2P as small as possible.
# However, take into account that the computational complexity of the algorithm is inversely proportional to DLOG2P.
# Default setting: DLOG2P = 1/48 (48 steps per octave)
dlog2p = 1/48,

#if ~ exist( 'dERBs', 'var' ) || isempty(dERBs), dERBs = 0.1; end
# Difference/timestep in ERBs
# The spectrum is sampled uniformly in the ERB scale with a step size of DERBS ERBs.
# For example, if you set this to 0.05 (i.e. 1/20), the spectrum will be sampled every 1/20th of an ERB.
# For better results, make DERBS as small as possible.
# However, take into account that the computational complexity of the algorithm is inversely proportional to DERBS.
# Default setting: DERBS = 0.1 ERBs
dERBs = 0.1,

#if ~ exist( 'woverlap', 'var' ) || isempty(woverlap)
#    woverlap = 0.5;
#elseif ...
#end
# Window overlap factor
# The spectrum is computed using a Hann window with an overlap WOVERLAP between 0 and 1.
# For example, if you set this to 0.5, the window overlap factor will be 50%
# For better results, make WOVERLAP as large as possible.
# However, take into account that the computational complexity of the algorithm is inversely proportional to 1-WOVERLAP.
# Default setting: WOVERLAP = 0.5
woverlap = 0.5,

#if ~ exist( 'sTHR', 'var' ) || isempty(sTHR), sTHR = -Inf; end
# Strength threshold
# Pitch estimates with a strength lower than sTHR are treated as undefined.
# For example, if you set this to 0.2, samples with pitch strength lower than 0.2 will be discarded.
# Default setting: sTHR = -Inf (hence nothing is discarded).
sTHR = -Inf

####################################################################################
){ # End argument list; begin function body
####################################################################################

# Below, each line of the original code is included in a comment next to the corresponding R 'translation'.

####################
# INPUT VALIDATION #
####################

#if ...
#elseif woverlap>1 || woverlap<0
#    error('Window overlap must be between 0 and 1.')
#end
if(woverlap<0 | woverlap>1){stop("Window overlap must be between 0 and 1.")}

# Other input validation should be added in future, constraining each argument to be of the appropriate type.

########################
# PACKAGE DEPENDENCIES #
########################

require("e1071")
require("signal")

#############
# FUNCTIONS #
#############

#function erbs = hz2erbs(hz)
#erbs = 6.44 * ( log2( 229 + hz ) - 7.84 );
hz2erbs = function(hz){ 6.44 * ( log2( 229 + hz ) - 7.84 ) }

#function hz = erbs2hz(erbs)
#hz = ( 2 .^ ( erbs./6.44 + 7.84) ) - 229;
erbs2hz = function(erbs){ ( 2 ^ ( erbs/6.44 + 7.84) ) - 229 }

# Following code modified from 'primest()' function here:
# http://stackoverflow.com/questions/3789968/generate-a-list-of-primes-in-r-up-to-a-certain-number

primes <- function(n){
    p = 2:n
    i = 1
    while( p[i] <= sqrt(n) ){
        p = p[ ( p %% p[i] != 0 ) | ( p==p[i] ) ]
        i = i+1 # Increment
    } # End 'while' loop
    return(p)
} # End definition of 'primes'

VectorNorm = function(input){ norm(as.matrix(input), "F") }
# Code obtained from here:
#http://stackoverflow.com/questions/10933945/how-to-calculate-the-euclidean-norm-of-a-vector-in-r
# 'F' stands for the 'Frobenius' norm

#function S = pitchStrengthOneCandidate( f, NL, pc )
pitchStrengthOneCandidate = function( f, NL, pc ){

#n = fix( f(end)/pc - 0.75 ); % Number of harmonics
n = floor( f[length(f)]/pc - 0.75 )

#if n==0, S=NaN; return, end
if(n==0){S=NaN; return}

#k = zeros( size(f) ); % Kernel
k = rep( 0, times=length(f) )

#% Normalize frequency w.r.t. candidate
#q = f / pc;
q = f / pc

#% Create kernel
#for i = [ 1 primes(n) ]
for(i in c(1,primes(n)) ){ # i=2 # Function 'primes()' defined up above
#    a = abs( q - i );
a = abs( q - i )

#    % Peak's weight
#    p = a < .25; 
p = ( a < 0.25 )

#    k(p) = cos( 2*pi * q(p) );
k[p] = cos( 2*pi * q[p] )

#    % Valleys' weights
#    v = .25 < a & a < .75;
v = ( 0.25 < a ) & ( a < 0.75 )

#    k(v) = k(v) + cos( 2*pi * q(v) ) / 2;
k[v] <- k[v] + ( cos( 2*pi * q[v] ) / 2 )

#end
} # End 'for' loop

#% Apply envelope
#k = k .* sqrt( 1./f  ); 
k = k * sqrt( 1 / f )

#% K+-normalize kernel
#k = k / norm( k(k>0) ); 
k = k / VectorNorm( k[k>0] ) # Function 'VectorNorm()' defined up above

#% Compute pitch strength
#S = k' * NL;
S = k %*% NL # Note that Matlab '*' is R '%*%'
# Dropped the transposition via the apostrophe

return(S)

} # End definition of function 'pitchStrengthOneCandidate()'

#function S = pitchStrengthAllCandidates( f, L, pc )
pitchStrengthAllCandidates = function( f, L, pc ){
#% Create pitch strength matrix
#S = zeros( length(pc), size(L,2) );
S = matrix( 0, nrow=length(pc), ncol=ncol(L) )

#% Define integration regions
#k = ones( 1, length(pc)+1 );
k = rep(1, times=length(pc)+1 )

#for j = 1 : length(k)-1
for( j in 1:(length(k)-1) ){ # j=1

#    k(j+1) = k(j) - 1 + find( f(k(j):end) > pc(j)/4, 1, 'first' );
k[j+1] <- k[j] - 1 + min( which( f[k[j]:length(f)] > pc[j]/4 ) )

#end
} # End for loop

#k = k(2:end);
k = k[2:length(k)]

#% Create loudness normalization matrix
#N = sqrt( flipud( cumsum( flipud(L.*L) ) ) );
LoudnessSquared = L*L
Flipped = LoudnessSquared[nrow(L):1,]
CumulativeSum = matrix( cumsum(Flipped), nrow=nrow(Flipped), ncol=ncol(Flipped) )
N = sqrt( CumulativeSum[nrow(CumulativeSum):1,] )

#for j = 1 : length(pc)
for(j in 1:length(pc)){ # j=1

#    % Normalize loudness
#    n = N(k(j),:);
n = N[ k[j], ]

#    n(n==0) = Inf; % to make zero-loudness equal zero after normalization
# MAINTENANCE HISTORY: Added [this line] to avoid division by zero in [the following line] if loudness equals zero (06/23/2010).
n[n==0] <- Inf

#    NL = L(k(j):end,:) ./ repmat( n, size(L,1)-k(j)+1, 1);
ReplicatedMatrix = matrix(n, byrow=TRUE, nrow=nrow(L)-k[j]+1, ncol=length(n))
# Repmat 'tiles' a matrix. Here, take the horizontal matrix 'n' and tile it vertically 'nrow(L)-k[j]+1' times
NL = L[ ( k[j]:nrow(L) ),] / ReplicatedMatrix

#    % Compute pitch strength
#    S(j,:) = pitchStrengthOneCandidate( f(k(j):end), NL, pc(j) );
S[j,] <- pitchStrengthOneCandidate( f[k[j]:length(f)], NL, pc[j] ) # Function 'pitchStrengthOneCandidate()' defined up above

#end
} # End for loop

return(S)
} # End definition of function 'pitchStrengthAllCandidates()'

######################
# MAIN FUNCTION CODE #
######################

#t = [ 0: dt: length(x)/fs ]'; % Times
t = seq(from=0, by=dt, to=length(x)/fs) # Perhaps do rounding to make sure from/by/to match up?

#% Define pitch candidates
#log2pc = [ log2(plim(1)): dlog2p: log2(plim(2)) ]';
log2pc = seq( from=log2(plim[1]), by=dlog2p, to=log2(plim[2]) ) # Perhaps do rounding to make sure from/by/to match up?

#pc = 2 .^ log2pc;
pc = 2^log2pc

#S = zeros( length(pc), length(t) ); % Pitch strength matrix
S = matrix( 0, nrow=length(pc), ncol=length(t) )

#% Determine P2-WSs
#logWs = round( log2( 8*fs ./ plim ) ); 
logWs = round( log2( 8*fs / plim ) )

#ws = 2.^[ logWs(1): -1: logWs(2) ]; % P2-WSs
ws = 2^seq(from=logWs[1],by=-1,to=logWs[2])

#pO = 8 * fs ./ ws; % Optimal pitches for P2-WSs
pO = 8 * fs / ws

#% Determine window sizes used by each pitch candidate
#d = 1 + log2pc - log2( 8*fs./ws(1) );
d = 1 + log2pc - log2( 8*fs/ws[1] )

#% Create ERB-scale uniformly-spaced frequencies (in Hertz)
#fERBs = erbs2hz([ hz2erbs(min(pc)/4): dERBs: hz2erbs(fs/2) ]');
fERBs = erbs2hz( seq(from=hz2erbs(min(pc)/4), by=dERBs, to=hz2erbs(fs/2)) ) # Functions 'hz2erbs()' and 'erbs2hz()' both defined up above.
# The single quote is ignored here, which does t(). That unnecessarily changes the vector into a matrix.
#for i = 1 : length(ws)
for(i in 1:length(ws)){ # i=4

#    dn = max( 1, round( 8*(1-woverlap) * fs / pO(i) ) ); % Hop size
dn = max( 1, round( 8*(1-woverlap) * fs / pO[i] ) )

#    % Zero pad signal
#    xzp = [ zeros( ws(i)/2, 1 ); x(:); zeros( dn + ws(i)/2, 1 ) ];
xzp = c( rep(0, times=ws[i]/2 ), as.vector(x), rep( 0, times=dn + ws[i]/2 ) )
# This is simplified a bit in order to make this a vector, not a matrix.
# The 'as.vector(x)' is probably not necessary. It (presumably) is already is a vector coming in.

#    % Compute spectrum
#    w = hanning( ws(i) ); % Hann window 
w = hanning.window( ws[i] ) # From package 'e1071'

#    o = max( 0, round( ws(i) - dn ) ); % Window overlap
o = max( c( 0, round( ws[i] - dn ) ) );

#    [ X, f, ti ] = specgram( xzp, ws(i), fs, w, o );
# From http://dali.feld.cvut.cz/ucebna/matlab/toolbox/signal/specgram.html
# [B,f,t] = specgram(a,nfft,Fs)
# B = the spectrogram for the signal
# f = a vector f of frequencies at which the function computes the discrete-time Fourier transform
# t = a vector of scaled times, with length equal to the number of columns of B.
Spectrogram = specgram(x=xzp, n=ws[i], Fs = fs, window=w, overlap=o) # From package 'signal'
X = Spectrogram[[1]]
f = Spectrogram[[2]]
ti = Spectrogram[[3]]

#    % Select candidates that use this window size
#    if length(ws) == 1
#        j=[(pc)]'; k = [];
if(length(ws) == 1){
j = pc # In R, there should be no need to transpose
k = matrix(ncol=0,nrow=0)

#    elseif i == length(ws)
#        j=find(d-i>-1); k=find(d(j)-i<0);
}else{ if(i == length(ws)){
j=which( ( d   -i ) > -1 )
k=which( ( d[j]-i ) <  0 )

#    elseif i==1
#        j=find(d-i<1); k=find(d(j)-i>0);
}else{ if(i==1){
j=which( ( d   -i) < 1 )
k=which( ( d[j]-i) > 0 )

#    else
#        j=find(abs(d-i)<1); k=1:length(j);
}else{
j = which( abs(d-i) < 1 )
k = 1:length(j)

#    end
}}}

#    % Compute loudness at ERBs uniformly-spaced frequencies
#    fERBs = fERBs( find( fERBs > pc(j(1))/4, 1, 'first' ) : end );
fERBs = fERBs[ min( which( fERBs > (pc[j[1]]/4) ) ):length(fERBs) ]

#    L = sqrt( max( 0, interp1( f, abs(X), fERBs, 'spline', 0) ) );
FUN1 = function(n){ # n=10
SplineInterpolate = splinefun( f, abs(X)[,n]);
InterpolatedPoints = SplineInterpolate(fERBs)
# Over-ride the output in order to do 0-extrapolation
InterpolatedPoints[ fERBs<f[1] ] <- 0
InterpolatedPoints[ fERBs>f[length(f)] ] <- 0
return(InterpolatedPoints)
} # End definition of function 'FUN1'
Interpolation = sapply(1:ncol(X),FUN=FUN1)
Interpolation[Interpolation<0] <- 0
L = sqrt( Interpolation )

#    % Compute pitch strength
#    Si = pitchStrengthAllCandidates( fERBs, L, pc(j) );
Si = pitchStrengthAllCandidates( fERBs, L, pc[j] ) # Function 'pitchStrengthAllCandidates()' defined up above

#    % Interpolate pitch strength at desired times
#    if size(Si,2) > 1
if( ncol(Si) > 1 ){
#        warning off MATLAB:interp1:NaNinY
#        Si = interp1( ti, Si', t, 'linear', NaN )';
#        warning on MATLAB:interp1:NaNinY
# No need to turn off warnings in R
FUN2 = function(n){ # n=10
LinearInterpolate = approxfun( ti, t(Si)[,n]);
InterpolatedPoints = LinearInterpolate(t)
# Over-ride the output in order to do NaN-extrapolation
InterpolatedPoints[ t<ti[1] ] <- NaN
InterpolatedPoints[ t>ti[length(ti)] ] <- NaN
return(InterpolatedPoints)
} # End definition of function 'FUN2'
Si = t( sapply(X=1:ncol(t(Si)),FUN=FUN2) )
# Must transpose here because of how sapply() works (relative to the transposition of the underlying matrix).

#    else
}else{

#        Si = repmat( NaN, length(Si), length(t) );
Si = matrix( NaN, nrow=length(Si), ncol=length(t) )
# In this case, Si is a one-column matrix, hence length() should be safe.

#    end
} # End if/else

#    % Add pitch strength to combination
#    lambda = d( j(k) ) - i;
lambda = d[ j[k] ] - i

#    mu = ones( size(j) );
mu = rep( 1, times=length(j) )

#    mu(k) = 1 - abs( lambda );
mu[k] <- 1 - abs( lambda )

#    S(j,:) = S(j,:) + repmat(mu,1,size(Si,2)) .* Si;
S[j,] <- S[j,] + ( matrix(mu,nrow=length(mu),ncol=ncol(Si)) * Si )

####################################################################################
#end
} # End 'i' loop
####################################################################################

#% Fine tune pitch using parabolic interpolation
#p = repmat( NaN, size(S,2), 1 );
#s = repmat( NaN, size(S,2), 1 );
p <- s <-rep( NaN, times=ncol(S))

#for j = 1 : size(S,2)
for(j in 2:ncol(S)){ # j=1000

#    [ s(j), i ] = max( S(:,j), [], 1 );
s[j] <- max( S[,j] )
i = which.max( S[,j] )

#    if s(j) < sTHR, continue, end
if( s[j] < sTHR ){ next }

#    if i == 1 || i == length(pc)
#        p(j) = pc(i);
if( i == 1 | i == length(pc) ){
p[j] <- pc[i]

#    else
}else{

#        I = i-1 : i+1;
I = (i-1) : (i+1)

#        tc = 1 ./ pc(I);
tc = 1 / pc[I]

#        ntc = ( tc/tc(2) - 1 ) * 2*pi;
ntc = ( tc/tc[2] - 1 ) * 2*pi

#        c = polyfit( ntc, S(I,j), 2 );
# http://stackoverflow.com/questions/3822535/fitting-polynomial-model-to-data-in-r
Fit = lm( S[I,j] ~ stats:::poly(ntc, 2, raw=TRUE ) ) # Indeed, it must be raw=TRUE

#        ftc = 1 ./ 2.^[ log2(pc(I(1))): 1/12/100: log2(pc(I(3))) ];
ftc = 1 / (2^seq( from=log2(pc[I[1]]), by=1/12/100, to=log2(pc[I[3]]) ) )

#        nftc = ( ftc/tc(2) - 1 ) * 2*pi;
nftc = ( ftc/tc[2] - 1 ) * 2*pi

#        [s(j) k] = max( polyval( c, nftc ) );
Prediction = makepredictcall(Fit, nftc)
k = which.max( Prediction )
# s(j) isn't used, hence just ignore it

#        p(j) = 2 ^ ( log2(pc(I(1))) + (k-1)/12/100 );
p[j] <- 2 ^ ( log2(pc[I[1]]) + (k-1)/12/100 )

#    end
} # End if/else statement

####################################################################################
#end
} # End for loop

return(data.frame(Time=t[-1],Frequency=p[-1],Strength=s[-1])) # Drop first row (time=0)

####################################################################################
} # End definition of function 'swipep()'

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
