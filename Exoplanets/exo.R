library('animation')
library('scatterplot3d')

exo = read.csv('oec.csv')

#Get rid of planets with missing data...
ex = is.na(exo$RightAscension) | is.na(exo$Declination) | is.na(exo$DistFromSunParsec) | is.na(exo$RadiusJpt)
exo = exo[!ex,]

#Get rid of the planets with really extreme distances or sizes (purely for aesthetic reasons)...
exo = exo[exo$DistFromSunParsec < 2000,]
exo = exo[exo$RadiusJpt < 6,]

#Change to characters...
exo$RightAscension = as.character(exo$RightAscension)
exo$Declination = as.character(exo$Declination)


#Strip out relevant data and convert from RA and declination to cartesian coordiates...
#Conversion taken from http://fmwriters.com/Visionback/Issue14/wbputtingstars.htm

i=1
exo$A = 0

while (i<length(exo$RightAscension)) {

    temp = strsplit(exo$RightAscension, split = " ")[[i]]
    temp_d = strsplit(exo$Declination, split = " ")[[i]]
    exo$A[i] = (as.numeric(temp[1])*15) + (as.numeric(temp[2])*0.25) + (as.numeric(temp[3])*0.004166)
    exo$B[i] = (abs(as.numeric(temp_d[1])) + (as.numeric(temp_d[2])/60) + (as.numeric(temp_d[3])/3600)) * sign(as.numeric(temp_d[1]))
    exo$X[i] = (exo$DistFromSunParsec[i] * cos(exo$B[i])) * cos(exo$A[i])
    exo$Y[i] = (exo$DistFromSunParsec[i] * cos(exo$B[i])) * sin(exo$A[i])
    exo$Z[i] = exo$DistFromSunParsec[i] * sin(exo$B[i])
    
    i=i+1
    
}


#Plot the scatterplot and produce an animation (changing the angle with each image)...

y=0

saveGIF(while (y < 360) {
    
    scatterplot3d(exo$X, exo$Y, exo$Z, pch=16, 
                         highlight.3d=TRUE, 
                         type="p", 
                         main="Local Neighbourhood", 
                         axis = F,
                         grid = F,
                         box = T,
                         angle = y, 
                         cex.symbols = exo$RadiusJpt)
    
    y=y+1

}, movie.name = "exo.gif", interval = 0.1, convert = "convert", ani.width = 800, 
ani.height = 800)