pO<- read.csv("C:/Users/renan/OneDrive/Desktop/R analysis/pitchOnly.csv",nrows=150)

sP<- read.csv("C:/Users/renan/OneDrive/Desktop/R analysis/spatialPitch.csv",nrows=150)

cB<- read.csv("C:/Users/renan/OneDrive/Desktop/R analysis/channelBasedPitch.csv",nrows=150)

aP<- read.csv("C:/Users/renan/OneDrive/Desktop/R analysis/alternatedPitch.csv",nrows=150)

sA<- read.csv("C:/Users/renan/OneDrive/Desktop/R analysis/spatialAlternatedPitch.csv",nrows=150)

h1<- read.csv("C:/Users/renan/OneDrive/Desktop/R analysis/spatialHarmonicsOneD.csv",nrows=150)

h2<- read.csv("C:/Users/renan/OneDrive/Desktop/R analysis/spatialHarmonicsTwoD.csv",nrows=150)

sample<-matrix(c(pO$timeTaken, sP$timeTaken, cB$timeTaken, aP$timeTaken, sA$timeTaken, h1$timeTaken, h2$timeTaken),ncol=7)

friedman.test(sample)

boxplot(sample, outline=FALSE, ylab ="Time Taken (s)", xlab ="Sonification Method", names = c("pO","sP","cB","aP","sA","h1","h2"))


sample2<-matrix(c(pO$totalDistance, sP$totalDistance, cB$totalDistance, aP$totalDistance, sA$totalDistance, h1$totalDistance, h2$totalDistance),ncol=7)

friedman.test(sample2)

boxplot(sample2, outline=FALSE, ylab ="Average 2D Distance from Target (pixels)", xlab ="Sonification Method", names = c("pO","sP","cB","aP","sA","h1","h2"))


sample3<-matrix(c(pO$Xdist, sP$Xdist, cB$Xdist, aP$Xdist, sA$Xdist, h1$Xdist, h2$Xdist),ncol=7)

friedman.test(sample3)

boxplot(sample3, outline=FALSE, ylab ="Average X-Axis Distance from Target (pixels)", xlab ="Sonification Method", names = c("pO","sP","cB","aP","sA","h1","h2"))



sample4<-matrix(c(pO$Ydist, sP$Ydist, cB$Ydist, aP$Ydist, sA$Ydist, h1$Ydist, h2$Ydist),ncol=7)

friedman.test(sample4)

boxplot(sample4, outline=FALSE, ylab ="Average Y-Axis Distance from Target (pixels)", xlab ="Sonification Method", names = c("pO","sP","cB","aP","sA","h1","h2"))

wilcox.test(pO$timeTaken, sP$timeTaken)
wilcox.test(pO$timeTaken, cB$timeTaken)
wilcox.test(pO$timeTaken, aP$timeTaken)
wilcox.test(pO$timeTaken, sA$timeTaken)
wilcox.test(pO$timeTaken, h1$timeTaken)
wilcox.test(pO$timeTaken, h2$timeTaken)

wilcox.test(sP$timeTaken, cB$timeTaken)
wilcox.test(sP$timeTaken, aP$timeTaken)
wilcox.test(sP$timeTaken, sA$timeTaken)
wilcox.test(sP$timeTaken, h1$timeTaken)
wilcox.test(sP$timeTaken, h2$timeTaken)

wilcox.test(cB$timeTaken, aP$timeTaken)
wilcox.test(cB$timeTaken, sA$timeTaken)
wilcox.test(cB$timeTaken, h1$timeTaken)
wilcox.test(cB$timeTaken, h2$timeTaken)

wilcox.test(aP$timeTaken, sA$timeTaken)
wilcox.test(aP$timeTaken, h1$timeTaken)
wilcox.test(aP$timeTaken, h2$timeTaken)

wilcox.test(sA$timeTaken, h1$timeTaken)
wilcox.test(sA$timeTaken, h2$timeTaken)

wilcox.test(h1$timeTaken, h2$timeTaken)

wilcox.test(pO$totalDistance, sP$totalDistance)
wilcox.test(pO$totalDistance, cB$totalDistance)
wilcox.test(pO$totalDistance, aP$totalDistance)
wilcox.test(pO$totalDistance, sA$totalDistance)
wilcox.test(pO$totalDistance, h1$totalDistance)
wilcox.test(pO$totalDistance, h2$totalDistance)

wilcox.test(sP$totalDistance, cB$totalDistance)
wilcox.test(sP$totalDistance, aP$totalDistance)
wilcox.test(sP$totalDistance, sA$totalDistance)
wilcox.test(sP$totalDistance, h1$totalDistance)
wilcox.test(sP$totalDistance, h2$totalDistance)

wilcox.test(cB$totalDistance, aP$totalDistance)
wilcox.test(cB$totalDistance, sA$totalDistance)
wilcox.test(cB$totalDistance, h1$totalDistance)
wilcox.test(cB$totalDistance, h2$totalDistance)

wilcox.test(aP$totalDistance, sA$totalDistance)
wilcox.test(aP$totalDistance, h1$totalDistance)
wilcox.test(aP$totalDistance, h2$totalDistance)

wilcox.test(sA$totalDistance, h1$totalDistance)
wilcox.test(sA$totalDistance, h2$totalDistance)

wilcox.test(h1$totalDistance, h2$totalDistance)

wilcox.test(pO$Xdist, sP$Xdist)
wilcox.test(pO$Xdist, cB$Xdist)
wilcox.test(pO$Xdist, aP$Xdist)
wilcox.test(pO$Xdist, sA$Xdist)
wilcox.test(pO$Xdist, h1$Xdist)
wilcox.test(pO$Xdist, h2$Xdist)

wilcox.test(sP$Xdist, cB$Xdist)
wilcox.test(sP$Xdist, aP$Xdist)
wilcox.test(sP$Xdist, sA$Xdist)
wilcox.test(sP$Xdist, h1$Xdist)
wilcox.test(sP$Xdist, h2$Xdist)

wilcox.test(cB$Xdist, aP$Xdist)
wilcox.test(cB$Xdist, sA$Xdist)
wilcox.test(cB$Xdist, h1$Xdist)
wilcox.test(cB$Xdist, h2$Xdist)

wilcox.test(aP$Xdist, sA$Xdist)
wilcox.test(aP$Xdist, h1$Xdist)
wilcox.test(aP$Xdist, h2$Xdist)

wilcox.test(sA$Xdist, h1$Xdist)
wilcox.test(sA$Xdist, h2$Xdist)

wilcox.test(h1$Xdist, h2$Xdist)

wilcox.test(pO$Ydist, sP$Ydist)
wilcox.test(pO$Ydist, cB$Ydist)
wilcox.test(pO$Ydist, aP$Ydist)
wilcox.test(pO$Ydist, sA$Ydist)
wilcox.test(pO$Ydist, h1$Ydist)
wilcox.test(pO$Ydist, h2$Ydist)

wilcox.test(sP$Ydist, cB$Ydist)
wilcox.test(sP$Ydist, aP$Ydist)
wilcox.test(sP$Ydist, sA$Ydist)
wilcox.test(sP$Ydist, h1$Ydist)
wilcox.test(sP$Ydist, h2$Ydist)

wilcox.test(cB$Ydist, aP$Ydist)
wilcox.test(cB$Ydist, sA$Ydist)
wilcox.test(cB$Ydist, h1$Ydist)
wilcox.test(cB$Ydist, h2$Ydist)

wilcox.test(aP$Ydist, sA$Ydist)
wilcox.test(aP$Ydist, h1$Ydist)
wilcox.test(aP$Ydist, h2$Ydist)

wilcox.test(sA$Ydist, h1$Ydist)
wilcox.test(sA$Ydist, h2$Ydist)

wilcox.test(h1$Ydist, h2$Ydist)

h2HS<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialHarmonicsTwoDvsMTHigh_Some.csv",nrows=90)

h2LN<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialHarmonicsTwoDvsMTLow_None.csv",nrows=80)

wilcox.test(h2HS$totalDistance, h2LN$totalDistance)
wilcox.test(h2HS$timeTaken, h2LN$timeTaken)
wilcox.test(h2HS$Xdist, h2LN$Xdist)
wilcox.test(h2HS$Ydist, h2LN$Ydist)

h1HS<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialHarmonicsOneDvsMTHS.csv",nrows=70)

h1LN<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialHarmonicsOneDvsMTLN.csv",nrows=90)

wilcox.test(h1HS$totalDistance, h1LN$totalDistance)
wilcox.test(h1HS$timeTaken, h1LN$timeTaken)
wilcox.test(h1HS$Xdist, h1LN$Xdist)
wilcox.test(h1HS$Ydist, h1LN$Ydist)

sAHS<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialAlternatedPitchvsMTHS.csv",nrows=80)

sALN<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialAlternatedPitchvsMTLN.csv",nrows=60)

wilcox.test(sAHS$totalDistance, sALN$totalDistance)
wilcox.test(sAHS$timeTaken, sALN$timeTaken)
wilcox.test(sAHS$Xdist, sALN$Xdist)
wilcox.test(sAHS$Ydist, sALN$Ydist)

aPHS<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/alternatedPitchMTHS.csv",nrows=70)

aPLN<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/alternatedPitchMTLN.csv",nrows=77)

wilcox.test(aPHS$totalDistance, aPLN$totalDistance)
wilcox.test(aPHS$timeTaken, aPLN$timeTaken)
wilcox.test(aPHS$Xdist, aPLN$Xdist)
wilcox.test(aPHS$Ydist, aPLN$Ydist)

cBHS<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/channelBasedPitchMTHS.csv",nrows=80)

cBLN<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/channelBasedPitchMTLN.csv",nrows=95)

wilcox.test(cBHS$totalDistance, cBLN$totalDistance)
wilcox.test(cBHS$timeTaken, cBLN$timeTaken)
wilcox.test(cBHS$Xdist, cBLN$Xdist)
wilcox.test(cBHS$Ydist, cBLN$Ydist)

sPHS<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialPitchMTHS.csv",nrows=118)

sPLN<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialPitchMTLN.csv",nrows=60)

wilcox.test(sPHS$totalDistance, sPLN$totalDistance)
wilcox.test(sPHS$timeTaken, sPLN$timeTaken)
wilcox.test(sPHS$Xdist, sPLN$Xdist)
wilcox.test(sPHS$Ydist, sPLN$Ydist)

pOHS<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/pitchOnlyMTHS.csv",nrows=90)

pOLN<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/pitchOnlyMTLN.csv",nrows=80)

wilcox.test(pOHS$totalDistance, pOLN$totalDistance)
wilcox.test(pOHS$timeTaken, pOLN$timeTaken)
wilcox.test(pOHS$Xdist, pOLN$Xdist)
wilcox.test(pOHS$Ydist, pOLN$Ydist)

h21x<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialHarmonicsTwoD1x.csv",nrows=100)

h22x<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialHarmonicsTwoD2x.csv",nrows=60)

wilcox.test(h21x$totalDistance, h22x$totalDistance)
wilcox.test(h21x$timeTaken, h22x$timeTaken)
wilcox.test(h21x$Xdist, h22x$Xdist)
wilcox.test(h21x$Ydist, h22x$Ydist)

h11x<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialHarmonicsOneD1x.csv",nrows=110)

h12x<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialHarmonicsOneD2x.csv",nrows=50)

wilcox.test(h11x$totalDistance, h12x$totalDistance)
wilcox.test(h11x$timeTaken, h12x$timeTaken)
wilcox.test(h11x$Xdist, h12x$Xdist)
wilcox.test(h11x$Ydist, h12x$Ydist)

sA1x<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialAlternatedPitch1x.csv",nrows=90)

sA2x<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialAlternatedPitch2x.csv",nrows=50)

wilcox.test(sA1x$totalDistance, sA2x$totalDistance)
wilcox.test(sA1x$timeTaken, sA2x$timeTaken)
wilcox.test(sA1x$Xdist, sA2x$Xdist)
wilcox.test(sA1x$Ydist, sA2x$Ydist)

aP1x<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/alternatedPitch1x.csv",nrows=99)

aP2x<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/alternatedPitch2x.csv",nrows=48)

wilcox.test(aP1x$totalDistance, aP2x$totalDistance)
wilcox.test(aP1x$timeTaken, aP2x$timeTaken)
wilcox.test(aP1x$Xdist, aP2x$Xdist)
wilcox.test(aP1x$Ydist, aP2x$Ydist)

cB1x<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/channelBasedPitch1x.csv",nrows=125)

cB2x<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/channelBasedPitch2x.csv",nrows=40)

wilcox.test(cB1x$totalDistance, cB2x$totalDistance)
wilcox.test(cB1x$timeTaken, cB2x$timeTaken)
wilcox.test(cB1x$Xdist, cB2x$Xdist)
wilcox.test(cB1x$Ydist, cB2x$Ydist)

sP1x<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialPitch1x.csv",nrows=98)

sP2x<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialPitch2x.csv",nrows=60)

wilcox.test(sP1x$totalDistance, sP2x$totalDistance)
wilcox.test(sP1x$timeTaken, sP2x$timeTaken)
wilcox.test(sP1x$Xdist, sP2x$Xdist)
wilcox.test(sP1x$Ydist, sP2x$Ydist)

pO1x<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/pitchOnly1x.csv",nrows=80)

pO2x<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/pitchOnly2x.csv",nrows=60)

wilcox.test(pO1x$totalDistance, pO2x$totalDistance)
wilcox.test(pO1x$timeTaken, pO2x$timeTaken)
wilcox.test(pO1x$Xdist, pO2x$Xdist)
wilcox.test(pO1x$Ydist, pO2x$Ydist)

h2BVI<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialHarmonicsTwoDBVI.csv",nrows=50)

h2SIGHTED<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialHarmonicsTwoDSIGHTED.csv",nrows=120)

wilcox.test(h2BVI$totalDistance, h2SIGHTED$totalDistance)
wilcox.test(h2BVI$timeTaken, h2SIGHTED$timeTaken)
wilcox.test(h2BVI$Xdist, h2SIGHTED$Xdist)
wilcox.test(h2BVI$Ydist, h2SIGHTED$Ydist)

h1BVI<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialHarmonicsOneDBVI.csv",nrows=20)

h1SIGHTED<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialHarmonicsOneDSIGHTED.csv",nrows=160)

wilcox.test(h1BVI$totalDistance, h1SIGHTED$totalDistance)
wilcox.test(h1BVI$timeTaken, h1SIGHTED$timeTaken)
wilcox.test(h1BVI$Xdist, h1SIGHTED$Xdist)
wilcox.test(h1BVI$Ydist, h1SIGHTED$Ydist)

sABVI<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialAlternatedPitchBVI.csv",nrows=20)

sASIGHTED<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialAlternatedPitchSIGHTED.csv",nrows=150)

wilcox.test(sABVI$totalDistance, sASIGHTED$totalDistance)
wilcox.test(sABVI$timeTaken, sASIGHTED$timeTaken)
wilcox.test(sABVI$Xdist, sASIGHTED$Xdist)
wilcox.test(sABVI$Ydist, sASIGHTED$Ydist)


aPBVI<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/alternatedPitchBVI.csv",nrows=30)

aPSIGHTED<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/alternatedPitchSIGHTED.csv",nrows=138)

wilcox.test(aPBVI$totalDistance, aPSIGHTED$totalDistance)
wilcox.test(aPBVI$timeTaken, aPSIGHTED$timeTaken)
wilcox.test(aPBVI$Xdist, aPSIGHTED$Xdist)
wilcox.test(aPBVI$Ydist, aPSIGHTED$Ydist)

cBBVI<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/channelBasedPitchBVI.csv",nrows=40)

cBSIGHTED<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/channelBasedPitchSIGHTED.csv",nrows=136)

wilcox.test(cBBVI$totalDistance, cBSIGHTED$totalDistance)
wilcox.test(cBBVI$timeTaken, cBSIGHTED$timeTaken)
wilcox.test(cBBVI$Xdist, cBSIGHTED$Xdist)
wilcox.test(cBBVI$Ydist, cBSIGHTED$Ydist)

sPBVI<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialPitchBVI.csv",nrows=60)

sPSIGHTED<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/spatialPitchSIGHTED.csv",nrows=118)

wilcox.test(sPBVI$totalDistance, sPSIGHTED$totalDistance)
wilcox.test(sPBVI$timeTaken, sPSIGHTED$timeTaken)
wilcox.test(sPBVI$Xdist, sPSIGHTED$Xdist)
wilcox.test(sPBVI$Ydist, sPSIGHTED$Ydist)

pOBVI<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/pitchOnlyBVI.csv",nrows=60)

pOSIGHTED<- read.csv("C:/Users/renan/hand-mouse-sonification/R analysis/pitchOnlySIGHTED.csv",nrows=110)

wilcox.test(pOBVI$totalDistance, pOSIGHTED$totalDistance)
wilcox.test(pOBVI$timeTaken, pOSIGHTED$timeTaken)
wilcox.test(pOBVI$Xdist, pOSIGHTED$Xdist)
wilcox.test(pOBVI$Ydist, pOSIGHTED$Ydist)