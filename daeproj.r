setwd("D:/IIT JODHPUR/sem 4/Design & Analysis of Experiments/project")

Data = read.csv("DAE_PROJECT.CSV", header=T,stringsAsFactors=T)
Data
str(Data)

placement = matrix(c(9, 10.5, 16, 18, 24, 21, 28, 32, 13, 14,
                     16, 19, 38, 42, 46, 53, 10, 9, 14, 19, 28,
                     32, 40, 28, 13.5, 18, 14, 18, 35, 38, 40, 62), byrow=T,ncol=1)


dimnames(placement) = list(c("(1)", "a", "b", "ab", "c", "ac", "bc", "abc", "d", "ad", "bd",
                              "abd", "cd", "acd", "bcd", "abcd", "e", "ae", "be", "abe",
                              "ce", "ace", "bce", "abce", "de", "ade", "bde", "abde", "cde",
                              "acde", "bcde", "abcde"), c("Yield"))



A = rep(c(-1,1),16)
B =rep(c(-1,-1,1,1),8)
C= c(rep(-1,4),rep(1,4),rep(-1,4),rep(1,4),rep(-1,4),rep(1,4),rep(-1,4),rep(1,4))
D=c(rep(-1,8),rep(1,8),rep(-1,8),rep(1,8))
E=c(rep(-1,16), rep(1,16))
data.rate=data.frame(A,B,C,D,E,placement)
data.rate

######################################

I=c(rep(1,32))
AB = A*B
AC = A*C
BC = B*C
ABC = A*B*C
AD=A*D
BD=B*D
ABD=A*B*D
CD=C*D
ACD=A*C*D
BCD=B*C*D
ABCD=A*B*C*D

AE = A*E
BE = B*E
CE = C*E
DE = D*E

ABE = A*B*E
ACE = A*C*E
BCE = B*C*E
ABCE = A*B*C*E
ADE = A*D*E
BDE = B*D*E
ABDE = A*B*D*E
CDE = C*D*E
ACDE = A*C*D*E
BCDE = B*C*D*E
ABCDE = A*B*C*D*E


#Design matrix

Design.matrix=cbind(I, A,B,AB,C, AC,BC,ABC,D,AD,BD,ABD,
                    CD,ACD,BCD,ABCD,E,AE,BE,ABE,CE,ACE,BCE,ABCE,DE,ADE,BDE,ABDE,CDE,ACDE,BCDE,ABCDE, placement)
Design.matrix



################## Interaction and Main effects  ########################

n = 1
Feff = t(placement) %*% cbind(A,B,AB,C, AC,BC,ABC,D,AD,BD,ABD,CD,ACD,BCD,ABCD,
                               E,AE,BE,ABE,CE,ACE,BCE,ABCE,DE,ADE,BDE,ABDE,CDE,ACDE,BCDE,ABCDE)/(16*n)
Ieff=t(placement) %*% cbind(I)/(32*n)
eff=cbind(Ieff,Feff)
eff
Summary = rbind( cbind(I,A,B,AB,C, AC,BC,ABC,D,AD,BD,ABD,CD,ACD,BCD,ABCD,
                       E,AE,BE,ABE,CE,ACE,BCE,ABCE,DE,ADE,BDE,ABDE,CDE,ACDE,BCDE,ABCDE),eff)
dimnames(Summary)[[1]] = c(dimnames(placement)[[1]],"Effect")
Summary

#### HALF NORMAL AND QQ PLOT ######


library(unrepx)
G=Design.matrix[,33]
pilotEff = yates(G, labels = c("A","B","C","D","E")) 
pilotEff
hnplot(pilotEff,ID=0)
qqnorm(pilotEff)

############ CENTER POINT TEST ######################3
M_data= rbind(data.rate,
              c(0, 0, 0, 0,0, 20),
              c(0, 0, 0, 0,0,18.5),
              c(0, 0, 0, 0,0,17),
              c(0, 0, 0, 0,0, 18)
)
M_data
Mod1=lm(placement~A*B*C*D*E, data=M_data)
anova(Mod1)


########################  ANOVA Model ##############
data= c(t(placement))
Af= rep(as.factor(A), rep(1,32))
Bf= rep(as.factor(B), rep(1,32))
Cf= rep(as.factor(C), rep(1,32))
Df= rep(as.factor(D), rep(1,32))
Ef= rep(as.factor(E), rep(1,32))
data.ctc=data.frame(Af,Bf,Cf,Df,Ef, data)
data.ctc
data.aov = aov(data ~ Bf*Cf*Df, data=data.ctc)
summary(data.aov)


############ Main effect and interactions ###############

lm.rate=lm(placement ~ B*C*D, data=data.rate)
summary(lm.rate)
library(DoE.base)
library(FrF2)
MEPlot(lm.rate)
IAPlot(lm.rate)


#######################################################################

################# Reorganize the data replication wise ################


Fil= matrix(c(9, 10.5,10, 9, 16, 18,14, 19, 24, 21,28, 32, 28, 32,40, 28, 13, 14,13.5, 18,
              16, 19,14, 18, 38, 42, 35, 38,46, 53, 40, 62),byrow=T,ncol=4)
dimnames(Fil) = list(c("(1)","b","c","bc","d","bd","cd","bcd"),
                     c("Rep1","Rep2","Rep3","Rep4"))
Fil
B= rep(c(-1,1),4)
C =rep(c(-1,-1,1,1),2)
D= c(rep(-1,4),rep(1,4))
Total = apply(Fil,1,sum)

data.rate2=data.frame(B,C,D,Total)
data.rate2

I=c(rep(1,8))
BC = B*C
BD=B*D
CD=C*D
BCD=B*C*D
Design.matrix2=cbind(I, B,C, BC,D,BD,CD,BCD,Total)
Design.matrix2


########################  ANOVA Model ##############

Frate= c(t(Fil))
Bf= rep(as.factor(B),rep(4,8))
Cf= rep(as.factor(C),rep(4,8))
Df= rep(as.factor(D),rep(4,8))
data.mat=data.frame(Bf,Cf,Df, Frate)
data.mat


Fil.av=aov(Frate ~ Bf*Cf*Df, data=data.mat)
summary(Fil.av)