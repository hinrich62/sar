# Program to estimate standard error and CV
# of a smolt-to-adult ratio (SAR). Adult recoveries
# are assumed to be binomially distributed
#input parameters
#Nr number of juveniles released
#Nd number of adults detected
SAR<-function(Nr,Nd){
if(Nd>Nr){
warning("The number of adults detected (Nd) must be no greater than the number of juveniles released (Nr)")
return(NULL)}
sar<-Nd/Nr
se<-sar*sqrt(1/Nd-1/Nr)
cv<-sqrt(1/Nd-1/Nr)
return(list(Nr=Nr,Nd=Nd,sar=sar,se=se,cv=cv))
}
#outputs
#sar survival rate estimate (SAR)
#se standard error
#cv coefficient of variation