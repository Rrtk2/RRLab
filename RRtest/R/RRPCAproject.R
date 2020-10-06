#' RRnorm
#'
#' This function normalizes dataset and offests/scales if needed.
#'
#' @param X Object that shoulb be normalized.
#' @param Scalar Object that shoulb be scaled by.
#' @param Offset Object that shoulb be Offseted by.
#' 
#' @return Normalized object
#' @examples
#' Object=seq(from = -1,to = 1,by = 0.1)^2
#' NormObject = RRnorm(Object, Scalar = 2, Offset = -1)
#' plot(NormObject, col = "red")
#' points(Object, col="blue")
#' @export

PCAproject = function(){

Data = iris[,-5]

PCA = prcomp(Data)
PCAcontrastX = 1
PCAcontrastY = 2

# Get relevant numbers from PCA
PCAx = PCA$x[,PCAcontrastX]
PCAy = PCA$x[,PCAcontrastY]

# Loadings object; scaled to max varaiance of PC
PCALoadingX = PCA$rotation[,PCAcontrastX]*IQR(PCAx)
PCALoadingY = PCA$rotation[,PCAcontrastY]*IQR(PCAy)
Loadings = data.frame(x = PCALoadingX, y = PCALoadingY)

# Get variance
TotVar = round((PCA$sdev^2/sum(PCA$sdev^2))*100,2)

# PCASamplePlot
PCASamplePlot  = ggplot2::ggplot() + 
ggplot2::theme(legend.position="bottom")+
ggplot2::geom_point(Data, mapping = ggplot2::aes(PCAx, PCAy)) +
ggplot2::xlim(min(PCAx)*1.1, max(PCAx)*1.1) +
ggplot2::ylim(min(PCAy)*1.1, max(PCAy)*1.1) +
ggplot2::labs( x = paste0("PC",PCAcontrastX," (",TotVar[PCAcontrastX],"%)"), y = paste0("PC",PCAcontrastY," (",TotVar[PCAcontrastY],"%)")) + 
ggplot2::ggtitle("Optimal unsupervised contrast") 
PCASamplePlot
#scale(newdata, PCA$center, PCA$scale) %*% pca$rotation	
#
#diag(PCA$center)
#PCA$x %*% t(PCA$rotation) %*% t(diag(PCA$center))


#-----------------------------------------------------------------------------------------------------#
#
#----------------							Project!-------------------------------------------------------------------------------------#

# Get the contrastof interst, increaseto capture more variation
xCont = c(PCAcontrastX,PCAcontrastY)

# extrapolate extremes; for identifying important variables
PV_LT = c(min(PCA$x[,PCAcontrastX]),max(PCA$x[,PCAcontrastY]))
PV_RT = c(max(PCA$x[,PCAcontrastX]),max(PCA$x[,PCAcontrastY]))
PV_LB = c(min(PCA$x[,PCAcontrastX]),min(PCA$x[,PCAcontrastY]))
PV_RB = c(max(PCA$x[,PCAcontrastX]),min(PCA$x[,PCAcontrastY]))

# make a object to store extremes
Extremes = rbind(PV_LT,PV_RT,PV_LB,PV_RB)

# Transform from latent space to real space
Xhat = Extremes %*% t(PCA$rotation[,xCont])


#Xhat = PCA$x[,xCont] %*% t(PCA$rotation[,xCont])
Xhat = scale(Xhat, center = -colMeans(Data), scale = FALSE)

HT = Xhat[1,]-Xhat[2,] #LT-RT -> Horizontal Top (right)
HB = Xhat[3,]-Xhat[4,] #LB-RB -> Horizontal Bottom (right)
VL = Xhat[1,]-Xhat[3,] #LT-LB -> Vertical Left (down)
VR = Xhat[2,]-Xhat[4,] #RT-RB -> Vertical Right (down)

HT = round(HT[order(abs(HT),decreasing = T)],3)
HB = round(HB[order(abs(HB),decreasing = T)],3)
VL = round(VL[order(abs(VL),decreasing = T)],3)
VR = round(VR[order(abs(VR),decreasing = T)],3)





}