#' RRFEDataset
#'
#' This function sets dataset for RRFE
#'
#' @return dataframe to test the RRFE function to default.
#' @examples
#' dataset1 = RRFEDataset(1)
#' dataset2 = RRFEDataset(2)
#' dataset3 = RRFEDataset(3)
#' @export

RRFEDataset = function(SelectDataSet = 1) {
	if(SelectDataSet==1){
		sampleNr = 2000
		dataset = as.data.frame(matrix(nrow = sampleNr,ncol = 0))
		rownames(dataset) = paste0("S",as.character(1:dim(dataset)[1]))
		dataset$f1 = seq(-1,1,2/(sampleNr-1))
		dataset$f2 = dataset$f1^2
		dataset$f3 = rnorm(sampleNr)
		dataset$f4 = -rnorm(sampleNr)
		dataset$f5 = rnorm(sampleNr)
		dataset$f6 = rnorm(sampleNr)
		dataset$f7 = -rnorm(sampleNr)
		dataset$f8 = rnorm(sampleNr)
		dataset$f9 = rnorm(sampleNr)
		dataset$f71 = -rnorm(sampleNr)
		dataset$f81 = rnorm(sampleNr)
		dataset$f91 = rnorm(sampleNr)
		dataset$f71 = -rnorm(sampleNr)
		dataset$f81 = rnorm(sampleNr)
		dataset$f91 = rnorm(sampleNr)
		dataset$f72 = -rnorm(sampleNr)
		dataset$f82 = rnorm(sampleNr)
		dataset$f92 = rnorm(sampleNr)
		dataset$f721 = -rnorm(sampleNr)
		dataset$f821 = rnorm(sampleNr)
		dataset$f921 = rnorm(sampleNr)
		dataset$f712 = -rnorm(sampleNr)
		dataset$f812 = rnorm(sampleNr)
		dataset$f912 = rnorm(sampleNr)
		dataset$f32 = rnorm(sampleNr)
		dataset$f42 = -rnorm(sampleNr)
		dataset$f52 = rnorm(sampleNr)
		dataset$f62 = rnorm(sampleNr)
		dataset$f72 = -rnorm(sampleNr)
		dataset$f82 = rnorm(sampleNr)
		dataset$f92 = rnorm(sampleNr)
		dataset$f712 = -rnorm(sampleNr)
		dataset$f812 = rnorm(sampleNr)
		dataset$f912 = rnorm(sampleNr)
		dataset$f712 = -rnorm(sampleNr)
		dataset$f8122 = rnorm(sampleNr)
		dataset$f912 = rnorm(sampleNr)
		dataset$f722 = -rnorm(sampleNr)
		dataset$f822 = rnorm(sampleNr)
		dataset$f922 = rnorm(sampleNr)
		dataset$f7212 = -rnorm(sampleNr)
		dataset$f8212 = rnorm(sampleNr)
		dataset$f9212 = rnorm(sampleNr)
		dataset$f7122 = -rnorm(sampleNr)
		dataset$f8122 = rnorm(sampleNr)
		dataset$f9122 = rnorm(sampleNr)

		Classdefiner = dataset$f1*dataset$f2-dataset$f9*0.05
		plot(Classdefiner)
		dataset$Class = as.factor((Classdefiner>=0.0)+1)
		dataset <<- dataset
	}

	if(SelectDataSet==2){
		sampleNr = 2000
		dataset = as.data.frame(matrix(nrow = sampleNr,ncol = 0))
		rownames(dataset) = paste0("S",as.character(1:dim(dataset)[1]))
		dataset$f1 = seq(-1,-0/(sampleNr-1))
		dataset$f2 = dataset$f1^2
		dataset$f3 = rnorm(sampleNr)
		dataset$f4 = -rnorm(sampleNr)
		dataset$f5 = rnorm(sampleNr)
		dataset$f6 = rnorm(sampleNr)
		dataset$f7 = -rnorm(sampleNr)
		dataset$f8 = rnorm(sampleNr)
		dataset$f9 = rnorm(sampleNr)
		dataset$f71 = -rnorm(sampleNr)
		dataset$f81 = rnorm(sampleNr)
		dataset$f91 = rnorm(sampleNr)
		dataset$f71 = -rnorm(sampleNr)
		dataset$f81 = rnorm(sampleNr)
		dataset$f91 = rnorm(sampleNr)
		dataset$f72 = -rnorm(sampleNr)
		dataset$f82 = rnorm(sampleNr)
		dataset$f92 = rnorm(sampleNr)
		dataset$f721 = -rnorm(sampleNr)
		dataset$f821 = rnorm(sampleNr)
		dataset$f921 = rnorm(sampleNr)
		dataset$f712 = -rnorm(sampleNr)
		dataset$f812 = rnorm(sampleNr)
		dataset$f912 = rnorm(sampleNr)
		dataset$f32 = rnorm(sampleNr)
		dataset$f42 = -rnorm(sampleNr)
		dataset$f52 = rnorm(sampleNr)
		dataset$f62 = rnorm(sampleNr)
		dataset$f72 = -rnorm(sampleNr)
		dataset$f82 = rnorm(sampleNr)
		dataset$f92 = rnorm(sampleNr)
		dataset$f712 = -rnorm(sampleNr)
		dataset$f812 = rnorm(sampleNr)
		dataset$f912 = rnorm(sampleNr)
		dataset$f712 = -rnorm(sampleNr)
		dataset$f8122 = rnorm(sampleNr)
		dataset$f912 = rnorm(sampleNr)
		dataset$f722 = -rnorm(sampleNr)
		dataset$f822 = rnorm(sampleNr)
		dataset$f922 = rnorm(sampleNr)
		dataset$f7212 = -rnorm(sampleNr)
		dataset$f8212 = rnorm(sampleNr)
		dataset$f9212 = rnorm(sampleNr)
		dataset$f7122 = -rnorm(sampleNr)
		dataset$f8122 = rnorm(sampleNr)
		dataset$f9122 = rnorm(sampleNr)

		Classdefiner = dataset$f1*dataset$f2-dataset$f9*0.05
		plot(Classdefiner)
		dataset$Class = (paste0("c",(Classdefiner>=-0.2)+1))
		dataset <<- dataset
	}

	if(SelectDataSet==3){
		a=read.table(file = "D:\\Dropbox\\MU Systems Biology\\Courses\\1. Completed\\MSB1005 Experimental design and Data management\\Assignment\\Exp design\\Data\\MCF7_HypoxiaExperiment_RawExpressionData_Table.txt",header = T,sep = "\t")
		rownames(a) = a$Ensembl_Gene_ID
		a=a[,-1]
		dataset = as.data.frame(t(a))
		dataset$Class = as.factor(gsub(x=rownames(dataset),pattern = "\\.I*$",replacement = ""))
		dataset <<- dataset
	}
}