# PhenoVisualizer

library(tidyverse)
library(ggsci)
library(showtext)
library(cowplot)
library(gridExtra)
library(reshape2)

# Data:
#	Dataframe with samples in rows and features in cols

# Function:
PhenoVisualizer = function(data, COI){

	# get indexes
	COI_index = match(colnames(data),x = COI)


	# make format
	dat_sub = melt(data ,  id.vars = c(COI))

		
	# create a list with a specific length 
	plot_lst <- vector("list", length = length(COI))

	# plot per 
	for( i in 1:length(COI)){

		current_COI = COI[i]
		
		p <- ggplot( data = dat_sub, aes(x=variable, y=value, fill=factor(get(current_COI))) )+
		#geom_boxplot(alpha = 0.7)+
		#geom_violin(alpha = 0.1, color = 1)+
		geom_boxplot(width = 0.5, lwd = 1)+
		theme_cowplot()+
		theme(legend.position = "none")+
		ylab("")+
		xlab("")+
		#scale_fill_manual(values = c("green2","red2"))+
		coord_flip()+
		ggtitle(current_COI)+
		theme(legend.position="bottom")+
		guides(fill=guide_legend(title=""))
		
		plot_lst[[i]] = ggplotGrob(p)
		rm("p")

	}

	cowplot::plot_grid(plotlist = plot_lst, ncol = length(COI))

}

#
## Example iris
#PhenoVisualizer(iris,"Species")
#
## Example mtcars
#PhenoVisualizer(mtcars,"cyl")
#
## Example attitude
#attitude$measure=(attitude$rating>65)+1
#PhenoVisualizer(attitude,"measure")
#
#
## example PIA data
#pheno$time = 1
#pheno$time[grep(pheno$SampleID,pattern = "(W2)",value = F)] = 2
#
#PhenoVisualizer(pheno[,c(60,13,14,15,grep(colnames(pheno),pattern = #"Sum"))],c("Group","Education","time","Gender"))
#
#PhenoVisualizer(pheno[,c(13,39,41,42,25,50,32,27)],c("Group","CAPS.w2.Sum",#"PCL.w2.Sum"))
