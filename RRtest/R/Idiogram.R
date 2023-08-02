#' Idiogram
#' @usage
#' Make Idiogram from dataframe object based on CpGs. Indicates the chromosomal location of which CpGs were found, and its significant level. Only chromosomes 1 untill 22 are supported for now. Can be developed if needed.
#'
#' @param data_points Data.frame must contain: "seqnames", "start", "end" and "pvalue", others are optional. See example.
#' @param s_barwidth Plotting (bar). Chromosome bar width
#' @param s_size_line Plotting (lines) CpG line size
#' @param s_alpha_lines Plotting (lines) CpG line Alpha
#' @param s_offset Plotting (points) CpG significance point offset to line
#' @param s_size_point Plotting (points) CpG significance point size
#' 
#'
#' @examples
#' # This is an example using a randomized set of sites of interest on the genome, then adds random unified sampled P-values. Then shows the plot
#' npoints = 1000
#' data_points <- sort(regioneR::createRandomRegions(nregions=npoints, mask=NA))
#' data_points$pvalue = runif(npoints)
#' Idiogram(data_points)
#' 
#' @export

Idiogram = function(data_points, s_barwidth = 0.4, s_size_line = 1, s_alpha_lines = 0.33, s_offset = 0.1, s_size_point = 2) {

    # libs
    require(ggplot2)
    require(scales)
    require(regioneR)
    require(data.table)
    require(ggnewscale)

    if(class(data_points)!="data.frame"){
        warning("converted data_points to dataframe")
        data_points = data.frame(data_points)
    }


    # check if a metric is present (like pval)
    if(!"pvalue"%in%colnames(data_points)){
        stop("\nPlease add pvalue to dataframe object (by running data_points$pvalue <- [VALUE])\n")
       
    }else{
        # Check if pvalue ranges log or p-value scale -> to log
        if(range(data_points$pvalue)[1] >=0 & range(data_points$pvalue)[2] <=1){
            warning("Transforming p-value to -log10 scale")
            data_points$pvalue_log = -log10(data_points$pvalue)
        }
    }
    


    # local function
    WhichChromosomeLoc = function(x, s_width = 0.7){
        
        Orderset = paste0("chr",1:22)
        
        Res = x
        count = 0
        for(i in 1:22){
            val = which(as.character(x) == Orderset[i])
            if(length(val)==0) {next}
            count = count +1
            Res[val] = count
        }
        
        Res = as.numeric(Res)

        out = data.frame(x = Res-(s_width/2), xend = Res+(s_width/2))
        return(out)
    
    }

    getCentromeres <- function( genome="hg19" ,regenerate = FALSE){
        # actually run, but takes time bc api and such
        if(regenerate){
            mySession <- try(browserSession("UCSC"), silent=TRUE)
            # In case of failure, try another mirror
            if(inherits(mySession, "try-error"))
                mySession <- browserSession("UCSC",
                                            url="http://genome-euro.ucsc.edu/cgi-bin/")
            genome(mySession) <- genome
            obj <- ucscTableQuery(mySession, table="gap")
            tbl <- getTable(obj)
            tbl <- tbl[tbl$type=="centromere", c("chrom", "chromStart", "chromEnd")]
            colnames(tbl)[2:3] <- c("centromerStart", "centromerEnd")
            return(tbl)
        
        }else{
        # Hardcoded data
            tbl = data.frame(
                chrom = c("chr1", "chr21", "chr22", "chr19", "chrY", "chr20", "chr18", "chr17", "chr16", "chr15", "chr14", "chr13", "chr12", "chr11", "chr10", "chr9", "chr8", "chrX", "chr7", "chr6", "chr5", "chr4", "chr3", "chr2"),
                centromerStart = c(121535434, 11288129, 1.3e+07, 24681782, 10104553, 26369569, 15460898, 22263006, 35335801, 1.7e+07, 1.6e+07, 1.6e+07, 34856694, 51644205, 39254935, 47367679, 43838887, 58632012, 58054331, 58830166, 46405641, 49660117, 90504854, 92326171),
                centromerEnd = c(124535434, 14288129, 1.6e+07, 27681782, 13104553, 29369569, 18460898, 25263006, 38335801, 2e+07, 1.9e+07, 1.9e+07, 37856694, 54644205, 42254935, 50367679, 46838887, 61632012, 61054331, 61830166, 49405641, 52660117, 93504854, 95326171)
            )
            return(tbl)
        }
    }

    #data_points <- sort(regioneR::createRandomRegions(nregions=20, mask=NA))
    #GRanges object with 10 ranges and 0 metadata columns:
    #       seqnames              ranges strand
    #          <Rle>           <IRanges>  <Rle>
    #   [1]     chr6   64042317-64042569      *
    #   [2]     chr6   91652064-91652271      *
    # ...




    ##############################################
    ############# Modifying data (PER CPG) ####### 
    ##############################################

    # modiifying data to get running
    data_points_df = as.data.table(data_points)

    # set up for plotting later
    XposBars = WhichChromosomeLoc(as.character(data_points_df$seqnames),s_width = s_barwidth)
    data_points_df$PlottingX = XposBars[,1]
    data_points_df$PlottingXend = XposBars[,2]

    #################################### 
    ############# Chromosome data ###### 
    #################################### 
    # Get all data 
    Chromosome_df = as.data.table(regioneR::getGenomeAndMask(genome = "hg19", mask = NA)$genome)
    
    # filter chromosomes to only look at data
    Chromosome_df = Chromosome_df[Chromosome_df$seqnames%in%data_points_df$seqnames,]

    # ONLY CHR 1 TILL 22; NO SUPPORTFOR OTHERS
    message("Only using chr 1:22")
    Chromosome_df = Chromosome_df[Chromosome_df$seqnames%in%paste0("chr",1:22),]

    # Get Centromeres
    Chromosome_Centro = getCentromeres()
    Chromosome_df$CentroY = Chromosome_Centro[match(Chromosome_df$seqnames,Chromosome_Centro$chrom),"centromerStart"]
    XposBars = WhichChromosomeLoc(as.character(Chromosome_df$seqnames),s_width = s_barwidth)
    Chromosome_df$CentroX = XposBars[,1]
    Chromosome_df$CentroXend = XposBars[,2]

    # relative number of CpGs found on chromosomes
    #data_points_df_relativeCpG = data_points_df[,.(NumCpG = round(length(pvalue)/dim(data_points_df)[1]*100,2),by = seqnames)]
    data_points_df_relativeCpG =  data.frame( sapply(unique(data_points_df$seqnames),function(x){round(sum(data_points_df$seqnames == x)/dim(data_points_df)[1]*100,2)}))
    data_points_df_relativeCpG$NumCpG = data_points_df_relativeCpG[,1]
    data_points_df_relativeCpG$seqnames = rownames(data_points_df_relativeCpG)
    data_points_df_relativeCpG = data_points_df_relativeCpG[,-1]

    #data_points_df_amountCpG = data_points_df[,.(NumCpG = length(pvalue)),by = seqnames]
    Chromosome_df$NumCpG  =  data_points_df_relativeCpG$NumCpG[match(Chromosome_df$seqnames,data_points_df_relativeCpG$seqnames)]
    #Chromosome_df$NumCpG  = paste0(format(Chromosome_df$NumCpG,2)," %")

    # correct for length of chromosome, so getting a enichment like number
    Chromosome_df$NumCpGCorrectChr =  Chromosome_df$NumCpG / (Chromosome_df$end/max(Chromosome_df$end))
    Chromosome_df$NumCpGCorrectChr = Chromosome_df$NumCpGCorrectChr /sum(Chromosome_df$NumCpGCorrectChr/100)
    Chromosome_df$NumCpGCorrectChrFormat = paste0(format(Chromosome_df$NumCpGCorrectChr,digits=3)," %")
    Chromosome_df$NumCpG  =  paste0(format(Chromosome_df$NumCpG,digits=3)," %")

    #################################### 
    ############# Plotting ############# 
    #################################### 

    
    ########### BAR: CHROMOSOMES
    p0 = ggplot(Chromosome_df,aes(x=seqnames, y= end)) +
        geom_bar(stat = "identity", fill = "white", width = s_barwidth, col="black")+
        scale_y_continuous(labels = scales::unit_format(unit = "M bp", scale = 1e-6))

        
    ############## TEXT: percentage CpG of all
    # Add lines within each bar
    #p0 <- p0 + geom_text(data = Chromosome_df, aes(label = NumCpG, y = 0), vjust = 1.5)

    ############## TEXT: percentage CpG corrected for length
    # Add lines within each bar
    p1 <- p0 + geom_text(data = Chromosome_df, aes(label = NumCpGCorrectChrFormat, y = 0, color=NumCpGCorrectChr), vjust = 2) +
                ggplot2::scale_colour_gradient(
                    #scale_name = "Enrichscale",
                    name="Relative CpG count(%)\ncorrected for chromosome length",
                    low = "gray80", high = "black"
			    ) + ggnewscale::new_scale_color()

                
    ############## LINES: LOCATION
    # Add lines within each bar
    #p0 <- p1 + geom_segment(data = data_points_df, aes(x = PlottingX, xend = PlottingXend, 
    #            y = end, yend = end), color = ggplot2::alpha("Black",s_alpha_lines), size = s_size_line) 
    p0 <- p1 + geom_segment(data = data_points_df, aes(x = PlottingX, xend = PlottingXend, 
                y = end, yend = end), color = ggplot2::alpha("Black",s_alpha_lines), size = s_size_line) 
    
    

        
    ############## POINTS: LOCATION
    # Add points
    p <- p0 + geom_point(data = data_points_df, aes(x = PlottingXend+s_offset, y = end, colour = pvalue_log), 
                size = s_size_point) +
                ggplot2::scale_colour_gradientn(
                    #scale_name = "pvalscale",
                    name="Significance\n-log10(P-value)",
                    colours		= c(ggplot2::alpha("gray90",0.1),ggplot2::alpha("gray90",0.1),ggplot2::alpha("yellow",0.2),ggplot2::alpha("red",1)),# the actual colors
					na.value	= "gray90",
                    limits      = c( 0, max(data_points_df$pvalue_log)), # needed to show out of bound elements.
					values   	= c( 0 / (max(data_points_df$pvalue_log) - 0), # the relative location of the color source # this is is the minimum (0) so min(x) should be treated as 0
                                    (-log10(0.05)-0.0001) / (max(data_points_df$pvalue_log) - 0), # the lower TH significance
                                     (-log10(0.05)+0.0001) / (max(data_points_df$pvalue_log) - 0), # the upper TH significance to get sharp cutoff
                                     1), # the max
					breaks   	= ceiling(c(0,-log10(0.05),((max(data_points_df$pvalue_log)+log10(0.05)) / 2 )- log10(0.05),max(data_points_df$pvalue_log))*100)/100 # For breaks, limits are needed! Just the spot where ticks are needed.
			    )

    ############## thinnLINES: Centromeres
    # put after all, to still show the centromeres
    # Add lines within each bar
    p <- p + geom_segment(data = Chromosome_df, aes(x = CentroX, xend = CentroXend, 
                y = CentroY, yend = CentroY), color = ggplot2::alpha("Blue",1), linetype = "dashed", size = 0.5) 
                 
    # Themes
    p = p + theme_minimal()+
        theme(
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()
        )+
        labs(title = "Idiogram",
            x = "Chromosome",
            y = "BP")
    
    
    # Display the plot
    print(p)



}