#' GetGenesFromCpGs
#' @usage
#' Give CpGs, get Genes (and such)
#'
#' @param sig.cpg CpG list 
#' 
#'
#' @examples
#' Result = GetGenesFromCpGs(sig.cpg = c("cg25871843","cg26405148"), array.type = "EPIC")
#' @export
GetGenesFromCpGs = function(sig.cpg = CpG_names){
    array.type = "EPIC"
    require(IlluminaHumanMethylationEPICanno.ilm10b4.hg19)
    flatu <- missMethyl:::.getFlatAnnotation(array.type)

    m_all <- match(flatu$cpg, sig.cpg)
    GeneAnnotationData = flatu[!is.na(m_all), ]
    LocationAnnotationData = Locations[match(sig.cpg,rownames(Locations)),]

    CombinedAnnoData = cbind(GeneAnnotationData,data.frame(LocationAnnotationData[match(GeneAnnotationData$cpg,rownames(LocationAnnotationData)),]))
    rownames(CombinedAnnoData) = 1:dim(CombinedAnnoData)[1]
    CombinedAnnoData = CombinedAnnoData[order(as.numeric(gsub(CombinedAnnoData$cpg,pattern = "cg",replacement = "")),CombinedAnnoData$group),]
    class(CombinedAnnoData) = c(class(CombinedAnnoData),"GetGenesFromCpGs")
    return(CombinedAnnoData)
}