#' RRgeneEnrichment
#'
#' Convenience wrapper around clusterProfiler to perform GO or KEGG enrichment
#' analysis for a vector of gene identifiers or CpG sites. Automatically loads
#' required packages via `libraryR`, generates basic plots and returns all
#' results in a list.
#'
#' @param genes Character vector of gene identifiers (typically gene symbols) or
#'   CpG identifiers starting with `cg`.
#' @param organism Annotation package (e.g. "org.Hs.eg.db") used for GO analysis
#'   or KEGG organism code.
#' @param type Type of enrichment to run: either "GO" or "KEGG".
#' @param pvalueCutoff P-value cutoff for significance filtering.
#' @param array.type If genes are CpGs this specifies the Illumina array type
#'   (e.g. "EPIC" or "450K") used to look up gene annotations.
#' @param ... Additional arguments passed to `clusterProfiler::enrichGO` or
#'   `clusterProfiler::enrichKEGG`.
#'
#' @return A list containing the result table, barplot, dotplot and information
#'   on the selected IDs.
#'
#' @examples
#' \dontrun{
#' genes <- c("BRCA1", "TP53", "EGFR")
#' RRgeneEnrichment(genes, organism = "org.Hs.eg.db", type = "GO")
#' RRgeneEnrichment(genes, organism = "hsa", type = "KEGG")
#' # CpG input
#' cpgs <- c("cg00000029", "cg00000108")
#' RRgeneEnrichment(cpgs, type = "GO", array.type = "EPIC")
#' }
#'
#' @export
RRgeneEnrichment <- function(genes, organism = "org.Hs.eg.db", type = "GO",
                             pvalueCutoff = 0.05, array.type = "EPIC", ...) {
  RRLab::libraryR("clusterProfiler")

  input_type <- if (all(grepl("^cg", genes))) "CpG" else "gene"
  used_ids <- genes

  if (input_type == "CpG") {
    RRLab::libraryR("missMethyl")
    if (toupper(array.type) == "EPIC") {
      RRLab::libraryR("IlluminaHumanMethylationEPICanno.ilm10b4.hg19")
    } else if (toupper(array.type) == "450K") {
      RRLab::libraryR("IlluminaHumanMethylation450kanno.ilmn12.hg19")
    }
    anno <- missMethyl:::.getFlatAnnotation(array.type)
    idx <- match(genes, anno$cpg)
    g <- unique(stats::na.omit(anno$gene[idx]))
    g <- unique(unlist(strsplit(g, ";")))
    genes <- g
  }

  if (type == "GO") {
    RRLab::libraryR(organism)
    enrich_obj <- clusterProfiler::enrichGO(
      gene = genes,
      OrgDb = get(organism),
      keyType = "SYMBOL",
      pvalueCutoff = pvalueCutoff,
      ...
    )
  } else if (type == "KEGG") {
    enrich_obj <- clusterProfiler::enrichKEGG(
      gene = genes,
      organism = organism,
      pvalueCutoff = pvalueCutoff,
      ...
    )
  } else {
    stop("type must be either 'GO' or 'KEGG'")
  }

  tbl <- as.data.frame(enrich_obj)
  p_bar <- clusterProfiler::barplot(enrich_obj, showCategory = 20) +
    ggplot2::ggtitle("Enrichment barplot")
  p_dot <- clusterProfiler::dotplot(enrich_obj, showCategory = 20) +
    ggplot2::ggtitle("Enrichment dotplot")

  res <- list(
    table      = tbl,
    barplot    = p_bar,
    dotplot    = p_dot,
    ids_used   = used_ids,
    input_type = input_type,
    array_type = if (input_type == "CpG") array.type else NA
  )
  class(res) <- c("RRgeneEnrichmentResult", class(res))
  res
}
