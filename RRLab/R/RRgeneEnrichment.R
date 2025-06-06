#' RRgeneEnrichment
#'
#' Convenience wrapper around missMethyl to perform GO or KEGG enrichment
#' analysis for a vector of gene identifiers or CpG sites. Automatically loads
#' required packages via `libraryR`, generates basic plots and returns all
#' results in a list.
#'
#' @param genes Character vector of gene identifiers (typically gene symbols) or
#'   CpG identifiers. The function treats the input as CpGs when a majority of
#'   values begin with \code{"cg"}.
#' @param organism Annotation package (e.g. "org.Hs.eg.db") used for GO analysis
#'   or KEGG organism code.
#' @param type Type of enrichment to run: either "GO" or "KEGG".
#' @param pvalueCutoff P-value cutoff for significance filtering.
#' @param array.type If genes are CpGs this specifies the Illumina array type
#'   (e.g. "EPIC" or "450K") used to look up gene annotations.
#' @param ... Additional arguments passed to `missMethyl::gometh` or
#'   `missMethyl::gsameth`.
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
  RRLab::libraryR("missMethyl")

  cpg_idx <- grepl("^cg", genes, ignore.case = TRUE)
  input_type <- if (mean(cpg_idx) > 0.5) "CpG" else "gene"
  used_ids <- if (input_type == "CpG") genes[cpg_idx] else genes[!cpg_idx]

  if (input_type == "CpG") {
    if (toupper(array.type) == "EPIC") {
      RRLab::libraryR("IlluminaHumanMethylationEPICanno.ilm10b4.hg19")
    } else if (toupper(array.type) == "450K") {
      RRLab::libraryR("IlluminaHumanMethylation450kanno.ilmn12.hg19")
    }
    enrich_tbl <- missMethyl::gometh(
      sig.cpg = used_ids,
      collection = toupper(type),
      array.type = array.type,
      plot.bias = FALSE,
      ...
    )
  } else {
    enrich_tbl <- missMethyl::gsameth(
      sig.genes = used_ids,
      collection = toupper(type),
      array.type = array.type,
      plot.bias = FALSE,
      ...
    )
  }

  tbl <- enrich_tbl[enrich_tbl$FDR <= pvalueCutoff, ]
  top_tbl <- head(tbl[order(tbl$FDR), ], 20)
  p_bar <- ggplot2::ggplot(top_tbl, ggplot2::aes(x = reorder(Term, FDR), y = -log10(FDR))) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "-log10(FDR)", title = "Enrichment barplot")
  p_dot <- ggplot2::ggplot(top_tbl, ggplot2::aes(x = -log10(FDR), y = reorder(Term, FDR))) +
    ggplot2::geom_point(ggplot2::aes(size = N)) +
    ggplot2::labs(x = "-log10(FDR)", y = NULL, title = "Enrichment dotplot")

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
