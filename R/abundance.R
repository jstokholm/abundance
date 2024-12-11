#' Abundance
#'
#' For creating relative abundance data.frame
#'
#' @param phylo_ob Phyloseq object with metadata in sample_data.
#' @param predictor Predictor of interestfor stastics/plotting in sample_data.
#' @param level Taxonomic rank from tax_table, case insensitive; default is "genus".
#' @param id Subject id; default is "abcno".
#' @param sample_id Sample identifier, to be used for repeated sampling of the same subject or different compartments; default is "Time".
#' @param relative_abun Use relative abundances, else absolute; default is TRUE.
#' @param strata Name of variable for stratification, e.g. "Time";
#' @param strata_val Value in variable @strata to keep; default is NULL.
#' @param remove_collapsed_taxa Will remove unclassified taxa which are elsewise merged; default is FALSE.
#' @param select_taxa Choose all taxa from a Taxonomic variable, eg. "Staphylocuccus" or "Staph" or "cuccus"; default is NULL.
#' @param select_level Taxonomic rank of the @select_taxa; default is NULL.
#'
#' @import phyloseq dplyr
#' @return A data.frame
#' @export
# ged <- abundance(phylo_ob=xf_2k_1w_6y_dada2, level="genus", id="Abcno",relative_abun=TRUE,
#                  strata_val=NULL,remove_collapsed_taxa=FALSE,
#                  select_taxa=NULL,select_level=NULL)
#
#
# ged <- abundance(phylo_ob=xf_2k_1w_6y_dada2, level="genus", id="Abcno",relative_abun=TRUE,
#                  strata_val="1y",remove_collapsed_taxa=FALSE,
#                  select_taxa="Enterobacteriaceae",select_level="family")


abundance<- function(phylo_ob, level="genus", id="abcno",sample_id="Time",relative_abun=TRUE,
                     strata=NULL,strata_val=NULL,remove_collapsed_taxa=FALSE,
                     select_taxa=NULL,select_level=NULL) {
  require(phyloseq)

  phylo_ob <- prune_samples(sample_sums(phylo_ob) > 0, phylo_ob)
  otu_mat <- as(otu_table(phylo_ob), "matrix")
    if(taxa_are_rows(phylo_ob)) otu_mat <- t(otu_mat)
  otu_mat  <- otu_mat[,colSums(otu_mat)>0] #removes empty OTUs;
  OTU_index <- colnames(otu_mat)
  tax <- as(tax_table(phylo_ob), "matrix") %>% data.frame(stringsAsFactors=FALSE)
  tax <- tax[rownames(tax) %in% OTU_index,,drop=FALSE]
  tax[is.na(tax)] <- "unclassified"
  tax[tax==""] <- "unclassified"
  org_tax <- names(tax)
  names(tax) <- tolower(names(tax))
  level <- tolower(level)
  tax$OTU <- rownames(tax)
  samp <- data.frame(sample_data(phylo_ob), stringsAsFactors=TRUE)

  if(!is.null(strata)){
    index <- rownames(samp[(samp[,strata] ==strata_val),])
    otu_mat <- otu_mat[rownames(otu_mat) %in% index,]
    otu_mat  <- otu_mat[,colSums(otu_mat)>0] #fjerner tomme OTUs;
    OTU_index <- colnames(otu_mat)
    tax <- tax[rownames(tax) %in% OTU_index,]
    samp <- samp[rownames(samp) %in% index,]
  }

  list <-as.character(tax[,level])
  unique_tax <- unique(list)

  abund <- as.data.frame(matrix(rep(0,(length(unique_tax)*nrow(otu_mat))),ncol=length(unique_tax)))
  row.names(abund) <- row.names(otu_mat)
  names(abund) <- unique_tax
  for(i in names(abund)){
    if(is.array(otu_mat[,list==i]))  abund[,i] <- rowSums(otu_mat[,list== i])
    else   abund[,i] <- otu_mat[,list== i]
  }

  #Sort by abundance
  if(relative_abun) abund <- apply(abund,1,function(x) x/sum(x)) %>% t %>% as.data.frame()

  if (!is.null(select_taxa))  {
    abund <- abund[,colnames(abund) %in% as.character(unique(tax[grep(select_taxa,tax[,select_level],ignore.case=TRUE),level])), drop = FALSE]
    unique_tax <- names(abund)
  }
  if(remove_collapsed_taxa){
    for(i in org_tax) {
      unique_tax <- unique_tax[!grepl(paste(i), unique_tax)]
    }
    unique_tax <- names(abund)
    abund1 <- abund[,(colnames(abund) %in% unique_tax), drop = FALSE]
    '%!in%' <- function(x,y)!('%in%'(x,y))
    unclassified <- data.frame(unclassified=rowSums(abund[,(colnames(abund) %!in% unique_tax), drop = FALSE]))
    abund <- cbind(abund1,unclassified)
    unique_tax <- c(unique_tax,"unclassified")
  }
  if(!is.null(sample_id) & !is.null(strata)) {
    if(sample_id!=strata) df <- cbind(data.frame(samp[,c(id,sample_id,strata)]), abund)
    if(sample_id==strata) df <- cbind(data.frame(samp[,c(id,sample_id)]), abund)
  }
  if(!is.null(sample_id) & is.null(strata)) df <- cbind(data.frame(samp[,c(id,sample_id)]), abund)
  if(is.null(sample_id) & !is.null(strata)) df <- cbind(data.frame(samp[,c(id,strata)]), abund)
  if(is.null(sample_id) & is.null(strata)) {
    df <- cbind(data.frame(samp[,id]), abund)
    colnames(df)[1] <- id
  }
  df
}
