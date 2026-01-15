#' Hierarchical condition categories included in CMS-HCC risk adjustment version 28
#'
#' A list of the hierarchical condition categories (e.g. diagnoses)
#' in version 28 of the CMS-HCC risk adjustment algorithm.
#'
#' @format ## `v28_hccs`
#' A vector with 115 elements, each beginning with 'hcc' and ending with a number
#'
#' @source https://www.cms.gov/medicare/payment/medicare-advantage-rates-statistics/risk-adjustment/2025-model-software/icd-10-mappings
"v28_hccs"

#' Hierarchy of hierarchical condition categories in CMS-HCC risk adjusment version 28
#'
#' A nested list describing the hierarchical structure of version 28 of
#' the CMS-HCC risk adjustment algorithm.
#'
#' @format ## `hcc_hierarchy_list`
#' A nested list, where the name of the nested list is the head of the branch
#'
#' @source https://www.cms.gov/medicare/payment/medicare-advantage-rates-statistics/risk-adjustment/2025-model-software/icd-10-mappings
"hcc_hierarchy_list"

#' Co-occuring v28 HCCs based on summarized AllOfUs self-reported survey data with missing HCCs imputed
#'
#' A tibble with two columns, 'hcc_set' and 'num_respondents'. Each row of 'hcc_set' corresponds to a
#' unique combination of co-occurring HCCs (from version 28 of the CMS-HCC risk adjustment formula) as a single character.
#' Missing HCCs were imputed and randomly inserted into summary AllOfUs data; see 'source' below for details.
#' All 'num_respondents' values are set to counts > 20, which aligns with All of Us policies; however, note that values set for imputed
#' HCCs (i.e. HCCs not included in `coocurring_v28_hcc_counts_all_of_us`) are not real.
#'
#' @source Self-reported survey data from All Of Us v7 (https://www.researchallofus.org/) with missing HCCs imputed. The
#' code to construct this from AllOfUs counts (a summarized version of which is available as 'coocurring_v28_hcc_counts_allofus')
#' is available in R/make_reference_dataset.R in the function 'make_reference_dataset()'.
"cms_hcc_v28_reference_hcc_count_dt"

#' Co-occuring v28 HCCs based on summarized AllOfUs self-reported survey data
#'
#' A tibble with two columns, 'hcc_set' and 'num_respondents'. Each row of 'hcc_set' corresponds to a
#' unique combination of co-occurring HCCs (from version 28 of the CMS-HCC risk adjustment formula) as a single character.
#' 'num_respondents' is a count > 21 of how many respondents were found to have a given
#' co-occurring set of v28 HCCs. This complies with All of Us policies because it both excludes any counts of 20 or fewer and a number of counts greater than 20.
#'
#' @source Self-reported survey data from All Of Us v7 (https://www.researchallofus.org/).
#' Lower level data are not allowed to be distributed outside the AllOfUs Research Workbench, but we provide the code we used
#' to generate these data in the manuscript associated with this package (TODO add link here).
"cooccurring_v28_hcc_counts_allofus"
