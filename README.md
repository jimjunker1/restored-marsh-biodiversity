
# Restored Marsh Biodiversity

## Summary

This is the code repository for the article, *Can created saltmarshes
match biodiversity of pre-existing ones across scales? An assessment
from microbes to predators*, published in *Ecosphere*.Scripts for
reproducing the analyses and figures can be found below.

<!-- Badges start -->
<!-- Badges end -->

## Reproducing the code

| Step                        |                      Script                      |
|-----------------------------|:------------------------------------------------:|
| clean data                  |        [:computer:](code/01_load-data.R)         |
| measurement of biodiversity |       [:computer:](code/02_analyze-MoB.R)        |
| partition beta diversity    |  [:computer:](code/03_analyze-beta-partition.R)  |
| plot abundances             |  [:computer:](code/04_make-abundance-figures.R)  |
| plot diversities            |  [:computer:](code/05_make-diversity-figures.R)  |
| plot rarefactions           | [:computer:](code/06_make-rarefaction-figures.R) |
| plot similarities           | [:computer:](code/07_make-dendrogram-figures.R)  |
| plot effects                |   [:computer:](code/08_make-effects-figures.R)   |
| plot general effects        | [:computer:](code/09_make-gen-effect-figures.R)  |
| plot effects on S           |  [:computer:](code/10_make-effect-S-figures.R)   |
| plot SAD                    |     [:computer:](code/11_make-SAD-figures.R)     |

## Reproducibility

Version information for the when this repository was last updated can be
found below.

<details>
<summary>
Reproducibility receipt
</summary>

``` r
## datetime
Sys.time()
```

    ## [1] "2022-12-08 12:17:16 CST"

``` r
source(here::here("code/01_load-data.R"))
## session info
sessionInfo()
```

    ## R version 4.2.1 (2022-06-23 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.utf8 
    ## [2] LC_CTYPE=English_United States.utf8   
    ## [3] LC_MONETARY=English_United States.utf8
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.utf8    
    ## 
    ## attached base packages:
    ## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] gplots_3.1.3       usedist_0.4.0      mobr_2.0.2         forcats_0.5.1     
    ##  [5] stringr_1.4.1      dplyr_1.0.10       purrr_0.3.5        readr_2.1.2       
    ##  [9] tidyr_1.2.1        tibble_3.1.8       ggplot2_3.4.0      tidyverse_1.3.1   
    ## [13] rmarkdown_2.17     styler_1.7.0       here_1.0.1         pairwiseAdonis_0.4
    ## [17] cluster_2.1.3      vegan_2.6-2        lattice_0.20-45    permute_0.9-7     
    ## [21] pacman_0.5.1       devtools_2.4.4     usethis_2.1.6     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] colorspace_2.0-3   ellipsis_0.3.2     rprojroot_2.0.3    fs_1.5.2          
    ##  [5] rstudioapi_0.13    remotes_2.4.2      bit64_4.0.5        fansi_1.0.3       
    ##  [9] lubridate_1.9.0    xml2_1.3.3         splines_4.2.1      R.methodsS3_1.8.2 
    ## [13] cachem_1.0.6       knitr_1.39         pkgload_1.3.0      jsonlite_1.8.3    
    ## [17] broom_1.0.0        dbplyr_2.2.1       R.oo_1.25.0        shiny_1.7.3       
    ## [21] compiler_4.2.1     httr_1.4.4         backports_1.4.1    assertthat_0.2.1  
    ## [25] Matrix_1.4-1       fastmap_1.1.0      cli_3.3.0          later_1.3.0       
    ## [29] htmltools_0.5.2    prettyunits_1.1.1  tools_4.2.1        gtable_0.3.1      
    ## [33] glue_1.6.2         Rcpp_1.0.9         cellranger_1.1.0   vctrs_0.5.0       
    ## [37] nlme_3.1-157       xfun_0.31          ps_1.7.1           rvest_1.0.2       
    ## [41] timechange_0.1.1   mime_0.12          miniUI_0.1.1.1     lifecycle_1.0.3   
    ## [45] gtools_3.9.2.2     MASS_7.3-57        scales_1.2.1       vroom_1.6.0       
    ## [49] hms_1.1.2          promises_1.2.0.1   yaml_2.3.6         curl_4.3.3        
    ## [53] memoise_2.0.1      geosphere_1.5-14   pbapply_1.6-0      gridExtra_2.3     
    ## [57] stringi_1.7.8      plotrix_3.8-2      caTools_1.18.2     pkgbuild_1.3.1    
    ## [61] bitops_1.0-7       rlang_1.0.6        pkgconfig_2.0.3    evaluate_0.15     
    ## [65] htmlwidgets_1.5.4  bit_4.0.4          processx_3.7.0     tidyselect_1.2.0  
    ## [69] magrittr_2.0.3     R6_2.5.1           generics_0.1.3     profvis_0.3.7     
    ## [73] DBI_1.1.3          pillar_1.8.1       haven_2.5.0        withr_2.5.0       
    ## [77] mgcv_1.8-40        sp_1.5-0           modelr_0.1.8       crayon_1.5.2      
    ## [81] KernSmooth_2.23-20 utf8_1.2.2         tzdb_0.3.0         urlchecker_1.0.1  
    ## [85] grid_4.2.1         readxl_1.4.0       callr_3.7.3        reprex_2.0.1      
    ## [89] digest_0.6.29      xtable_1.8-4       R.cache_0.15.0     httpuv_1.6.6      
    ## [93] R.utils_2.12.2     munsell_0.5.0      egg_0.4.5          sessioninfo_1.2.2

</details>
