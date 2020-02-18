DAPAR.loc <- DAPARdata.loc <- Prostar.loc <- NULL


G_path_to_pipeline_conf <- 'src/modules/process/pipeline.conf'


listBrewerPalettes <- c("Dark2 (qualit.)" = "Dark2",
                        "Accent (qualit.)"="Accent",
                        "Paired (qualit.)" = "Paired",
                        "Pastel1 (qualit.)" = "Pastel1",
                        "Pastel2 (qualit.)" = "Pastel2",
                        "Set1 (qualit.)" = "Set1",
                        "Set2 (qualit.)" = "Set2", 
                        "Set3 (qualit.)" = "Set3",
                        "BrBG (diverging)"="BrBG",
                        "PiYG (diverging)"=  "PiYG",
                        "PRGn (diverging)" ="PRGn",
                        "PuOr (diverging)" ="PuOr",
                        "RdBu (diverging)"="RdBu",
                        "RdGy (diverging)" ="RdGy",
                        "RdYlBu (diverging)" ="RdYlBu",
                        "RdYlGn (diverging)" ="RdYlGn",
                        "Spectral (diverging)"="Spectral")





## URLs for the .md files stored in the website github directory
base_URL <- "http://www.prostar-proteomics.org/md/"
URL_FAQ <- paste0(base_URL, "FAQ.md")
URL_links <- paste0(base_URL, "links.md")
URL_ProstarPresentation <- paste0(base_URL, "presentation.md")
URL_formerReleases <-paste0(base_URL, "formerReleases.md")
URL_versionNotes <- paste0(base_URL, "versionNotes.md")


actionBtnClass <- "btn-primary"
PrevNextBtnClass <- "btn-info"
optionsBtnClass <- "info"


defaultGradientRate <- 0.9


grey <- "#FFFFFF"
orangeProstar <- "#E97D5E"

G_path2conf <- './R/pipeline.conf'