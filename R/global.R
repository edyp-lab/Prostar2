DAPAR.loc <- DAPARdata.loc <- Prostar.loc <- NULL


library(fresh)
library(DAPAR2)
library(QFeatures)




## URLs for the .md files stored in the website github directory
base_URL <- "http://www.prostar-proteomics.org/md/"
URL_FAQ <- paste0(base_URL, "FAQ.md")
URL_links <- paste0(base_URL, "links.md")
URL_ProstarPresentation <- paste0(base_URL, "presentation.md")
URL_formerReleases <-paste0(base_URL, "formerReleases.md")
URL_versionNotes <- paste0(base_URL, "versionNotes.md")


actionBtnClass <- "btn-primary"

# Bootstrap colors for buttons

# ="btn btn-primary">Primary</button>
#   <button type="button" class="btn btn-secondary">Secondary</button>
#   <button type="button" class="btn btn-success">Success</button>
#   <button type="button" class="btn btn-danger">Danger</button>
#   <button type="button" class="btn btn-warning">Warning</button>
#   <button type="button" class="btn btn-info">Info</button>
#   <button type="button" class="btn btn-light">Light</button>
#   <button type="button" class="btn btn-dark">Dark</button>
#   
#   <button type="button" class="btn btn-link">Link</button>

redBtnClass <- "btn-danger"
PrevNextBtnClass <- "btn-info"
btn_success_color <- "btn-success"
optionsBtnClass <- "info"




