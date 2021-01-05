library(shiny)
library(shinyWidgets)
library(shinyjs)
setwd("~/TELETRAVAIL/github_DAPARforFeatures/Prostar2/dev/prostar_NvCSS/")
#setwd("~/Github/AdaptedForFeatures/Prostar2/dev/prostar_NvCSS/")


ui <- fluidPage(
  
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "style_tl5.css")
  ),
  
  
  tags$div(class="box",
           img(src='fleche.PNG', height='20px', width='20px'),
           tags$div(class="sub_box",
                    p('Filtration')
           ),
           img(src='fleche.PNG', height='20px', width='20px'),
           tags$div(class="sub_box",
                    p('Normalization')
           ),
           img(src='fleche.PNG', height='20px', width='20px'),
           tags$div(class="sub_box",
                    p('Imputation')
           ),
           img(src='fleche.PNG', height='20px', width='20px'),
           tags$div(class="sub_box",
                    p('Aggregation')
           )
  )
  # tags$div(class="box",
  #          tags$div(class="sub_box",
  #                   p('Filt.')
  #          ),
  #          tags$div(class="sub_box",
  #                   p('Norm.')
  #          ),
  #          tags$div(class="sub_box",
  #                   p('Imp.')
  #          ),
  #          tags$div(class="sub_box",
  #                   p('Aggr.')
  #          )
  # )
  
)


server <- function(input, output){
  
  
  
}


shinyApp(ui, server)

##################################################
# la 4 (E: serpentin a demi cercles)
#   
#   Voici quelques motivations :

#   2.Plus sérieusement, elle a à mon avis le meilleur facteur de forme
# (ratio largeur/ hauteur) pour un slide show vertical ; C'est encore plus vrai si on veut à
# terme que le slideshow puisse être masqué par la barre de menu latérale
# 
#   3.Enfin, elle peut être naturellement adaptée pour faire un slideshow horizontal,
# et donc avoir des layouts cohérents entre le slideshow du pipeline et du process en cours
# 
# 
# 
# le 5 (à gauche, E: items penchés à 45°)
#   
#  les motivations ci-dessus s'appliquent aussi (facteur de forme et mise à l'horizontal).
# Si jamais vous n'aimez pas la diapo 4, voici quelques idées pour améliorer la diapo 5 :
#   
#   -          Mettre des rectangles avec des coins arrondis
# 
# -          Remplacer les ronds (qui ne servent à rien parce qu'il s'agit en fait des
# transitions entre étapes) par des flèches qui suivent un quart de rond, genre comme ça :*
#   
#   -          Eventuellement, il est possible d'utiliser la flèche pour indiquer si l'on
# a le droit de passer à l'étape suivante (en changeant sa couleur ou sa forme, ou en
# rajoutant une croix dessus), et de n'utiliser que 2 couleurs pour les rectangle
# (fait ou non fait ; en décrivant l'aspect obligatoire ou pas par l'état de la flèche,
# plutôt que par la couleur du rond de l'étape). D'ailleurs, maintenant que j'y pense,
# il y a peut-etre moyen de faire pareil sur la diapo 4, et de changer les arcs de cercles
# qui composent le serpentin : il serait vert si l'étape suivante est autorisée, rouge sinon,
# et gris si l'étape a déjà été passé ?
#   
