library(shiny)
#setwd("~/TELETRAVAIL/github_DAPARforFeatures/Prostar2/dev/prostar_NvCSS/")
setwd("~/Github/AdaptedForFeatures/Prostar2/dev/prostar_NvCSS/")

ui <- fluidPage(
  
  # HEAD
  theme = "style.css",
  tags$head(
    tags$link(rel="stylesheet", href="https://www.w3schools.com/w3css/4/w3.css")
  ),
  
  
  # BODY
  # sidebar
  tags$div(
    class = "w3-sidebar w3-bar-block w3-border-right",
    style = "display:block",
    id="mySidebar",
    tags$button(class="w3-bar-item w3-teal w3-small",
                onclick="w3_close()",
                "Close X"),
    tags$a(href="#",
           class="w3-bar-item w3-button",
           "Link 1"),
    tags$a(href="#",
           class="w3-bar-item w3-button",
           "Link 2"),
    tags$a(href="#",
           class="w3-bar-item w3-button",
           "Link 3")
  ),
  
  # header
  tags$header(
    id = "container_header",
    tags$div(class="element_header",
             tags$button(class="w3-button w3-teal w3-small",
                  onclick="w3_open()",
                  "=")),
    tags$div(class="element_header",
             tags$h1("Prostar"))
  ),
  
  
  
  # navigation dessous sidebar
  tags$nav(
    tags$ul(
      tags$li(tags$a(href="index.html", "Accueil")),
      tags$li(tags$a(href="part1.html", "Partie 1")),
      tags$li(tags$a(href="part2.html", "Partie 2")),
      tags$li(tags$a(href="#part3", "Partie 3"))
    )
  ),
  
  
  # section générale
  tags$section(
    id = "container_general",
    tags$h2("Container General"),
    tags$h3("Description du container general: bla bla bla bla..."),
    
    tags$div(tags$a(href="https://www.w3schools.com/w3css/tryit.asp?filename=tryw3css_sidebar_over",
                    "sidebar OK")),
    
    tags$div(class = "element",
             tags$h1("Partie 1"),
             tags$p("Excitavit hic ardor milites per municipia plurima, quae isdem conterminant, dispositos et castella, sed quisque serpentes latius pro viribus repellere moliens, nunc globis confertos, aliquotiens et dispersos multitudine superabatur ingenti, quae nata et educata inter editos recurvosque ambitus montium eos ut loca plana persultat et mollia, missilibus obvios eminus lacessens et ululatu truci perterrens."),
             tags$br(),
             tags$p("Ergo ego senator inimicus, si ita vultis, homini, amicus esse, sicut semper fui, rei publicae debeo. Quid? si ipsas inimicitias, depono rei publicae causa, quis me tandem iure reprehendet, praesertim cum ego omnium meorum consiliorum atque factorum exempla semper ex summorum hominum consiliis atque factis mihi censuerim petenda."),
             tags$br(),
             tags$p("Ut enim benefici liberalesque sumus, non ut exigamus gratiam (neque enim beneficium faeneramur sed natura propensi ad liberalitatem sumus), sic amicitiam non spe mercedis adducti sed quod omnis eius fructus in ipso amore inest, expetendam putamus.")
    ),
    
    tags$div(class = "element",
             tags$h1("Partie 2"),
             tags$p("Nam quibusdam, quos audio sapientes habitos in Graecia, placuisse opinor mirabilia quaedam (sed nihil est quod illi non persequantur argutiis): partim fugiendas esse nimias amicitias, ne necesse sit unum sollicitum esse pro pluribus; satis superque esse sibi suarum cuique rerum, alienis nimis implicari molestum esse; commodissimum esse quam laxissimas habenas habere amicitiae, quas vel adducas, cum velis, vel remittas; caput enim esse ad beate vivendum securitatem, qua frui non possit animus, si tamquam parturiat unus pro pluribus.")
    ),
    
    tags$div(class = "element",
             id = "part3",
             tags$h1("Partie 3"),
             tags$p("Nam quibusdam, quos audio sapientes habitos in Graecia, placuisse opinor mirabilia quaedam (sed nihil est quod illi non persequantur argutiis): partim fugiendas esse nimias amicitias, ne necesse sit unum sollicitum esse pro pluribus; satis superque esse sibi suarum cuique rerum, alienis nimis implicari molestum esse; commodissimum esse quam laxissimas habenas habere amicitiae, quas vel adducas, cum velis, vel remittas; caput enim esse ad beate vivendum securitatem, qua frui non possit animus, si tamquam parturiat unus pro pluribus.")
    )
  ),
  
  
  
  # script pour sidebar
  tags$script(
    HTML(
      'function w3_open() {
      document.getElementById("mySidebar").style.display = "block";
    }
    
    function w3_close() {
      document.getElementById("mySidebar").style.display = "none";
    }'
    )
  )
)


server <- function(input, output){}


shinyApp(ui, server)