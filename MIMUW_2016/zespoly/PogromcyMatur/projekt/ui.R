
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

# słownik użyty do konwersji:
# {'bio': 'biologia', 'his': 'historia', 'mat': 'matematyka', 'fiz': 'fizyka', 'pods': 'matura podstawowa', 'rozsz': 'matura rozszerzona', 'wos': 'wos', 'ang': 'jezyk angielski', 'podst': 'matura podstawowa', 'gm': 'egzamin gimnazjalny', 'chem': 'chemia', 'geo': 'geografia', 'inf': 'informatyka', 'pl': 'jezyk polski', 'prz': 'cz. matematyczno-przyrodnicza'}


choice_names = c(
  "egzamin gimnazjalny - historia - 2012",
  "matura rozszerzona - matematyka - 2015",
  "matura podstawowa - jezyk polski - 2015",
  "matura podstawowa - informatyka - 2015",
  "matura rozszerzona - chemia - 2015",
  "matura podstawowa - wos - 2015",
  "matura rozszerzona - jezyk angielski - 2015",
  "egzamin gimnazjalny - jezyk polski - 2012",
  "matura podstawowa - jezyk angielski - 2015",
  "matura rozszerzona - fizyka - 2015",
  "matura podstawowa - matematyka - 2015",
  "matura podstawowa - chemia - 2015",
  "matura rozszerzona - informatyka - 2015",
  "egzamin gimnazjalny - cz. matematyczno-przyrodnicza - 2012",
  "matura podstawowa - fizyka - 2015",
  "matura rozszerzona - jezyk polski - 2015",
  "egzamin gimnazjalny - matematyka - 2012",
  "matura podstawowa - biologia - 2015",
  "matura rozszerzona - geografia - 2015",
  "matura podstawowa - geografia - 2015",
  "matura rozszerzona - biologia - 2015"
)

choice_vals = c(
  "gm_his_2012",
  "rozsz_mat_2015",
  "pods_pl_2015",
  "pods_inf_2015",
  "rozsz_chem_2015",
  "pods_wos_2015",
  "rozsz_ang_2015",
  "gm_pl_2012",
  "pods_ang_2015",
  "rozsz_fiz_2015",
  "pods_mat_2015",
  "pods_chem_2015",
  "rozsz_inf_2015",
  "gm_prz_2012",
  "pods_fiz_2015",
  "rozsz_pl_2015",
  "gm_mat_2012",
  "pods_bio_2015",
  "rozsz_geo_2015",
  "pods_geo_2015",
  "rozsz_bio_2015"
)

choices = setNames(as.list(choice_vals), choice_names)

shinyUI(pageWithSidebar(
  
  headerPanel("Trzecia iteracja"),
  
  sidebarPanel(
    selectInput("egzamin", label = "Egzamin:",
                choices = choices),
    checkboxInput("is_scatterplot", "Scatterplot zamiast wykresu słupkowego.",
                  value = TRUE),
    actionButton("gen", "Generuj")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Kobieta/mężczyzna",
          div(
            # style = "position:relative",
            plotOutput("plec_plot", clickId = "plec_hover"
                      #click = clickOpts("plec_hover"#, delay = 100, delayType = "debounce")
                                        ),
            uiOutput("plec_hover_info", height="800px")
          )
        ),
        tabPanel("Wieś/miasto",
          plotOutput("wies_plot")
        ),
        tabPanel("Histogram",
            plotOutput("histogram_plot")
        ),
        tabPanel("Szczegóły",
            plotOutput("szczegoly_ui", height="4000px", width="600px")
        )
      )
  )
))