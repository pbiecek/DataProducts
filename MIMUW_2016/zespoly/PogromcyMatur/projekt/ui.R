
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
  "ZPD_gm_his_2012",
  "ZPD_rozsz_mat_2015",
  "ZPD_pods_pl_2015",
  "ZPD_pods_inf_2015",
  "ZPD_rozsz_chem_2015",
  "ZPD_pods_wos_2015",
  "ZPD_rozsz_ang_2015",
  "ZPD_gm_pl_2012",
  "ZPD_pods_ang_2015",
  "ZPD_rozsz_fiz_2015",
  "ZPD_pods_mat_2015",
  "ZPD_pods_chem_2015",
  "ZPD_rozsz_inf_2015",
  "ZPD_gm_prz_2012",
  "ZPD_pods_fiz_2015",
  "ZPD_rozsz_pl_2015",
  "ZPD_gm_mat_2012",
  "ZPD_pods_bio_2015",
  "ZPD_rozsz_geo_2015",
  "ZPD_pods_geo_2015",
  "ZPD_rozsz_bio_2015"
)

choices = setNames(as.list(choice_vals), choice_names)

shinyUI(pageWithSidebar(
  
  headerPanel("Trzecia iteracja"),
  
  sidebarPanel(
    selectInput("egzamin", label = "Egzamin:",
                choices = choices),
    
    checkboxInput("wykresy_woj", "Wykres zależności dla województw?", 
                                 value = FALSE),
    checkboxInput("wykresy_plec", "Wykres zależności dla płci?", 
                  value = FALSE),
    checkboxInput("wykresy_wies", "Wykres zależności przy podziale wieś/miasto?", 
                  value = FALSE),
    checkboxInput("wykresy_szkola", "Wykres zależności dla danych szkół?", 
                  value = FALSE),
    actionButton("gen", "Generuj")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Podsumowanie",
          conditionalPanel(
            condition = "input.wykresy_plec == true",
            plotOutput("plec_plot")
          ),
          conditionalPanel(
            condition = "input.wykresy_plec == true",
            plotOutput("woj_plot")
          ),
          conditionalPanel(
            condition = "input.wykresy_plec == true",
            plotOutput("wies_plot")
          ),
          conditionalPanel(
            condition = "input.wykresy_ == true",
            plotOutput("szkola_plot")
          )
        ),
        tabPanel("Szczegóły",
            plotOutput("szczegoly_ui", height="4000px")
        )
      )
  )
))