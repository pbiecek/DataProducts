## Create school type list. Remove undefined schools from the choice list (only 6 schools, shouldn't be an issue)

rodzaj_szkoly <- all_data_geo %>% filter(typ_szkoly!='ZZ') %>% select(typ_szkoly) %>% distinct %>% unlist

## Create names for list of examination

names(rodzaj_szkoly) <- rodzaj_szkoly

## Make a list assigning exam type to each school type

typy_egzaminow <- lapply(rodzaj_szkoly,
                         function(x) all_data_geo %>%
                           filter(typ_szkoly==x) %>%
                           select(rodzaj_egzaminu) %>%
                           na.omit %>%
                           distinct %>%
                           unlist(use.names=FALSE))


## Create names for UI
## ?? Seems like reusing it like that is dirty and should be redone ??

lista_egzaminow <- lapply(rodzaj_szkoly,
                          function(x) all_data_geo %>%
                            filter(typ_szkoly == x) %>%
                            select(skrot) %>%
                            na.omit %>%
                            distinct)

names(rodzaj_szkoly) <- c('gimnazjum', 'technikum uzupełniające', 'liceum ogólnokształcące', 'technikum',
                          'szkoła podstawowa')


header <- dashboardHeader(title = "Szkoly", titleWidth = 150)

sidebar <- dashboardSidebar(width = 250,
                           sidebarMenu(
                             menuItem("Mapa", tabName = "mapa", icon = icon("map")),
                             menuItem("Tabela", tabName = "tabela", icon = icon("table")),
                             menuItem("Testing", tabName = "testing", icon = icon("table")),
                             selectInput('rodzaj', label = 'Rodzaj szkoły',
                                         choices = rodzaj_szkoly),
                             sidebarMenuOutput("wagi_czesci"),
                             sliderInput("wybierz_lata", "Wybierz lata", min = 2002, max = 2015,
                                         value = c(2002,2015), ticks = FALSE, sep = '')
                            )
                           )

body <- dashboardBody(
  tags$style(type = "text/css", "#mapa {height: calc(100vh - 80px) !important;}"),
  tabItems(
    tabItem(tabName = "mapa",
            leafletOutput("mapa")
    ),
    tabItem(tabName = "tabela",
            DT::dataTableOutput("schools_in_view")
    ),
    tabItem(tabName = 'testing',
            textOutput('teshting'))
  )
)
  
dashboardPage(
  header,
  sidebar,
  body
)