
library(shiny)
library(knitr)

shinyUI(fluidPage(
  br(),
  img(src = "logo_web.png", width = "300px", height = "50px"),
  br(),
  br(),
  titlePanel(h2("DISTANCE-SAMPLING-ANALYSER: KURSMODUL")),
  br(),
  hr(),
  
  br(),
  br(),
  br(),
  sidebarLayout(position="left", 
    sidebarPanel(
      helpText(h4("Oversikt"),p("Her dukker det opp hjelpefunksjoner som er tilpasset
               den siden du er på. Dette gjør det mulig for deg å laste opp kursfila
                og utforske datasettt på en enkel måte")),
      
      tags$hr(),
      conditionalPanel(
        "$('li.active a').first().html()==='Last opp kursfil'",
      
      helpText("Her kan du laste opp og se på kursfila. Dersom den ser merkelig ut 
                   skyldes dette trolig at du ikke har valgt riktig filformat i menyen til venstre. Sørg
               for at du har lastet opp fila i riktig format før du går videre"),  
        
      fileInput('file1', 'Se på kursfila',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      
      
      radioButtons('sep', 'Filformat',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ';'),
      br(),
      br()
      
      ),
      
      
      conditionalPanel(
        "$('li.active a').first().html()==='Histogram'",
        helpText("Velg hvor mange grupperinger (søyler) du ønsker i hisogrammet som vises til høyre på siden"),
        numericInput("bins1", 'Antall grupperinger', value=5, width="170px"),
        selectInput("histcol", "Farge på histogram", choices=c(Blå="cornflowerblue", Grå="grey", 
                                                               orange="darkgoldenrod"), width="170px")
        ),
      
      
      conditionalPanel(
        "$('li.active a').first().html()==='Summarisk oversikt'",
        helpText("Velg hvilken målestokk som er benyttet for de oppgitte linjeavstander og
                 linjelengder, og hvorvidt du ønsker oversikt linje for linje eller samlet"),
        br(),
        radioButtons("desc1", 'Vis oversikt', c(Linjevis='lin', Samlet='saml'), 'lin'),
        br(),
        radioButtons("LiAv", 'Linjeavstand er oppgitt i', c(Meter='meter', Kilometer='km'), 'meter'),
        br(),
        radioButtons("Taks", 'Linjelengde er oppgitt i', c(Meter='meter', Kilometer='km'), 'meter'),
        br()
       
        
        
      ),
      
      conditionalPanel(
        "$('li.active a').first().html()==='Distance Sampling'",
        helpText("Her kan du gjøre diverse valg knyttet til analysene og 
                  hvordan du vil se på resultatene. Når du har gjort dine valg, 
                  trykk på knappen Analyser data"),
        br(),
        br(),
        radioButtons("Dens", 'Tetthet måles i', c(Kvadratmeter='meter', Kvadratkilometer='km'), 'km'),
        br(),
        br(),
        numericInput("bins2", 'Antall grupperinger', value=7, width="170px"),
        br(),
        br(),
        actionButton("go_analyse", "Analyser data!", icon("thumbs-up")),
        br()
        
        )
     
      
    ),
    mainPanel(width=5,
      tabsetPanel(type="pills",
        tabPanel("Hjem",
                 
                 br(),
                 br(),
                 h4("Velkommen til Hønsefuglportalens analyseportal for takseringskurs"),
                 br(),
                 p("Disse sidene er primært ment å være en hjelp i forbindelse med kurs i linjetaksering
                   av hønsefugl basert på Distance Sampling. En nærmere beskrivelse av 
                   takseringene finner dere på ", 
                   tags$a(href="http://honsefugl.nina.no/Innsyn/", "Hønsefuglportalen,"),
                   "og en nærmere beskrivelse av kursinnholdet finner dere på",
                   tags$a(href="http://honsefugl.nina.no/Innsyn/Home/Kurs", "Hønsefuglportalens kurssider."), align="justify"),
                 br(),
                 p("Det er viktig å merke seg at funksjonene her er forenklet og tilpasset testdata som samles inn
                   i forbindelse med gjennomføring av kurs. Før du tar disse sidene i bruk har du vanligvis gjennomført
                   en kursøvelse hvor man går langs linjer og registrerer (NOE??) langs disse. Hver gang man 
                   ser et objekt vil man notere hvor mange det er (flokkstørrelse) samt hvor langt fra
                   takseringslinja observasjonen er gjort. Dette er også helt sentral informasjon når man gjennomfører
                   ekte linjetaksering av hønsefugl basert Distance-sampling metoder"),
                 br(),
                 p("Før du starter må du ha klargjort en testfil. I kurssammenheng vil dette vanligvis være resultatet av
                   test-gjennomføringen av linjetaksering. Det er viktig at fila inneholder følgende kolonner, i nevnte rekkefølge:")
                 
                 
                 ),
        
        tabPanel("Last opp kursfil",
                 br(),
                 br(),
                 h4("Oversikt over kursfila"),
                 p(""),
                 br(),
                 tableOutput('contents')),

        tabPanel("Histogram",
                 br(),
                 br(),
                 h4("Ovsersikt over linjeavstander"),
                 br(),
                 plotOutput("distPlot")),

        
        tabPanel("Summarisk oversikt", 
                 br(),
                 br(),
                 h4("Summarisk oversikt over datasettet"),
                 br(),
                 br(),
                 tableOutput("Deskr")),
        
        tabPanel("Distance Sampling", 
                 br(),
                 br(),
                 h4("Distance sampling-analyser"),
                 br(),
                 p("Oversikt over estimert tetthet (total og cluster)"),
                 br(),
                 tableOutput("Distance"),
                 br(), 
                 br(),
                 plotOutput("Distance3", width="600px"))
        
      )
    )
  ),
  hr()
)
)