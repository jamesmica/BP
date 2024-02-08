library(shiny)
library(shinydashboard)
library(shinydashboardPlus, include.only = c("carousel","carouselItem","dashboardPage","dashboardHeader","dashboardFooter","dashboardSidebar"))
library(magrittr,include.only = "%>%")
library(urltools)
library(dplyr,include.only = "filter")
library(shinyWidgets,include.only = c("pickerInput","pickerOptions"))
library(waiter)

library(googleAnalyticsR)

options(shiny.port=1410)

secrets<-jsonlite::fromJSON("client.json",flatten = TRUE)

filtre_compa<-data.frame(1)
OK<-data.frame(1)

client <- secrets[[1]]$client_id
secret <- secrets[[1]]$client_secret

options(googleAuthR.client_id = client)
options(googleAuthR.client_secret = secret ) 



# Provide the service account email and private key
googleAnalyticsR::ga_auth(email = "james.michelot@ithea-conseil.fr",
        json_file = "clientt.json")

# Get a list of all GA4 metrics
# metrics_list = ga_meta(version = "data",propertyId = "417806289")
# 
# ga_auth()
# Set up GA property ID
property_id <- "417806289"

# Test API is working
basic <- ga_data(
  property_id,
  metrics = c("screenPageViews"),
  dimensions = c("fullPageUrl"),
  date_range = c("2023-01-01", "2030-12-28")
)

basic <- basic %>%
  filter(grepl("pageid=BP",fullPageUrl,ignore.case = TRUE)) %>%
  filter(grepl("ithea",fullPageUrl,ignore.case = TRUE))

basic$fullPageUrl<-as.numeric(gsub("\\D", "", basic$fullPageUrl))
basic <- basic[order(basic$screenPageViews,decreasing=TRUE),]

options(shiny.maxRequestSize=30*1024^2)

BDD <- readxl::read_excel("BDD_final.xlsx")
BDD$X<-as.character(BDD$X)
BDD$Y<-as.character(BDD$Y)
BDD$CODE_COMEPCI<-as.character(BDD$CODE_COMEPCI)
BDD$CODE_COM<-as.character(BDD$CODE_COM)
BDD[is.na(BDD)] <- ""
BDD$THEMA<-paste0(BDD$THEMA1,BDD$THEMA2,BDD$THEMA3,BDD$THEMA4)
BDD<-merge(x = BDD,y=basic,by.x="N°",by.y="fullPageUrl",all.x = TRUE)


themes <- readxl::read_excel("thematiques.xlsx")
dep <- readxl::read_excel("département.xlsx")
per <- readxl::read_excel("pertinence.xlsx",col_types = c("text","text","text","text","text","text","text","text","text","text"))

names(per$INSEE)<-per$NOM2
THEMAS<-unique(levels(as.factor(themes$THEMA)))
names(dep$CODE)<-dep$NOM


 # url <- "https://ithea-conseil.shinyapps.io/Plateforme_Bonnes_Pratiques/"
  url <- "http://127.0.0.1:1410/"

BDD$BP_link_plateforme <- paste0(url,"?tab=bp_show","&","?pageid=BP_",BDD$`N°`)

BDD_accueil<-BDD[order(BDD$screenPageViews,decreasing = TRUE),]

# Define UI for application that draws a histogram
header<-dashboardHeader()
body<-dashboardBody(tags$head(HTML("
  <meta name='description' content='Plateforme de visualisation de données en Open Data pour les collectivités territoriales et les entreprises. INSEE, CAF, DataCaf, Pôle Emploi...'>
  <meta name='keywords' content='HTML, CSS, JAVASCRIPT, INSEE, CAF, Pole Emploi, Open Data'>
  <meta name='author' content='Ithea Conseil'>
  <meta name='viewport' content='width=device-width, initial-scale=1.0'>"),
  includeCSS("www/styles.css"),
  tags$script(src = "custom.js"),
  HTML("<!-- Google tag (gtag.js) -->
         <script async src='https://www.googletagmanager.com/gtag/js?id=G-1ZBDNSFN2J'></script>
         <script>
         window.dataLayer = window.dataLayer || [];
       function gtag(){dataLayer.push(arguments);}
       gtag('js', new Date());
       
       gtag('config', 'G-1ZBDNSFN2J');
       </script>"
  ),
  # tags$script(HTML(js)),
  tags$link(rel = "icon", sizes = "16x16", href = "favicon1.ico")),
  shinybusy::busy_start_up(
    loader = tags$img(
      src = "GIF_ithea.gif",
      width = 160
    ),
    text = "Chargement...",
    mode = "timeout",background = "#00735B",timeout = 3000,color = "white"
  ),
  
  
  
  
  
  tabItems(
    
    tabItem(
      
      tabName = "accueil",
      tags$div(id="accueil_wrapper",
               tags$div(class="green_div",
                        tags$div(id="accueil_text",
                                 h2("BIBLIOTHÈQUE DE BONNES PRATIQUES"),
                                 h3("La première banque d’initiatives innovantes portées par les collectivités de France et à l’étranger"),
                                 h3("+ de 500 Bonnes Pratiques"),tags$div(id="collectivite?",
                                                                           tags$div(id="ac",pickerInput(inputId="avec_collectivite",label = NULL,choices=per$INSEE,multiple=T,options = pickerOptions(liveSearch = T,size = 9,`none-selected-text` = "Saisir une collectivité",maxOptions = 1)),actionButton(inputId = "ok",label = "OK")),
                                                                           tags$a(id="sc",tags$div(id="sans_collectivite","Continuer sans collectivité 〉"),onclick="openTab('decouvrir')",href="#"))),
                        tags$div(id="accueil_img",
                                 HTML(paste0('
                                      <div class="csslider1 autoplay cs_handle" style="width:300px;">
  <input name="cs_anchor1" id="cs_slide1_0" type="radio" class="cs_anchor slide">
  <input name="cs_anchor1" id="cs_slide1_1" type="radio" class="cs_anchor slide">
  <input name="cs_anchor1" id="cs_slide1_2" type="radio" class="cs_anchor slide">
    <input name="cs_anchor1" id="cs_slide1_3" type="radio" class="cs_anchor slide">
    <input name="cs_anchor1" id="cs_slide1_4" type="radio" class="cs_anchor slide">
  <input name="cs_anchor1" id="cs_play1" type="radio" class="cs_anchor" checked="">
  <input name="cs_anchor1" id="cs_pause1_0" type="radio" class="cs_anchor pause">
  <input name="cs_anchor1" id="cs_pause1_1" type="radio" class="cs_anchor pause">
  <input name="cs_anchor1" id="cs_pause1_2" type="radio" class="cs_anchor pause">
    <input name="cs_anchor1" id="cs_pause1_3" type="radio" class="cs_anchor pause">
    <input name="cs_anchor1" id="cs_pause1_4" type="radio" class="cs_anchor pause">

  <ul>
	  <div>
		  <img src="img/',BDD_accueil$`@IMG`[1],'" style="width: 100%;">
	  </div>
	  <li class="num0 img">
		  <a href="',BDD_accueil$BP_link_plateforme[1],'" target="_blank">
  <h6>',BDD_accueil$INTITULE[1],'</h6>
			  <img src="img/',BDD_accueil$`@IMG`[1],'" alt="" title="">
		  </a>
	  </li>
	  <li class="num1 img">
		  <a href="',BDD_accueil$BP_link_plateforme[2],'" target="_blank">
  <h6>',BDD_accueil$INTITULE[2],'</h6>
			  <img src="img/',BDD_accueil$`@IMG`[2],'" alt="" title="">
		  </a>
	  </li>
	  <li class="num2 img">
		  <a href="',BDD_accueil$BP_link_plateforme[3],'" target="_blank">
  <h6>',BDD_accueil$INTITULE[3],'</h6>
			  <img src="img/',BDD_accueil$`@IMG`[3],'" alt="" title="">
		  </a>
	  </li>
  <li class="num3 img">
		  <a href="',BDD_accueil$BP_link_plateforme[4],'" target="_blank">
  <h6>',BDD_accueil$INTITULE[4],'</h6>
			  <img src="img/',BDD_accueil$`@IMG`[4],'" alt="" title="">
		  </a>
	  </li>
  <li class="num4 img">
		  <a href="',BDD_accueil$BP_link_plateforme[5],'" target="_blank">
  <h6>',BDD_accueil$INTITULE[5],'</h6>
			  <img src="img/',BDD_accueil$`@IMG`[5],'" alt="" title="">
		  </a>
	  </li>
  </ul>

	<div class="cs_play_pause">
		<label class="cs_play" for="cs_play1">►</label>
	</div>

  <div class="cs_arrowprev">
		  <label class="num0" for="cs_slide1_0"><span><i></i></span></label>
		  <label class="num1" for="cs_slide1_1"><span><i></i></span></label>
		  <label class="num2" for="cs_slide1_2"><span><i></i></span></label>
  <label class="num3" for="cs_slide1_3"><span><i></i></span></label>
  <label class="num4" for="cs_slide1_4"><span><i></i></span></label>
  </div>
  <div class="cs_arrownext">
		  <label class="num0" for="cs_slide1_0"><span><i></i></span></label>
		  <label class="num1" for="cs_slide1_1"><span><i></i></span></label>
		  <label class="num2" for="cs_slide1_2"><span><i></i></span></label>
  <label class="num3" for="cs_slide1_3"><span><i></i></span></label>
  <label class="num4" for="cs_slide1_4"><span><i></i></span></label>
  </div>

  <div class="cs_bullets">
		  <label class="num0" for="cs_slide1_0">
			  <span class="cs_point"></span>
		  </label>
		  <label class="num1" for="cs_slide1_1">
			  <span class="cs_point"></span>
		  </label>
		  <label class="num2" for="cs_slide1_2">
			  <span class="cs_point"></span>
		  </label>
  <label class="num3" for="cs_slide1_3">
			  <span class="cs_point"></span>
		  </label>
  <label class="num4" for="cs_slide1_4">
			  <span class="cs_point"></span>
		  </label>
  </div>
</div>'))
                                 
                        )
               )
      )
    ),
    
    tabItem(
      
      tabName = "thematiques",
      tags$div(id="thematiques_wrapper",class="flex-column",
               h2(class="titreBP","BIBLIOTHÈQUE DE BONNES PRATIQUES"),
               tags$div(class="green_div_column",
                        
                        h3("Thématiques"),
                        tags$div(class="themes",
                                 tags$div(class="ombre carre-thematique",actionLink(inputId = "TH1",label = NULL, icon = NULL,tags$img(src="picto/Attractivité.png",height="80%",alt="Démographie_logo"),"Attractivité du territoire",onclick="openTab('decouvrir')",href="#")),
                                 tags$div(class="ombre carre-thematique",actionLink(inputId = "TH2",label = NULL, icon = NULL,tags$img(src="picto/Jeunesse.png",height="80%",alt="Démographie_logo"),"Jeunesse",onclick="openTab('decouvrir')",href="#")),
                                 
                                 tags$div(class="ombre carre-thematique",actionLink(inputId = "TH3",label = NULL, icon = NULL,tags$img(src="picto/Enfance & éducation.png",height="80%",alt="petite_enfance_logo"),"Enfance & éducation",onclick="openTab('decouvrir')",href="#")),
                                 tags$div(class="ombre carre-thematique",actionLink(inputId = "TH4",label = NULL, icon = NULL,tags$img(src="picto/Mobilités.png",height="80%",alt="enfance_jeunesse_logo"),"Mobilités",onclick="openTab('decouvrir')",href="#")),
                                 tags$div(class="ombre carre-thematique",actionLink(inputId = "TH5",label = NULL, icon = NULL,tags$img(src="picto/Solidarités & action sociale.png",height="80%",alt="familles_logo"),"Solidarités & action sociale",onclick="openTab('decouvrir')",href="#")),
                                 tags$div(class="ombre carre-thematique",actionLink(inputId = "TH6",label = NULL, icon = NULL,tags$img(src="picto/Sport, culture, loisirs.png",height="80%",alt="séniors_logo"),"Sport, culture, loisirs",onclick="openTab('decouvrir')",href="#")),
                                 tags$div(class="ombre carre-thematique",actionLink(inputId = "TH7",label = NULL, icon = NULL,tags$img(src="picto/Logement & habitat.png",height="80%",alt="logement_logo"),"Logement & habitat",onclick="openTab('decouvrir')",href="#")),
                                 tags$div(class="ombre carre-thematique",actionLink(inputId = "TH8",label = NULL, icon = NULL,tags$img(src="picto/Vie de la collectivité.png",height="80%",alt="habitat_logo"),"Vie de la collectivité",onclick="openTab('decouvrir')",href="#")),
                                 tags$div(class="ombre carre-thematique",actionLink(inputId = "TH9",label = NULL, icon = NULL,tags$img(src="picto/Egalité F-H.png",height="80%",alt="emploi_logo"),"Égalité femmes-hommes",onclick="openTab('decouvrir')",href="#")),
                                 tags$div(class="ombre carre-thematique",actionLink(inputId = "TH10",label = NULL, icon = NULL,tags$img(src="picto/Santé & handicap.png",height="80%",alt="revenus_précarité_logo"),"Santé & handicap",onclick="openTab('decouvrir')",href="#")),
                                 tags$div(class="ombre carre-thematique",actionLink(inputId = "TH11",label = NULL, icon = NULL,tags$img(src="picto/Emploi & entreprises.png",height="80%",alt="égalité_femmes_hommes_logo"),"Emploi & entreprises",onclick="openTab('decouvrir')",href="#")),
                                 tags$div(class="ombre carre-thematique",actionLink(inputId = "TH12",label = NULL, icon = NULL,tags$img(src="picto/Vie locale.png",height="80%",alt="santé_handicap_logo"),"Vie locale",onclick="openTab('decouvrir')",href="#")),
                                 tags$div(class="ombre carre-thematique",actionLink(inputId = "TH13",label = NULL, icon = NULL,tags$img(src="picto/Participation citoyenne.png",height="80%",alt="Démographie_logo"),"Participation citoyenne",onclick="openTab('decouvrir')",href="#")),
                                 
                                 tags$div(class="ombre carre-thematique",actionLink(inputId = "TH14",label = NULL, icon = NULL,tags$img(src="picto/Environnement & urbanisme.png",height="80%",alt="entreprises_logo"),"Environnement & urbanisme",onclick="openTab('decouvrir')",href="#")),
                                 tags$div(class="ombre carre-thematique",actionLink(inputId = "TH15",label = NULL, icon = NULL,tags$img(src="picto/Finances publiques.png",height="80%",alt="vie_locale_logo"),"Finances publiques",onclick="openTab('decouvrir')",href="#")))
               )
      )
    ),
    
    tabItem(
      
      tabName = "decouvrir",
      tags$div(id="decouvrir_wrapper",class="flex-column",
               h2(class="titreBP","BIBLIOTHÈQUE DE BONNES PRATIQUES"),
               tags$div(id="filtres",
                        shinyWidgets::pickerInput(inputId = "Thematiques",selected=NULL,multiple=TRUE,label=NULL,choices=split(themes$SOUSTHEMA,themes$THEMA),
                        width = "208px",options = pickerOptions(
                          `actions-box` = TRUE,
                          `deselect-all-text` = "Tout déselectionner",
                          `select-all-text` = "Tout sélectionner",
                          `none-selected-text` = "Thématiques",liveSearch = TRUE
                        )),
                        shinyWidgets::pickerInput(inputId = "Habitants",multiple=TRUE,selected = NULL,label=NULL,
                                                  choices=c("Moins de 500 habitants","Entre 500 et 1 000 habitants","Entre 1 000 et 2 500 habitants","Entre 2 500 et 10 000 habitants","Entre 10 000 et 50 000 habitants","Entre 50 000 et 80 000 habitants","Entre 80 000 et 100 000 habitants","Plus de 100 000 habitants"),width = "320px",
                                                  options = pickerOptions(
                                                    `actions-box` = TRUE,
                                                    `deselect-all-text` = "Tout déselectionner",
                                                    `select-all-text` = "Tout sélectionner",
                                                    `none-selected-text` = "Nombre d'habitants"
                                                  )),
                        shinyWidgets::pickerInput(inputId = "Pays",selected=NULL,multiple = TRUE,label=NULL,choices=dep$CODE,width = "208px",options = pickerOptions(placeholder="Département", liveSearch = TRUE,`none-selected-text` = "Département",`actions-box` = TRUE,
                                                                                                                                                                     `deselect-all-text` = "Tout déselectionner",
                                                                                                                                                                     `select-all-text` = "Tout sélectionner")),
                        textInput(inputId = "Motscles",label=NULL,width = "208px", placeholder="Mots-clés"),
                        pickerInput(inputId = "BP_nb",label = "Afficher :",choices = c(20,50,100,200,500),selected = 20)),
               
               
               tags$div(class="green_div_column",
                        tags$div(id="BP",uiOutput("BPs")))
      )
    ),

    
    tabItem(
      
      tabName = "bp_show",
      tags$div(id="bp_wrapper",class="flex-column",
               h2(class="titreBP","BIBLIOTHÈQUE DE BONNES PRATIQUES"),
               tags$div(class="green_div_column",uiOutput("BP_page"))
      )
    )
  )
)


sidebar <- dashboardSidebar(id="sidebar",tags$style(".sidebar-collapse .main-sidebar {width:80px;} #sidebar {background-color:#00753B;}"),
                            minified = TRUE, collapsed = T,
                            shinydashboard::sidebarMenu(id="sidebarID",
                              tags$a(tags$img(src="logo.webp",alt="logo"),onclick="openTab('accueil')",href="#",""),
                              tags$a(tags$img(src="logo_crop.webp",alt="logo_crop"),onclick="openTab('accueil')",href="#",""),
                              menuItem("bp_show",tabName="bp_show", icon = icon("home")),
                              menuItem("Accueil",tabName="accueil", icon = icon("home"),selected = TRUE),
                              menuItem("Thématiques",tabName="thematiques", icon = icon("home")),
                              menuItem("Tout découvrir",tabName="decouvrir", icon = icon("home"))
                              
                            ))

ui = shinydashboardPlus::dashboardPage(title = "Plateforme Bonnes Pratiques",
                                       header,
                                       sidebar,
                                       footer = NULL,
                                       body,
                                       options = list(sidebarExpandOnHover = TRUE)
)




server <- function(input, output, session) {
  

 
  observeEvent(c(input$TH1,input$TH2,input$TH3,input$TH4,input$TH5,input$TH6,input$TH7,input$TH8,
            input$TH9,input$TH10,input$TH11,input$TH12,input$TH13,input$TH14,input$TH15),{
    
              
              

              
    
    vector1<-c(input$TH1,input$TH2,input$TH3,input$TH4,input$TH5,input$TH6,input$TH7,input$TH8,
               input$TH9,input$TH10,input$TH11,input$TH12,input$TH13,input$TH14,input$TH15)

 
    shinyWidgets::updatePickerInput(session = session,inputId = "Thematiques",selected = NULL )
    
        
    # vector1<-as.integer(c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0))
index<-1:15
index<-as.data.frame(index)
index<-data.frame(index,vector1)

index<-index %>%
  filter(vector1 == 1)


filtre_thema<-themes %>%
  filter(UNIC %in% index$index)



if (!is.numeric(filtre_compa[[1]])) {

  filtre_thema <- filtre_thema %>%
    filter(!UNIC %in% as.numeric(unlist(filtre_compa)))

} else {

}

filtre_compa <- unique(levels(factor(c(filtre_thema$UNIC,unlist(filtre_compa)))))


filtre_compa<<-data.frame(filtre_compa)

    
    shinyWidgets::updatePickerInput(session = session,inputId = "Thematiques",selected =filtre_thema$SOUSTHEMA )
    
  },ignoreInit = TRUE,ignoreNULL = TRUE)
  
  
  
  observeEvent(getQueryString(session)$tab, {
    currentQueryString <- getQueryString(session)$tab # alternative: parseQueryString(session$clientData$url_search)$tab
    if(is.null(input$sidebarID) || !is.null(currentQueryString) && currentQueryString != input$sidebarID){
      freezeReactiveValue(input, "sidebarID")
      updateTabItems(session, "sidebarID", selected = currentQueryString)
    }
  }, priority = 1)
  

  observeEvent(input$sidebarID, {
    currentQueryString <- getQueryString(session)$tab # alternative: parseQueryString(session$clientData$url_search)$tab
    pushQueryString <- paste0("?tab=", input$sidebarID)
    if(is.null(currentQueryString) || currentQueryString != input$sidebarID){
      freezeReactiveValue(input, "sidebarID")
      updateQueryString(pushQueryString, mode = "push", session)
    }
  }, priority = 0)
  
 
  



  ##initial_rendering_BP
  observeEvent(c(input$Motscles,input$Habitants,input$Thematiques,input$Pays,input$ok,input$BP_nb),{
    
    BDD<-dplyr::distinct(BDD,INTITULE,.keep_all = TRUE)

      motscles_df<-BDD[,c("THEMA1","THEMA2","THEMA3","THEMA4","MOT-CLES","SYNONYMES 1","SYNONYMES 2","SYNONYMES 3","SYNONYMES 4","INTITULE")]

      BDD$mots<-apply( motscles_df , 1 , paste , collapse = "-")

      
      
      if (!is.null(input$Motscles)) {
        BDD <-  BDD %>%
          dplyr::filter(grepl(input$Motscles,mots,ignore.case = TRUE))
        BDD<-BDD[complete.cases(BDD$`N°`),]
  

      } else {
        
      }
      
      if (!is.null(input$Habitants)) {
        BDD <-  BDD %>%
          dplyr::filter(BDD$STRATE %in% input$Habitants)

      } else {
        
      }
      if (!is.null(input$Pays)) {
      BDD <-  BDD %>%
        dplyr::filter(BDD$DEP %in% input$Pays)

      } else {

      }
      
      
      if (!is.null(input$Thematiques)) {
        BDD <-  BDD %>%
          dplyr::filter(grepl(paste0(input$Thematiques,collapse = "|"),THEMA,ignore.case = TRUE)) 


      } else {
        
      }
      


      BDD<-BDD[complete.cases(BDD$`N°`),]
  
      mini<-min(c(as.numeric(input$BP_nb),length(BDD$`N°`)))
      
      ###----PERTINENCE------
      if (!is.null(input$avec_collectivite)) {
    
        
        shinydashboard::updateTabItems(inputId = "sidebarID",selected = "decouvrir")
        
        BDD_score <- per %>%
          filter(INSEE == input$avec_collectivite)

        BDD$score<-as.numeric(0)
        
      
        
        ##---pertinence----COM/EPCI
        
        if (BDD_score$`COM/EPCI`=="COM") {
          for (i in 1:length(BDD$ANNEE)) {
            if (nchar(BDD$ID[i])<7 & nchar(BDD$ID[i])>3) {
              BDD$score[i] <- 0.7
            } else {
              BDD$score[i] <- 0
            }
          }
        } else {
          for (i in 1:length(BDD$ANNEE)) {
            if (nchar(BDD$ID[i])>7) {
              BDD$score[i] <- 0.7
            } else {
              BDD$score[i] <- 0
            }
          }
        }
    BDD3<<-BDD

        ##---pertinence----STRATE
        
        for (i in 1:length(BDD$STRATE2)) {
          
          if (abs(as.numeric(BDD_score$STRATE[1]) - BDD$STRATE2[i])>6) {
            BDD$score[i]<-as.numeric(BDD$score[i])+0
          } else if (abs(as.numeric(BDD_score$STRATE[1]) - BDD$STRATE2[i])>5) {
            BDD$score[i]<-as.numeric(BDD$score[i])+0.16
          } else if (abs(as.numeric(BDD_score$STRATE[1]) - BDD$STRATE2[i])>4) {
            BDD$score[i]<-as.numeric(BDD$score[i])+0.24
          } else if (abs(as.numeric(BDD_score$STRATE[1]) - BDD$STRATE2[i])>3) {
            BDD$score[i]<-as.numeric(BDD$score[i])+0.36
          } else if (abs(as.numeric(BDD_score$STRATE[1]) - BDD$STRATE2[i])>2) {
            BDD$score[i]<-as.numeric(BDD$score[i])+0.5
          } else if (abs(as.numeric(BDD_score$STRATE[1]) - BDD$STRATE2[i])>1) {
            BDD$score[i]<-as.numeric(BDD$score[i])+0.8
          } else {
            BDD$score[i]<-as.numeric(BDD$score[i])+1
          }
        }
        BDD2<<-BDD

        ##---distance----
        
        BDD$dist<-as.numeric(0)
        
        for (i in 1:length(BDD$ANNEE)) {
          
          ##racine de ((xb-xa)²+(yb-ya)²)
          BDD$dist[i]<-sqrt((as.numeric(BDD$X[i]) - as.numeric(BDD_score$X))^2+(as.numeric(BDD$Y[i])-as.numeric(BDD_score$Y))^2)
        }

        print(length(BDD$dist))
        for (i in 1:length(BDD$dist)) {
          
          ##racine de ((xb-xa)²+(yb-ya)²)
          if (BDD$dist[i]>800000) {
            BDD$score[i]<-as.numeric(BDD$score[i]) + 0
          } else if (BDD$dist[i]>600000) {
            BDD$score[i]<-as.numeric(BDD$score[i]) + 0.1
          } else if (BDD$dist[i]>400000) {
            BDD$score[i]<-as.numeric(BDD$score[i]) + 0.2
          } else if (BDD$dist[i]>200000) {
            BDD$score[i]<-as.numeric(BDD$score[i]) + 0.4
          } else if (BDD$dist[i]>100000) {
            BDD$score[i]<-as.numeric(BDD$score[i]) + 0.7
          } else if (BDD$dist[i]>40000) {
            BDD$score[i]<-as.numeric(BDD$score[i]) + 0.8
          } else if (BDD$dist[i]>=0) {
            BDD$score[i]<-as.numeric(BDD$score[i]) + 1
          }
          
          
        }
        
        
        # BDD <- BDD[order(BDD$score,BDD$dist,decreasing = TRUE),]
        BDD <- BDD %>%
          dplyr::group_by(score) %>%
          dplyr::arrange(desc(score),dist)
        BDD1<<-BDD
      
        
        output$BPs <- renderUI({

          
  
          
          lapply(1:mini, function(i) {
            tags$a(id=paste0("BP_",i),class="BP_nlink",href=BDD$BP_link_plateforme[i],onclick="",tags$div(class="listBP_elem",tags$img(class="listBP_img",src=paste0("img/",BDD$`@IMG`[i])),tags$span(class="listBP_titre",paste0(BDD$INTITULE[i]," - ",BDD$TER[i]," (",BDD$ID[i],")")),tags$span(class="listBP_description",BDD$DESCRIPTION1[i])))
          })
          
          actionButton(inputId = "plus",label = "Voir plus")
       
          
        })
        
        
      } else {
        
      }
    
    

    
    if (length(BDD$`N°`)==0) {
      output$BPs<-renderUI({
        tags$span(id="rien","Votre recherche ne correspond à aucune bonne pratique")
      })
    } else {
      output$BPs <- renderUI({
        

                  
        lapply(1:mini, function(i) {
          tags$a(id=paste0("BP_",i),class="BP_nlink",href=BDD$BP_link_plateforme[i],onclick="",tags$div(class="listBP_elem",tags$img(class="listBP_img",src=paste0("img/",BDD$`@IMG`[i])),tags$span(class="listBP_titre",paste0(BDD$INTITULE[i]," - ",BDD$TER[i]," (",BDD$ID[i],")")),tags$span(class="listBP_description",BDD$DESCRIPTION1[i])))
        })
        
        
      })
    }
      
    
    
  },ignoreInit = FALSE,ignoreNULL = TRUE)
  



  #creating_dynamic_links

  observeEvent(input$sidebarID,{

    
    aa<<-reactiveValuesToList(session$clientData)


  
   BP_id<-aa[["url_search"]]

  BP_id <- substring(text = BP_id,first = 25,last = nchar(BP_id))
    
    
    if (nchar(BP_id)>0) {
      
      
      BDD_BP <- BDD %>%
        dplyr::filter(`N°`== as.numeric(BP_id))
      
      
      BDD_mv<- BDD %>%
        dplyr::filter(grepl(BDD_BP$THEMA1,THEMA,ignore.case = TRUE)) 
      
      BDD_mv <- BDD_mv %>%
        dplyr::filter(BDD_mv$`N°`!=BDD_BP$`N°`[1])
      
      BDD_mv<-BDD_mv[order(BDD_mv$screenPageViews,decreasing = TRUE),] 
        
      output$BP_page<-renderUI({
        
        tagList(tags$div(class="BP_wrapper",
                 tags$div(class="BP_title",
                 h2(BDD_BP$INTITULE),br(),
                 h3(HTML(BDD_BP$PAYS,"|&#8239;","<b>",BDD_BP$TER,"</b>","&#8239;"), paste0("(",format(round(BDD_BP$POP,0),big.mark=" ")," hab.) |"),BDD_BP$ANNEE)),
                 tags$div(class="BP_content",
                          tags$div(class="BP_imgdownload",tags$img(class="BP_img",src=paste0("img/",BDD_BP$`@IMG`)), HTML(paste0('<a href="pdf/Bonnes Pratiques_Partie',BDD_BP$`N°`,'.pdf" download="',BDD_BP$INTITULE,'">Télécharger <i class="fa-solid fa-download"></i></a>'))),
                          tags$div(class="BP_description",BDD_BP$DESCRIPTION1,br(),br(),BDD_BP$DESCRIPTION2,br(),br(),BDD_BP$DESCRIPTION3),
                          tags$div(class="BP_source",tags$div(class="BP_acteur",p(HTML("<b>","Portée par","</b>"),br(),BDD_BP$CREDIT),p(HTML("<b>","Contact","</b>"),br(),BDD_BP$CONTACT),BDD_BP$LINK),tags$span(class="BP_notes",tags$img(src=paste0("graph/",BDD_BP$`@GRAPH`))))
                         
                 )
        ),
        tags$div(class="BP_corr",tags$h3("Bonnes pratiques corrélées"),tags$div(
          tags$div(tags$a(href=BDD_mv$BP_link_plateforme[1],tags$img(src=paste0("img/",BDD_mv$`@IMG`[1])),h5(BDD_mv$INTITULE[1]))),
          tags$div(tags$a(href=BDD_mv$BP_link_plateforme[2],tags$img(src=paste0("img/",BDD_mv$`@IMG`[2])),h5(BDD_mv$INTITULE[2]))),
          tags$div(tags$a(href=BDD_mv$BP_link_plateforme[3],tags$img(src=paste0("img/",BDD_mv$`@IMG`[3])),h5(BDD_mv$INTITULE[3])))
        )))
        
      })
      
      
    } else {
      
      
      
    }


    



  })
  
  observeEvent(input$plus,{
    shinyWidgets::updatePickerInput(inputId = "BP_nb",selected = if (input$plus==20) {
      50
    } else if (input$plus==50) {
      100
    } else if (input$plus==100) {
      200
    } else if (input$plus==200) {
      500
    }
    )
  })

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
