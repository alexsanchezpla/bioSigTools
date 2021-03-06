require(shiny)
require(shinydashboard)
require(shinyjs)
require(Rgraphviz)

require(hgu133a.db)
require(org.Hs.eg.db)
require(org.Mm.eg.db)
require(KEGG.db)
require(reactome.db)
require(annotate)

require(GOstats)
require(topGO)
require(GSA)
require(goProfiles)

#Interface d'usuari amb 'dashboardPage'
ui <- dashboardPage(
        dashboardHeader(title = "Gene Set Analyst"),
        dashboardSidebar(
                #Generem el menu lateral amb 'sidebarMenu'
                sidebarMenu(
                        menuItem("Help", tabName = "help", icon = icon("question-circle")),
                        menuItem("Upload", tabName = "upload", icon = icon("upload")),
                        menuItem("Settings", tabName = "settings", icon = icon("gears")),
                        menuItem("Results", tabName = "results", icon = icon("bar-chart"),
                                 conditionalPanel("input.GO_en",
                                                  menuSubItem("GO Enrichment (with GOstats)",
                                                              tabName = 'go_enrich')),
                                 conditionalPanel("input.topGO_en",
                                                  menuSubItem("GO Enrichment (with topGO)",
                                                              tabName = 'topGO_enrich')),
                                 conditionalPanel("input.KEGG_en",
                                                  menuSubItem("KEGG Enrichment", 
                                                              tabName = 'KEGG_enrich'))
                                 )
                        )
                ),
        #Generem els panells de cada apartat del menu lateral amb 'dashboardBody'
        dashboardBody(
                tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                        ),
                tabItems(
                        tabItem(tabName = "help",
                                p("Gene Set Analysis (also known as Functional Analysis) describes a set of methods ",
                                  "intended to perform certain types of analyses on the results of omics data analysis",
                                  "most of which aim at providing a biological interpretation of these results. ",
                                  "This can include (i) identify groups of genes that are over-represented in a larger set of genes, ",
                                  "(ii) identify gene sets that are associated with a difference in phenotypes or ",
                                  "(iii) compare gene lists using their biological annotations."),
                                p("'Gene Set Analyst' is a web tool to perform Gene Set Analysis in three simple steps:"),
                                tags$ol(
                                        tags$li(p(strong("Uploads:"), "In this step the user can upload the different files required for the analysis",
                                                tags$ul(
                                                  tags$li("A file with one or more gene lists (with ENTREZ identifiers), ",
                                                          "(see ", tags$a(href="samples/exampleLst.txt", target="_blank", "example"),")."),
                                                  tags$li("A file with a background list ('Gene Universe'), ",
                                                          "(see ", tags$a(href="samples/exampleBkg.txt", target="_blank", "example"),")."),
                                                  tags$li("A file with one or more 'gene sets' in .gmt format, ",
                                                          "(see ", tags$a(href="samples/exampleGMT.gmt", target="_blank", "example"),").")
                                                )),
                                                p(
                                                  "If you just want to see how this application works, you",
                                                  "can use pre-loaded data. Simply go to the 'Settings' tab,",
                                                  "set the parameters as you wish, and click 'Submit'.")
                                                ), 
                                        tags$li(p(strong("Settings: "), " There are many ways to do Gene Set Analysis",
                                                  "Here one can choose one or more between the followings options:"),
                                                tags$ul(
                                                        tags$li(p(strong("Overrepresentation (Enrichment) Analysis:"), 
                                                                  "Given a list of genes, and a background ('Universe') list ",
                                                                  "enrichment analysis will find (GO or KEGG) terms ",
                                                                  "over-represented (or under-represented) in the list."),
                                                                  p("That analysis can be based on two different packages implementing similar algorithms:"),
                                                                tags$ul(
                                                                        tags$li("GOstats"),
                                                                        tags$li("topGO")
                                                                        )
                                                                ),
                                                        tags$li(p(strong("Gene Set Enrichment Analysis: "), " Given a dataset ('expression matrix') ",
                                                                  "with at least two distinct phenotypes ('groups'), GSEA looks for gene sets that ",
                                                                  "can be associated with the difference between the phenotypes.")
                                                                ),
                                                                p("That analysis can be based on two different packages implementing similar algorithms:"),
                                                                tags$ul(
                                                                    tags$li("GSEA-base"),
                                                                    tags$li("GSA")
                                                                )
                                                          ),
                                                p("You can set a p-value cut-off. Only terms with a ",
                                                  "p-value smaller than the cut off will be shown.")
                                                ), 
                                        tags$li(p(strong("Results: "), " After the computing time (depending on which analysis",
                                                  "chosen and the number of gens of interest), a results table is provided"))
                                        )
                                ),
                        
                        tabItem(tabName = "upload",
                                h2("Choose input file"), br(),
                                fileInput('genesfile', 'Upload Gene List #1 (leave blank to use demo data)',
                                          accept = c('text', '.txt')),
                                fileInput('universefile', 'Upload Background List (leave blank to use demo data)',
                                          accept = c('text', '.txt')),
                                fileInput('gmtfile', 'Upload gene sets (.gmt) file  (leave blank to use demo data)',
                                          accept = c('text', '.txt')),
                                fileInput('genesfile2', 'Upload Gene List #2 (goProfiles analysis only. Leave blank to use demo data)',
                                          accept = c('text', '.txt'))
                                ),
                        
                        tabItem(tabName="settings",
                                h2("Set the parameters of the analysis"), br(),
                                checkboxInput("GO_en", "GO Enrichment Analysis (with GOstats)"),
                                checkboxInput("topGO_en", "GO Enrichment Analysis (with topGO)"),
                                checkboxInput("KEGG_en", "KEGG Enrichment Analysis"), 
                                selectInput("pval", 
                                            label = "p-value cut off", 
                                            choices = list(0.1, 0.05, 0.01, 0.001),
                                            selected = 0.01),
                                actionButton("submit","Submit")),
                        
                        tabItem(tabName = 'go_enrich', h2("GO Enrichment Analysis (with GOstats)"),
                                downloadButton("downloadGOtab", "Download table"),
                                uiOutput("tableGO")),
                        tabItem(tabName = 'topGO_enrich', h2("GO Enrichment Analysis (with topGO)"),
                                downloadButton("downloadtopGOtab", "Download table"),
                                tableOutput("tabletopGO"),
                                fluidRow(
                                        column(6,
                                               column(7, h3("Fisher")), br(),
                                               column(2, downloadButton("downloadtopGOplotF", "Download plot")), 
                                               plotOutput("topGOplotF")),
                                        column(6, 
                                               column(7, h3("KS")), br(),
                                               column(2, downloadButton("downloadtopGOplotKS", "Download plot")),
                                               plotOutput("topGOplotKS")
                                               )
                                        )
                                ),
                        tabItem(tabName = 'KEGG_enrich', h2("KEGG Enrichment Analysis"),
                                downloadButton("downloadKEGGtab", "Download table"),
                                uiOutput("tableKEGG")
                                )
                        )
                )
        )

server <- function(input, output, session) {
        
        #Carreguem les dades de l'usuari o les d'exemple en el seu defecte
        genesID <- reactive({
                
                genesfile <- input$genesfile
                
                if(!is.null(genesfile)){
                        genesID <- read.table(genesfile$datapath)
                        genesID <- factor(genesID[,1])
                        }else{
                                genesID <- read.table("data/genes.txt")
                                genesID <- factor(genesID[,1])
                                }
                return(genesID)
                })
        
        universe <- reactive({
                
                universefile <- input$universefile
                
                if(!is.null(universefile)){
                        universe <- read.table(universefile$datapath)
                        universe <- factor(universe[,1])
                        }else{
                                universe <- read.table("data/universe.txt")
                                universe <- factor(universe[,1])
                                }
                return(universe)
                })
        
        #Anàlisis d'enriquiment GO amb 'GOstats'
        genEnrichGO <- eventReactive(input$submit, {
                
                if(input$GO_en){
                        pval <- as.numeric(input$pval)
                        GOparams = new("GOHyperGParams",
                                       geneIds=genesID(), universeGeneIds=universe(),
                                       annotation="org.Hs.eg.db", ontology="BP",
                                       pvalueCutoff=pval, conditional=FALSE,
                                       testDirection="over")
                        hyperGTest(GOparams)
                        }else{NULL}
                })
        
        rawHTMLGO <- eventReactive(input$submit, {
                if(input$GO_en){
                        htmlReport(genEnrichGO(), file='results/tempGO.html', 
                                   summary.args=list("htmlLinks"=TRUE))
                        paste(readLines("results/tempGO.html"), collapse="\n")
                        }else{NULL}
                })
        
        output$tableGO <- renderUI({
                if(!is.null(rawHTMLGO())){
                        HTML(rawHTMLGO())
                        }else{NULL}
                })
        
        output$downloadGOtab <- downloadHandler(
                filename = paste("GOtab_", Sys.Date(), ".csv", sep=''),
                content = function(file){write.csv(summary(genEnrichGO()), file, row.names = FALSE, quote = F)}
                )
        
        #Anàlisis d'enriquiment KEGG
        enrichKEGG <- eventReactive(input$submit, {
                
                if(input$KEGG_en){
                        pval <- as.numeric(input$pval)
                        KEGGparams = new("KEGGHyperGParams",
                                         geneIds=genesID(), universeGeneIds=universe(),
                                         annotation="org.Hs.eg.db",pvalueCutoff=pval, 
                                         testDirection="over")
                        hyperGTest(KEGGparams)
                        }else{NULL}
                })
        
        rawHTMLKEGG <- eventReactive(input$submit, {
                
                if(input$KEGG_en){
                        htmlReport(enrichKEGG(), file='results/tempKEGG.html', 
                                   summary.args=list("htmlLinks"=TRUE))
                        paste(readLines("results/tempKEGG.html"), collapse="\n")
                        }else{NULL}
                })
        
        output$tableKEGG <- renderUI({
                if(!is.null(rawHTMLKEGG())){
                        HTML(rawHTMLKEGG())
                        }else{NULL}
                })
        
        output$downloadKEGGtab <- downloadHandler(
                filename = paste("KEGGtab_", Sys.Date(), ".csv", sep=''),
                content = function(file){write.csv(summary(enrichKEGG()), file, 
                                                   row.names = FALSE, quote = F)}
                )
        
        #Anàlisis d'enriquiment GO amb 'topGO'
        myGOData <- eventReactive(input$submit, {
                if(input$topGO_en){
                        
                        require(topGO)
                        universe <- universe()
                        genesID <- genesID()
                        geneList <- factor(as.integer(universe %in% genesID))
                        names(geneList) <- universe
                        
                        new("topGOdata",
                            ontology = "BP",
                            allGenes = geneList,
                            nodeSize = 10,
                            annot=annFUN.org, 
                            mapping="org.Hs.eg.db")
                        }else{NULL}
                })
        
        enrichFisher <- reactive({
                if(!is.null(myGOData())){
                        runTest(myGOData(), algorithm= "classic", statistic="fisher")
                        }else{NULL}
                })
        
        enrichKS <- reactive({
                if(!is.null(myGOData())){
                        runTest(myGOData(), algorithm= "classic", statistic="ks")
                        }else{NULL}
                })
        
        enrich_table <- reactive({
                if(!is.null(myGOData())){
                        pval <- as.numeric(input$pval)
                        GenTable(myGOData(), classicFisher = enrichFisher(), classicKS = enrichKS(),
                                 orderBy = "classicKS", ranksOf = "classicFisher", topNodes = 10)
                        }else{NULL}
                })
        
        output$tabletopGO <- renderTable({
                if(!is.null(myGOData())){
                        enrich_table()
                        }else{NULL}
                })
        
        output$downloadtopGOtab <- downloadHandler(
                filename = paste("topGOtable_", Sys.Date(), ".csv", sep=''),
                content = function(file){write.csv(enrich_table(), file, row.names = FALSE, quote = F)
                        })
        
        topGOplotF <- reactive({
                if(!is.null(myGOData())){
                        showSigOfNodes(myGOData(), score(enrichFisher()), firstSigNodes=5, useInfo="all")
                        pdf("results/temptopGOplotF.pdf")
                        showSigOfNodes(myGOData(), score(enrichFisher()), firstSigNodes=5, useInfo="all")
                        dev.off()
                        }else{NULL}
                })
        
        output$topGOplotF <- renderPlot({
                topGOplotF()
                })
        
        output$downloadtopGOplotF <- downloadHandler(
                filename = paste("topGOplotF_", Sys.Date(), ".pdf", sep=''),
                content = function(file){
                        file.copy("results/temptopGOplotF.pdf", file)
                        })
        
        topGOplotKS <- reactive({
                if(!is.null(myGOData())){
                        showSigOfNodes(myGOData(), score(enrichKS()), firstSigNodes=5, useInfo="all")
                        pdf("results/temptopGOplotKS.pdf")
                        showSigOfNodes(myGOData(), score(enrichKS()), firstSigNodes=5, useInfo="all")
                        dev.off()
                        }else{NULL}
                })
        output$topGOplotKS <- renderPlot({
                topGOplotKS()
                })
        
        output$downloadtopGOplotKS <- downloadHandler(
                filename = paste("topGOplotKS_", Sys.Date(), ".pdf", sep=''),
                content = function(file){
                        file.copy("results/temptopGOplotKS.pdf", file)
                        })
        
        #Eliminem els arxius generats per evitar visualitzar resultats d'anàlisis anteriors
        eventReactive(input$submit, {
                  if (file.exists("results/tempGO.html")) file.remove("results/tempGO.html")
                  if (file.exists("results/tempKEGG.html")) file.remove("results/tempKEGG.html")
                  if (file.exists("results/temptopGOplotKS.pdf")) file.remove("results/temptopGOplotKS.pdf")
                  if (file.exists("results/temptopGOplotF.pdf")) file.remove("results/temptopGOplotF.pdf")
                })
        
        session$onSessionEnded(function() {
          if (file.exists("results/tempGO.html")) file.remove("results/tempGO.html")
          if (file.exists("results/tempKEGG.html")) file.remove("results/tempKEGG.html")
          if (file.exists("results/temptopGOplotKS.pdf")) file.remove("results/temptopGOplotKS.pdf")
          if (file.exists("results/temptopGOplotF.pdf")) file.remove("results/temptopGOplotF.pdf")
        })
}

shinyApp(ui, server)