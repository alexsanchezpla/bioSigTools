# Run this script when you first install the application

installifnot <- function(pckgName){
  if (!(require(pckgName, character.only = TRUE))) {
    install.packages(pckgName)
  }else{
    print(paste("Package", pckgName, "already installed", sep = " "))
  } 
}
installBiocifnot <- function(pckgName){
  if (!(require(pckgName, character.only = TRUE))) {
    source("https://bioconductor.org/biocLite.R")
    biocLite(eval(pckgName), suppressUpdates = TRUE)
  }else{
    print(paste("Package", pckgName, "already installed", sep = " "))
  } 
}

installifnot("shiny")
installifnot("shinydashboard")
installifnot("shinyjs")
installifnot("memoise")

installBiocifnot("Rgraphviz")

installBiocifnot("AnnotationDbi")
installBiocifnot("annotate")

installBiocifnot("hgu133a.db")
installBiocifnot("org.Hs.eg.db")
installBiocifnot("org.Mm.eg.db")
installBiocifnot("KEGG.db")
installBiocifnot("reactome.db")
installBiocifnot("GO.db")

installBiocifnot("GOstats")
installBiocifnot("topGO")
installBiocifnot("goProfiles")
installifnot("GSA")
