##################################
##################################
###  Alberto  Gonzalez Delgado ###
###          Enero 2022        ###
##################################
##################################
#
# Esto es una aplicacion web de shiny. La aplicacion se puede cargar pulsando el boton
# 'Run App' que se encuentra arriba.
# Esta aplicacion espera como entrada dos listas de genes y da como resultado un plot que representa la interseccion entre ambas
# listas de genes. 

#Se instalan y cargan los paquetes necesarios:i
#Shiny es un paquete de R que facilita la creación de aplicaciones web interactivas directamente desde R. 
# https://shiny.rstudio.com/
#install.packages("shiny")
library(shiny)

#VennDiagram es un paquete que presenta un conjunto de funciones para generar diagramas de Venn y Euler de alta resolucion.
#https://cran.r-project.org/web/packages/VennDiagram/index.html
#install.packages("VennDiagram")
library(VennDiagram)

#Shinythemes permite cargar temas para personalizar la aplicacion web 
#https://rstudio.github.io/shinythemes/
#install.packages("shinythemes")
library(shinythemes)

#Se define la UI de la aplicacion
ui <- fluidPage(
    titlePanel("Vennyto"), #Nombre de la aplicacion
    theme=shinytheme("darkly"),  #Tema elegido para la aplicacion
    sidebarLayout(              #Se define un layout con una barra lateral y un area principal
        sidebarPanel(           #En la barra lateral se va a incluir:
            textInput(inputId="name1", #1: Una entrada de texto con Id=name1
                      placeholder = "Insert genes list name", #Texto que aparece cuando el cuadro de texto esta vacio
                      value="Gene list 1", #Se define como valor predeterminado Gene list 1, aunque se puede modificar
                      label="Gene list name:" #Se define como etiqueta de la entrada Gene list 1
                      ),
            textAreaInput("i1", "Gene list 1",  #2: Un cuadro de texto con Id=i1 y nombre Gene list 1
                          value = " ", rows =6,   #sin valor inicial y se define el tamaño
                          placeholder = "Instert the gene list here" # Texto que aparece cuando el cuadro de texto esta vacio
                          ),
            textInput(inputId = "name2", #3: Una entrada de texto con Id=name2
                      placeholder="Insert gene list name", #Texto que aparece cuando el cuadro de texto esta vacio
                      value="Gene list 2", #Se define como valor predeterminado Gene list 2, aunque se puede modificar
                      label="Gene list name:",#Se define como etiqueta de la entrada Gene list 2
                      ),
          textAreaInput("i2", "Gene list 2", #4: Un cuadro de texto con Id=i2 y nombre Gene list 2
                        value = " ", rows = 6, #sin valor inicial y se define el tamaño
                        placeholder = "Insert the gene list here",#Texto que aparece cuando el cuadro de texto esta vacio
                        )
          
        ),
        mainPanel( #En el panel principal se va a incluir:
           plotOutput("VennyDiagram", height = "900px") #un plot con id=VennyDiagram y con un tamaño 900px
        )
    )
    )


#Se define el servidor:
server <- function(input, output){
    #Se definen parametros que se van a utilizar para generar el grafico. 
    #La funcion strlist divide la entrada de texto en subcadenas de texto. Se utiliza el "enter" (\n) como localizador de los distintos
    #elementos que han de separarse como subcadenas de la entrada.
    #La funcion reactive permite que al cambiar las entradas, cambie el resultado de la funcion.
    lista1=reactive(unique(unlist(strsplit(x=input$i1,split="\n"))))
    lista2=reactive(unique(unlist(strsplit(x=input$i2,split="\n"))))
    crossarea=reactive(unique(intersect(lista1(),lista2())))
    output$VennyDiagram <- renderPlot({ #La salida se coloca en el panel principal utilizando la ID introducida en el plotoutput
        draw.pairwise.venn(area1 = length(lista1()), #La salida llama a la funcion de VennDiagram que permite generar Diagramas de Venn de dos listas 
                           area2 = length(lista2()),
                           cross.area = length(crossarea()),
                           category = c(input$name1,
                                        input$name2
                                        ),
                           cat.cex=c(2.5,2.5),
                           fill=c("blue","red"),
                           euler.d=TRUE,
                           cat.pos=c(0,180),
                           scaled=TRUE, #Esta funcion permite que las circunferencias del diagrama de Venn presenten tamaños relativos a sus areas
                           print.mode=c("raw","percent")
                           )
    
    })
}
# Run the application 
shinyApp(ui = ui, server = server)

