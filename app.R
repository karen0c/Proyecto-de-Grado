library(readxl)
library(xts)
library(shiny)
library(tsDyn)



Date<-seq(as.Date("2202-04-04"),by="week", len=30)
# Define UI for application 
ui <- fluidPage(
  
  # Título aplicación
  headerPanel(HTML("<b><center>Herramienta de apoyo a la decisión</b></center></br>")),
  
  #Se crea conjunto de pestañas
  tabsetPanel(type = "tabs",
              
    #Primera pestaña     
    tabPanel("Predicción del precio del cacao",#Tab title
        
        sidebarLayout(
          sidebarPanel(
            
            #Ingresar los datos históricos
            fileInput(inputId = "archivoentrada", 
                      label = "Ingrese los datos históricos del precio ($/kg) nacional e internacional del cacao",
                      accept =c(".xlsx")),
            p("Nota: Se aceptan archivos formato .xlsx, en la primera columna debe ir la fecha, luego el precio nacional y en la tercera columna el precio internacional"),
            
            p("."),
            
            dateInput('fecha', 'Selecciona la fecha', value = "2022-04-04"),
            p("Nota: Solo se pueden seleccionar los días lunes de cada semana")
            
          ),
          
          mainPanel(
            
            
            h4("El precio para las proximas 30 semanas"),
            plotOutput("plotPrecio"),
            h5("El precio nacional del cacao ($/kg) para la fecha seleccionada es:" ),
            textOutput("Precio")
            
          )
          
        )),
    
    #Segunda pestaña
    tabPanel("Flujo de caja",
              sidebarLayout(
                sidebarPanel(
                  
                  #Ingresar los datos predictivos de producción
                  fileInput(inputId = "fcstProducción", 
                            label = "Ingrese la predicción sobre la producción de cacao (kg)",
                            accept =c(".xlsx")),
                  p("Nota: Se aceptan archivos formato .xlsx, debe ir primero la fecha seguido por el valor de producción"),
                  
                  dateInput('fechaEgreso', 'Selecciona la fecha', value = "2022-04-04"),
                  p("Nota: Solo se pueden seleccionar los días lunes de cada semana"),
                  
                  #Ingresar egresos
                  numericInput(inputId = "Egresos",
                            label = "Ingrese el total de sus egresos ($) para la fecha seleccionada",value = 730300000)
                  ),
                
                mainPanel(
                     h5("El total de ingresos para las siguientes semanas:" ),
                     plotOutput("plotIngresos"),
                     h5("Beneficio (Ingreso-Egreso) total del periodo(M$) seleccionado es :"),
                     textOutput("Beneficio"),
                     
                   )
                 )
    )
    )
)


server <- function(input, output) {
  
     output$plotPrecio<- renderPlot({
        
            #funcion reactive para el archivo de los precios historicos
            datos<-reactive ({
              archivo<-input$archivoentrada
              if (is.null(archivo)) {return(NULL)}
              data<-read_xlsx(archivo$datapath,sheet = 1)
            })
            
            #se asigna una variable al archivo que se importó
            Data<-datos()
            
            #Se obtiene la fecha del archivo
            fecha<-Data[,1]
            fecha<-as.matrix(fecha)
            
            #Se obtiene el modelo vecm con las columnas de precio internacional y nacional
            vecm1 = VECM(Data[,2:3], lag=2, r=1, estim = ("ML"))
            
            #se guarda la predicción del precio nacional
            fcstPrecio<-predict(vecm1,n.ahead = 30)[,1]
            
            #se captura la última fecha ingresada
            ultimoDia<-as.Date(tail(fecha, 1))
            
            
            # se crea secuencia de fechas semanales para los valores predictivos
            Date<-as.Date(seq(ultimoDia,by="week", len=31))[-1]
            
            #Gráfico de valores predictivos sobre el precio del cacao
            plotPrecio<-plot(x=Date, y=fcstPrecio,type=c("l"), col = c("orange"),ylab="Precio Nacional del Cacao",xlab="Tiempo" )
      })
      
      #Texto que indica el precio de la fecha que seleccionó el usuario
      output$Precio<-renderText({
        
            #funcion reactive para el archivo de los precios historicos
            datos<-reactive ({
              archivo<-input$archivoentrada
              if (is.null(archivo)) {return(NULL)}
              data<-read_xlsx(archivo$datapath,sheet = 1)
            })
            
            #se asigna una variable al archivo que se importó
            Data<-datos()
            
            #Se obtiene la fecha del archivo
            fecha<-Data[,1]
            fecha<-as.matrix(fecha)
            
            #Se obtiene el modelo vecm con las columnas de precio internacional y nacional
            vecm1 = VECM(Data[,2:3], lag=2, r=1, estim = ("ML"))
            
            #se guarda la predicción del precio nacional
            fcstPrecio<-predict(vecm1,n.ahead = 30)[,1]
            
            #se captura la última fecha ingresada
            ultimoDia<-as.Date(tail(fecha, 1))
            
            # se crea secuencia de fechas semanales para los valores predictivos
            Date<-as.Date(seq(ultimoDia,by="week", len=31))[-1]
            
            
            #se contruye yn xts con los datos futuros del precio del cacao
            base<-as.xts(fcstPrecio, order.by = Date)
            
            Precio<-base[input$fecha]
      })
      
      
      output$plotIngresos<-renderPlot({
        
            #funcion reactive para el archivo de la predicción sobre la producción de cacao
            forecastP<-reactive ({
              archivo<-input$fcstProducción
              if (is.null(archivo)) {return(NULL)}
              data<-read_xlsx(archivo$datapath,sheet = 1)
            })
            
            #funcion reactive para el archivo de los precios historicos
            datos<-reactive ({
              archivo<-input$archivoentrada
              if (is.null(archivo)) {return(NULL)}
              data<-read_xlsx(archivo$datapath,sheet = 1)
            })
            
            #se asigna una variable al archivo que se importó
            Data<-datos()
            
            #Se obtiene la fecha del archivo
            fecha<-Data[,1]
            fecha<-as.matrix(fecha)
            
            #Se obtiene el modelo vecm con las columnas de precio internacional y nacional
            vecm1 = VECM(Data[,2:3], lag=2, r=1, estim = ("ML"))
            
            #se guarda la predicción del precio nacional
            fcstPrecio<-predict(vecm1,n.ahead = 30)[,1]
            
            #se captura la última fecha ingresada
            ultimoDia<-as.Date(tail(fecha, 1))
            
            # se crea secuencia de fechas semanales para los valores predictivos
            Date<-seq(ultimoDia,by="week", len=31)[-1]
            
           
            #Se calcula el ingreso ($M) como lo que se va a producir por el precio
            Ingreso<-fcstPrecio*forecastP()/1000000
            #Ingreso<-fcstPrecio*Produccionn/1000000
           
            #Gráfico de ingresos
            plotIngresos<-plot(cbind(Date, Ingreso), type='h', ylab="Ingreso (M$)", xlab="Fecha", lwd = 6)
            
      })
      
      output$Beneficio<-renderText({
            #funcion reactive para el archivo de la predicción sobre la producción de cacao
            forecastP<-reactive ({
              archivo<-input$fcstProducción
              if (is.null(archivo)) {return(NULL)}
              data<-read_xlsx(archivo$datapath,sheet = 1)
            })
            
            #funcion reactive para el archivo de los precios historicos
            datos<-reactive ({
              archivo<-input$archivoentrada
              if (is.null(archivo)) {return(NULL)}
              data<-read_xlsx(archivo$datapath,sheet = 1)
            })
            
            #se asigna una variable al archivo que se importó
            Data<-datos()
            
            #Se obtiene la fecha del archivo
            fecha<-Data[,1]
            fecha<-as.matrix(fecha)
            
            #Se obtiene el modelo vecm con las columnas de precio internacional y nacional
            vecm1 = VECM(Data[,2:3], lag=2, r=1, estim = ("ML"))
            
            #se guarda la predicción del precio nacional
            fcstPrecio<-predict(vecm1,n.ahead = 30)[,1]
            
            #se captura la última fecha ingresada
            ultimoDia<-as.Date(tail(fecha, 1))
            
            # se crea secuencia de fechas semanales para los valores predictivos
            Date<-seq(ultimoDia,by="week", len=31)[-1]
          
            #se calcula el ingreso como lo producido por el precio 
            ValorIn<-as.xts(forecastP()*fcstPrecio, order.by = Date)
             
            #se obtiene el beneficio (M$)(ingreso - egreso) del periodo seleccionado
            Beneficio<-(ValorIn[input$fechaEgreso]-input$Egresos)/1000000
      })
      
      
}
# Run the application 
shinyApp(ui = ui, server = server)


