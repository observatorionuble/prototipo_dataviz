


# Load R packages
library(shiny)
library(shinythemes)
library(shinyjs)
#Paquetes de gráficos
library(ggplot2)
library(plotly)
library(dplyr)
library(shinycssloaders)
library(DT)
#para BDD
library(pool)
library(DBI)
library(dbplyr)
#
library(lubridate)
library(shinyFeedback)
library(polished)
#auth
library(shinymanager)
#library(keyring)

#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
ns <- NS("tasas_table")

# Turn off scientific notation
options(scipen = 999)

# Set spinner type (for loading)
options(spinner.type = 8)


pool <- dbPool(
  drv = RPostgres::Postgres(),
  host = "pacheco.chillan.ubiobio.cl",
  port = 5432,
  dbname = "appweb",
  user = "postgres",
  password = "olnbdd"
)


#####################################
#LENGUAJE ESPAÑOL PARA AUTENTICACION#
#####################################


set_labels(
  language = "en",
  "Please authenticate" = "Por favor autenticar",
  "Username:" = "Usuario:",
  "Password:" = "Contraseña:",
  "Login" = "Iniciar sesión",
  "Logout" ="Cerrar sesión",
  "Username or password are incorrect" = "Nombre de usuario o contraseña son incorrectos",
  "Your account has expired" = "Su cuenta ha expirado",
  "Please change your password" = "Por favor cambie su contraseña",
  "New password:" = "Nueva contraseña :",
  "Confirm password:" = "Confirmar contraseña :",
  "Update new password" = "Actualizar nueva contraseña",
  "Password successfully updated! Please re-login" = "La contraseña se ha actualizado correctamente! Por favor vuelva a iniciar sesión.",
  "The two passwords are different" = "Las dos contraseñas son distintas",
  "Failed to update password" = "No se pudo actualizar contraseña",
  "Go to application" = "Volver al sitio principal",
  "Administrator mode" = "Administrar usuarios",
  "Add a user" = "Agregar un usuario",
  "Too many users" = "Demasiados usuarios",
  "Maximum number of users : %s" = "Numero máximo de usuarios : %s",
  "Failed to update user" = "No se pudo actualizar la usuario",
  "User successfully updated" = "El usuario se ha actualizado correctamente",
  "Cancel" = "Cancelar",
  "Confirm new user" = "Confirmar nuevo usuario",
  "Confirm change" = "Confirmar cambio",
  "Are you sure to remove user(s): %s from the database ?" = "Estás seguro de eliminar el(los) usuario(s): %s de la base de datos ?",
  "Delete user(s)" = "Eliminar usuario(s)",
  "Delete user" = "Eliminar usuario",
  "Edit user" = "Modificar usuario",
  "User already exist!" = "Usuario ya existe!",
  "Dismiss" = "Descartar",
  "New user %s succesfully created!" = "Nuevo usuario %s creado exitosamente!",
  "Ask to change password" = "Solicitar cambio de contraseña",
  "Confirm" = "Confirmar",
  "Ask %s to change password on next connection?" = "Solicitar a %s que cambie la contraseña en la próxima conexión?",
  "Change saved!" = "Cambio guardado!",
  "Failed to update the database" = "No se pudo actualizar la base de datos",
  "Password does not respect safety requirements" = "La contraseña no respeta los requisitos de seguridad.",
  "Password must contain at least one number, one lowercase, one uppercase and must be at least length 6." = "La contraseña debe contener al menos un número, una minúscula, una mayúscula y debe tener al menos una longitud de 6 carácteres.",
  "Number of connections per user" = "Número de conexiones por usuario",
  "Number of connections per day" = "Número de conexiones por día",
  "Total number of connection" = "Número total de conexión",
  "You can\'t remove yourself!" = "¡No te puedes eliminar a ti mismo!",
  "User:" = "Usuario :",
  "Period:" = "Período :",
  "Last week" = "La semana pasada",
  "Last month" = "El mes pasado",
  "All period" = "Todo el período",
  "Home" = "Casa",
  "Select all shown users" = "Seleccionar todos los usuarios mostrados",
  "Remove selected users" = "Eliminar usuarios seleccionados",
  "Edit selected users" = "Editar usuarios seleccionados",
  "Force selected users to change password" = "Forzar a los usuarios seleccionados a cambiar la contraseña",
  "Users" = "Usuarios",
  "Passwords" = "Contraseñas",
  "Download logs database" = "Descargar la base de datos de registros",
  "Download SQL database" = "Descargar la base de datos SQL",
  "Reset password for %s?" = "Restablecer contraseña para %s?",
  "Reset password" = "Restablecer la contraseña",
  "Temporary password:" = "Contraseña temporal:",
  "Password succesfully reset!" = "¡Contraseña restablecida con éxito!",
  "You are not authorized for this application" = "No estás autorizado para esta aplicación",
  "Language"  = "Lenguaje", 
  "Yes" = "Sí",
  "No" = "No",
  "Password" = "Contraseña",
  "start" = "inicia",
  "expire" = "expira",
  "admin" = "Administrador",
  "user" = "Usuario",
  "Edit" = "Editar",
  "Remove" = "Eliminar",
  "must_change" = "Debe cambiarse",
  "have_changed" = "Se ha cambiado",
  "date_change" = "Fecha",
  "Change password" = "Cambiar la contraseña",
  "Select" = "Seleccionar",
  "Logs" = "Registros",
  "All users" = "Todos los usuarios",
  "Nb logged" = "Nb registrado",
  "Allowed null values" = "Valores nulos permitidos"
)


# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
                shinyFeedback::useShinyFeedback(),
                shinyjs::useShinyjs(),
                navbarPage(
                  "Prototipo Observatorio Laboral Ñuble", id="banner",
                  #tabsetPanel(id = "tabs",
                  tabPanel("Gráficos",
                           sidebarPanel( width=3,
                                         tags$h3("Indicadores"),
                                         #htmlOutput sets a placeholder for an object that will come from database
                                         htmlOutput("lista_indicadores"),
                                         actionButton(
                                           "actualizar_lista","",
                                           class = "btn-info",
                                           style = "color: #fff;",
                                           icon = icon('sync'),
                                           width = '30%'
                                         )
                                         ,
                                         tags$h3("Filtros"),
                                         selectInput("general", label = "Alcance:",
                                                     choices = list("Nacional" = "Nacional",
                                                                    "Arica y Parinacota" = "Arica y Parinacota",
                                                                    "Tarapacá" = "Tarapacá",
                                                                    "Antofagasta" = "Antofagasta",
                                                                    "Atacama" = "Atacama",
                                                                    "Coquimbo" = "Coquimbo",
                                                                    "Valparaíso" = "Valparaíso",
                                                                    "Metropolitana de Santiago" = "Metropolitana de Santiago",
                                                                    "O'Higgins" = "O'Higgins",
                                                                    "Maule" = "Maule",
                                                                    "Ñuble" = "Ñuble",
                                                                    "Biobío" = "Biobío",
                                                                    "La Araucanía" = "La Araucanía",
                                                                    "Los Ríos" = "Los Ríos",
                                                                    "Los Lagos" = "Los Lagos",
                                                                    "Aysén" = "Aysén",
                                                                    "Magallanes" = "Magallanes"
                                                     )),
                                         selectInput("sexo", label = "Sexo:",
                                                     choices = list("Ambos" = "ALL",
                                                                    "Femenino" = 2,
                                                                    "Masculino" = 1
                                                     )),
                                         dateRangeInput('periodo',
                                                        label = "Período: ",
                                                        format = "mm/yyyy",
                                                        language="es",
                                                        start = as.Date("2010-02-01"),
                                                        end=Sys.Date(),
                                                        min= as.Date("2010-02-01"),
                                                        max=Sys.Date(),
                                                        startview = "year",
                                                        separator = "-")
                                         
                                         #Generar el gráfico a través de un botón
                                         ,actionButton("submit_grafico","Generar gráfico", class = "btn btn-primary")
                                         
                           ), # sidebarPanel
                           
                           #--PANEL DONDE SE MUESTRA EL GRÁFICO------------------------------------------
                           mainPanel(
                             htmlOutput("titulo_grafico"),
                             textOutput("ind_selec"),
                             tableOutput("row"),
                             textOutput("un_dato"),
                             plotlyOutput("grafico")
                             
                             
                             
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel(title="Manejar BDD",
                           h1("Lista de tasas"),
                           tagList(
                             fluidRow(
                               column(
                                 width = 2,
                                 actionButton(
                                   "add","Agregar",
                                   class = "btn-success",
                                   style = "color: #fff;",
                                   icon = icon('plus'),
                                   width = '100%'
                                 ),
                                 tags$br(),
                                 tags$br()
                               )
                             ),
                             fluidRow(
                               column(
                                 width = 12,
                                 title = "Lista de Indicadores",
                                 DTOutput('tabla') %>%
                                   withSpinner(),
                                 tags$br(),
                                 tags$br()
                               )
                             ),
                             #codigojavascript de editar y modificar
                             tags$script(src = "modulo.js"),
                             tags$script(paste0("modulo_js()")),
                             
                           )
                           
                  )#Fin tabPanel MANEJAR BDD
                  #)              
                ) # navbarPage
                #,verbatimTextOutput("auth_output")
                ,htmlOutput("usuario_actual"),
                
                
) # fluidPage

ui <- secure_app(ui,
                 enable_admin =TRUE,
                 theme = shinytheme("flatly"),
                 tags_top = tags$img(src = "https://pbs.twimg.com/profile_images/1232755966745202689/saEuQwpC_400x400.jpg", width = 100),
                 # Información extra
                 tags_bottom = tags$div(
                   tags$p(
                     "Para cualquier duda contacte con el ",
                     tags$a(
                       href = "mailto:antonia.navarrete1601@alumnos.ubiobio.cl?Subject=Shiny%20aManager",
                       target="_top", "administrador"
                     )
                   )
                 )
)


################################################
# ---------SERVER-------------------------------
################################################

server <- function(input, output, session) {
  
  shinyjs::disable("submit_grafico")
  
  ###############
  #AUTENTICACIÓN#
  ###############
  
  
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "authuser.sqlite",
      passphrase = ""
      #passphrase = "passphrase_without_keyring"
    ),
    timeout=15
  )
  
  #output$auth_output <- renderPrint({
  #  reactiveValuesToList(res_auth)
  #})
  
  output$usuario_actual <- renderUI({
    tags$script(HTML(paste0("var header = $('.navbar> .container-fluid');
                       header.append('<div style=\"float:right;color:white;\"><b>Usuario actual: </b>",res_auth$user,"</div>');
                       console.log(header)")))
  })
  
  
  
  
  ########################################
  #CARGAR INDICADORES EN EL PRIMER FILTRO#
  ########################################
  
  loadData <- function(fields, table,where=''){
    if(where=='')
      query <- sprintf("SELECT %s FROM %s",fields,table)
    else{
      query <- sprintf("SELECT %s FROM %s WHERE %s",fields,table,where)
    }
    
    dataDB <- dbGetQuery(pool,query)
    #if(sortBy!='') dataDB[order(dataDB[sortBy]),]
    #else
    dataDB
  }
  
  loadDropdown <-function(fields,table){
    
    lista <-loadData(fields,table)
    
    output$lista_indicadores <- renderUI({
      selectInput("lista_indicadores", label= "Indique la tasa a graficar:", lista)
    })
    shinyjs::enable("submit_grafico")
  }
  
  loadDropdown("titulo","indicador")
  
  observeEvent(input$actualizar_lista, {
    shinyjs::disable("actualizar_lista")
    
    loadDropdown("titulo","indicador")
    
    shinyjs::enable("actualizar_lista")
  })
  
  
  output$titulo_grafico <-renderUI({
    h1("")
  })
  
  
  #Función para que automáticamente se seleccione el dia primero del mes
  monthStart <- function(x) {
    x <- as.POSIXlt(x)
    x$mday <- 1
    as.Date(x)
  }
  
  sliderMonth <- reactiveValues()
  observe({
    if(!is.null(input$periodo)){
      full.date <- as.POSIXct(input$periodo, tz="GMT")
      sliderMonth$Month <- as.character(monthStart(full.date))
    }
  })
  
  
  #Crea una tabla de una fila con el titulo seleccionado en el input
  datos_indicador<- reactive({
    req(input$lista_indicadores)
    title <- input$lista_indicadores
    if(length(title)!=0){
      pool %>% tbl("indicador") %>% filter(titulo==title)
    }
    
    
  })
  
  
  
  #La fila la transforma a un arreglo
  #------------------------------------------------------------
  #Numeros a cambiar en el pull
  #1= id
  #2= titulo
  #3= url
  #4= columna_asociada
  url_seleccionado <- reactive({
    req(datos_indicador())
    if(length(datos_indicador())!=0)
      datos_indicador() %>% pull(3)
  })
  
  tabla_indicador_seleccionado <- reactive({
    req(url_seleccionado())
    enlace <- paste0(url_seleccionado())
    if(length(enlace)!=0)
      tryCatch({
        read.csv(enlace)
      },
      error = function(e){
        cat("URL Inválido. Verificar que no tenga espacios.")
      })
    
    #try(read.csv(paste0(url_seleccionado())))
    
  })
  
  columna_asociada_seleccionado <- reactive({
    req(datos_indicador())
    if(length(datos_indicador)!=0)
      datos_indicador() %>% pull(4)
  })
  
  ###############################
  #Filtro por: período y alcance#
  ###############################
  nuevatabla <- reactive({
    if(length(tabla_indicador_seleccionado())!=0){
      
      if("alcance" %in% colnames(tabla_indicador_seleccionado()) && "trimestre" %in% colnames(tabla_indicador_seleccionado())  ){
        tryCatch({
          filter(tabla_indicador_seleccionado(), alcance==input$general & as.Date(trimestre)>=sliderMonth$Month[1] & as.Date(trimestre)<=sliderMonth$Month[2])
          
        },error=function(e){
          showToast("error", "Ha ocurrido un error al filtrar...")
        })
      }else{
        showToast("error", "La columna 'trimestre' o 'alcance' no existen.")
      }
      
      
    }#end if
  })   
  
  #####################################
  #Filtro por: período, sexo y alcance#
  #####################################
  nuevatabla_completa <- reactive({
    sx<-input$sexo
    
    if(length(tabla_indicador_seleccionado())!=0){
      
      if("alcance" %in% colnames(tabla_indicador_seleccionado()) && "trimestre" %in% colnames(tabla_indicador_seleccionado()) && "sexo" %in% colnames(tabla_indicador_seleccionado()) ){
        tryCatch({
          if(sx=="ALL"){
            filter(tabla_indicador_seleccionado(), alcance==input$general & as.Date(trimestre)>=sliderMonth$Month[1] & as.Date(trimestre)<=sliderMonth$Month[2])
          }else{
            filter(tabla_indicador_seleccionado(), alcance==input$general & sexo==input$sexo & as.Date(trimestre)>=sliderMonth$Month[1] & as.Date(trimestre)<=sliderMonth$Month[2])
          }
        },error=function(e){
          showToast("error", "Ha ocurrido un error al filtrar...")
        })
      }else if("alcance" %in% colnames(tabla_indicador_seleccionado()) && "trimestre" %in% colnames(tabla_indicador_seleccionado())){
        filter(tabla_indicador_seleccionado(), alcance==input$general & as.Date(trimestre)>=sliderMonth$Month[1] & as.Date(trimestre)<=sliderMonth$Month[2])
      }
      else{
        showToast("error", "La columna 'trimestre' o 'alcance' no existen.")
      }
      
      
    }#end if
  })   
  
  
  
  ##########################
  #GENERA GRÁFICO EN SUBMIT#
  ##########################
  
  #Nota para después: usar isolate(input$xxxxx) hace que sólo se actualice al hacer click en el botón.
  observeEvent(input$submit_grafico, {
    
    shinyjs::disable("submit_grafico")
    
    
    #Verificar que la tabla no sea vacía
    if(length(nuevatabla_completa())!=0){
      
      
      output$titulo_grafico <-renderUI({
        h1(isolate(input$lista_indicadores))
      })
      
      if (input$sexo=="ALL" && "sexo" %in% colnames(nuevatabla_completa())){
        colsexo <- c("1" = "#0068b7", "2" = "#ff3641")
        
        graph <-ggplot(nuevatabla_completa(), aes(x= as.Date(trimestre, format = "%Y-%m-%d"), shape= as.factor(sexo), colour = as.factor(sexo))) + aes_string(y=columna_asociada_seleccionado()) +
          scale_shape_manual(values=c("1","2"), breaks = c("1","2"), labels=c("Masculino", "Femenino"))+
          scale_colour_manual("Sexo", values=c("#0068b7","#ff3641"),  breaks = c("1","2"), labels=c("Masculino", "Femenino"))+
          geom_line(size = 1.5) +
          xlab("Año") +
          ylab(
            paste0(toupper(substr(columna_asociada_seleccionado(), 1, 1)), substr(columna_asociada_seleccionado(), 2, nchar(columna_asociada_seleccionado()))," (%)")
          )
      }else{
        graph <-ggplot(nuevatabla_completa(), aes(x= as.Date(trimestre, format = "%Y-%m-%d"), colour = alcance)) + aes_string(y=columna_asociada_seleccionado()) +
          geom_line(size = 1.5) +
          xlab("Año") +
          ylab(
            paste0(toupper(substr(columna_asociada_seleccionado(), 1, 1)), substr(columna_asociada_seleccionado(), 2, nchar(columna_asociada_seleccionado()))," (%)")
          )
      }
      
      
      
      #----------------visualizar grafico con datos filtrados-----------------
      tryCatch({
        x<- ggplotly(graph)
        output$grafico <- renderPlotly(x)
      },error= function(e){
        showToast("error","Uno o más de los filtros ingresados no están registrados en el indicador seleccionado.")
      })
      
      
      
    }#end if
    
    shinyjs::enable("submit_grafico")    
    
  })
  
  #############################################################################
  #--------------------------------COSAS CRUD---------------------------------#
  #############################################################################
  
  
  ###############
  #MOSTRAR TODOS#
  ###############
  
  session$userData$indicador_trigger <- reactiveVal(0)
  
  indicadores <- reactive({
    session$userData$indicador_trigger()
    
    out <- NULL
    tryCatch({
      out <- pool %>%
        tbl('indicador') %>%
        collect()
    }, error = function(err) {
      
      
      msg <- "Database Connection Error"
      # print `msg` so that we can find it in the logs
      print(msg)
      # print the actual error to log it
      print(error)
      # show error `msg` to user.  User can then tell us about error and we can
      # quickly identify where it cam from based on the value in `msg`
      showToast("error", msg)
    })
    
    out
  })
  
  prep <- reactiveVal(NULL)
  
  observeEvent(indicadores(), {
    out <- indicadores()
    
    ids <- out$id
    
    actions <- purrr::map_chr(ids, function(id_) {
      paste0(
        '<div class="btn-group" style="width: 75px;" role="group" aria-label="Basic example">
          <button class="btn btn-warning btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
        </div>'
      )
    })
    
    # Remove the `id` column. We don't want to show this column to the user
    out <- out %>%
      select(-id)
    
    # Set the Action Buttons row to the first column of the `mtcars` table
    out <- cbind(
      tibble(" " = actions),
      out
    )
    
    prep(out)
    
  })
  
  output$tabla <- renderDT({
    req(prep())
    out <- prep()
    
    datatable(
      out,
      rownames = FALSE,
      colnames = c('Título', 'URL', 'Columna Asociada'),
      selection = "none",
      class = "compact stripe row-border nowrap",
      # Escape the HTML in all except 1st column (which has the buttons)
      escape = -1,
      options = list(
        scrollX = TRUE,
        dom = 'Bftip',
        columnDefs = list(
          list(targets = 0, orderable = FALSE)
        ),
        drawCallback = JS("function(settings) {
          // removes any lingering tooltips
          $('.tooltip').remove()
        }")
      )
    )
    
  })
  
  
  
  callModule(
    modulo_editar,
    "add",
    modal_title = "Agregar Indicador",
    pool = dbPool(
      drv = RPostgres::Postgres(),
      host = "pacheco.chillan.ubiobio.cl",
      port = 5432,
      dbname = "appweb",
      user = "postgres",
      password = "olnbdd"
    ),
    indicador_to_edit = function() NULL,
    modal_trigger = reactive({input$add})
  )
  
  indicador_to_edit <- eventReactive(input$id_to_edit, {
    
    indicadores() %>%
      filter(id == input$id_to_edit)
  })
  
  callModule(
    modulo_editar,
    "edit",
    modal_title = "Editar Indicador",
    pool = dbPool(
      drv = RPostgres::Postgres(),
      host = "pacheco.chillan.ubiobio.cl",
      port = 5432,
      dbname = "appweb",
      user = "postgres",
      password = "olnbdd"
    ),
    indicador_to_edit = indicador_to_edit,
    modal_trigger = reactive({input$id_to_edit})
  )
  
  indicador_to_delete <- eventReactive(input$id_to_delete, {
    
    out <- indicadores() %>%
      filter(id == input$id_to_delete) %>%
      as.list()
  })
  
  callModule(
    modulo_eliminar,
    "delete",
    modal_title = "Borrar Indicador",
    pool = dbPool(
      drv = RPostgres::Postgres(),
      host = "pacheco.chillan.ubiobio.cl",
      port = 5432,
      dbname = "appweb",
      user = "postgres",
      password = "olnbdd"
    ),
    indicador_to_delete = indicador_to_delete,
    modal_trigger = reactive({input$id_to_delete})
  )
  
  onStop(function() cat("Session stopped\n"))
  
  
} # server



# Create Shiny object
shinyApp(ui = ui, server = server,
         onStart = function() {
           cat("Doing application setup\n")
           
           onStop(function() {
             poolClose(pool)
             cat("Doing application cleanup\n")
           })
         }
)