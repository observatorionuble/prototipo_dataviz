
modulo_editar <- function(input, output, session, modal_title, pool, indicador_to_edit, modal_trigger) {
  
  ns <- session$ns
  
  observeEvent(modal_trigger(), {
    hold <- indicador_to_edit()
    
    showModal(
      modalDialog(
        fluidRow(
          column(
            width = 6,
            textInput(
              ns("titulo"),
              'Título:',
              value = ifelse(is.null(hold), "", hold$titulo)
            ),
            textInput(
              ns('columna_asociada'),
              'Columna Asociada: ',
              value = ifelse(is.null(hold), "", hold$columna_asociada)
            ),
            textInput(
              ns("url"),
              "Indique el enlace:",
              value = ifelse(is.null(hold), "", hold$url)
            )
          )
        ),
        title = modal_title,
        size = 'm',
        footer = list(
          modalButton('Cancelar'),
          actionButton(
            ns('submit'),
            'Guardar',
            class = "btn btn-primary",
            style = "color: white"
          )
        )
      )
    )
    
    # Verifica los inputs
    # `shinyFeedback`
    observeEvent(input$titulo, {
      if (input$titulo == "") {
        shinyFeedback::showFeedbackDanger(
          "titulo",
          text = "Campo obligatorio."
        )
        shinyjs::disable('submit')
      } else {
        shinyFeedback::hideFeedback("titulo")
        shinyjs::enable('submit')
      }
    })
    
    observeEvent(input$url, {
      if (input$url == "") {
        shinyFeedback::showFeedbackDanger(
          "url",
          text = "Campo obligatorio."
        )
        shinyjs::disable('submit')
      } else {
        shinyFeedback::hideFeedback("url")
        shinyjs::enable('submit')
      }
    })
    
    observeEvent(input$columna_asociada, {
      if (input$columna_asociada == "") {
        shinyFeedback::showFeedbackDanger(
          "columna_asociada",
          text = "Campo obligatorio."
        )
        shinyjs::disable('submit')
      } else {
        shinyFeedback::hideFeedback("columna_asociada")
        shinyjs::enable('submit')
      }
    })
    
  })#end observeEvent
  
  
  
  
  
  edit_ind_dat <- reactive({
    hold <- indicador_to_edit()
    
    out <- list(
      id = if (is.null(hold)) NA else hold$id,
      data = list(
        "titulo" = input$titulo,
        "columna_asociada" = input$columna_asociada,
        "url" = input$url
      )
    )
    
    out
  })
  
  ###############
  #VALIDAR DATOS#
  ###############
  validate_edit <- eventReactive(input$submit, {
    
    dat <- edit_ind_dat()
    
    validado = FALSE
    columna = dat$data$columna_asociada
    url= dat$data$url
    
    
    #VERIFICA QUE SEA UN ENLACE VÁLIDO PARA LEER UN CSV
    tryCatch({
      tabla= read.csv(url)
      
      #VERIFICA QUE LA COLUMNA EXISTA EN EL CSV
      if(columna %in% colnames(tabla)){
        validado=TRUE
      }else{
        shinyFeedback::showFeedbackDanger(
          "columna_asociada",
          text = "La columna no existe en la tabla. Verifique nuevamente."
        )
      }
      
    },
    error = function(e){
      shinyjs::disable('submit')
    },
    warning = function(w){
      shinyFeedback::showFeedbackDanger(
        "url",
        text = paste0("Ingrese un URL válido.")
      )
    }
    )#end tryCatch
    
    if(isTRUE(validado)){
      return(dat)
      #shinyFeedback::showFeedbackWarning(
      #  "titulo",
      #  text = paste0(unname(dat$data))
      #)
    }else{
      shinyjs::disable('submit')
    }
    
  })
  
  observeEvent(validate_edit(), {
    removeModal()
    dat <- validate_edit()
    
    tryCatch({
      
      if (is.na(dat$id)) {
        # Agregar
        
        poolWithTransaction(pool, function (conn) {
          x <- dbSendQuery(conn, 
                           "INSERT INTO indicador (titulo,columna_asociada, url) VALUES ($1, $2, $3)",
                           params = c(
                             unname(dat$data)
                           )
          )
          dbClearResult(x)
        })
        
        
      } else {
        # Modificar uno existente
        poolWithTransaction(pool, function (conn) {
          dbExecute(conn,
                    "UPDATE indicador SET titulo=$1, columna_asociada=$2, url=$3 WHERE id=$4",
                    params = c(
                      unname(dat$data),
                      list(dat$id)
                    )
          )
        })
      }
      
      session$userData$indicador_trigger(session$userData$indicador_trigger() + 1)
      showToast("success", paste0(modal_title, " Successs"))
    }, error = function(error) {
      
      msg <- paste0(modal_title, " Error")
      
      
      # print `msg` so that we can find it in the logs
      print(msg)
      # print the actual error to log it
      print(error)
      # show error `msg` to user.  User can then tell us about error and we can
      # quickly identify where it cam from based on the value in `msg`
      showToast("error", msg)
    })
  })
  
}