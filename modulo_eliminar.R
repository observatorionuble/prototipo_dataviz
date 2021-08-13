modulo_eliminar <- function(input, output, session, modal_title, pool, indicador_to_delete, modal_trigger) {
  ns <- session$ns
  # Observes trigger for this module (here, the Delete Button)
  observeEvent(modal_trigger(), {
    # Authorize who is able to access particular buttons (here, modules)
    #req(session$userData$email == 'tycho.brahe@tychobra.com')
    
    showModal(
      modalDialog(
        div(
          style = "padding: 30px;",
          class = "text-center",
          h2(
            style = "line-height: 1.75;",
            paste0(
              'Está seguro de querer eliminar "',
              indicador_to_delete()$titulo,
              '"?'
            )
          )
        ),
        title = modal_title,
        size = "m",
        footer = list(
          modalButton("Cancelar"),
          actionButton(
            ns("submit_delete"),
            "Eliminar",
            class = "btn-danger",
            style="color: #fff;"
          )
        )
      )
    )
  })
  
  observeEvent(input$submit_delete, {
    req(indicador_to_delete())
    
    removeModal()
    
    tryCatch({
      
      id <- indicador_to_delete()$id
      
      poolWithTransaction(pool, function (conn) {
        dbExecute(conn,
                  "DELETE FROM indicador WHERE id=$1",
                  params = c(id)
        )
      })
      
      
      session$userData$indicador_trigger(session$userData$indicador_trigger() + 1)
      showToast("success", "Se ha eliminado satisfactoriamente.")
    }, error = function(error) {
      
      msg <- "Error al eliminar."
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