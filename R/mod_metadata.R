metadataUI <- function(id){
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns('metadata'))
  )
}

metadataServer <- function(id,
                           r_neosites_data){
  moduleServer(
    id,
    function(input, output, session) {

      # Sidebar table withe the NeotomaDB siteId metadata
      output$metadata <- DT::renderDT({

        name  <-  as.character(r_neosites_data()$sitename)
        description  <-  as.character(r_neosites_data()$sitedescription)
        notes  <-  as.character(r_neosites_data()$notes)

        data <- data.frame(c(description,
                             'notes', notes))

        colnames(data) <- as.character(name)

        DT::datatable(data,
                      rownames= FALSE,
                      options = list(dom = 't',
                                     ordering = FALSE))

      })
    }
  )
}