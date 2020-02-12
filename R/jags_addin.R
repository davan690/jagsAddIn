
#' jags AddIn
#'
#' @usage jags_addin()
#' @return jags in R template code
#' @export
#'
#' @examples
#'
#' # HOW TO USE:
#' # 1. open new R script
#' # 2. run jags_addin()
#'
#' cat("model {
#'
#' for (i in 1:N) {
#'
#' }")
#'
#' data <- list("fdfd")
#'
#' params <- list("fdfd")
#'
#' nc <- 1
#' ns <- 1000
#' nb <- 100
#' nt <- 1
#'
#' bug_out <-
#' bugs(
#'                            data = dat,
#'                            inits = inits,
#'                            parameters.to.save = params,
#'                            n.chains = nc,
#'                            n.iter = ns,
#'                            n.burnin = nb,
#'                            n.thin = nt,
#'                            DIC = TRUE
#'                            working.directory = getwd())
#'
jags_addin <- function() {

  library(shiny)
  library(miniUI)
  library(rstudioapi)

  ui <- miniPage(
    gadgetTitleBar("jags_addin"),
    miniContentPanel(
      # Define layout, inputs, outputs
      textInput("filename", "Input data file name", value = ".csv"),
      textInput("inits", "inits (seperate by comma)", value = ""),
      textInput("param", "Parameters to monitor (seperate by comma)", value = ""),
      textInput("nchains", "Number of chains", value = "1"),
      textInput("ns", "Samples", value = "1000"),
      textInput("burnin", "Burn-in", value = "100"),
      textInput("thin", "Thinning", value = "1"),
      checkboxInput("dic", "DIC", value = FALSE)
    )
  )

  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.

    # When the Done button is clicked, return a value
    observeEvent(input$done, {

      id <- getActiveDocumentContext()$id
      param_names <- strsplit(x = input$param, split = ",")[[1]]
      # Qparams <- paste0(dQuote(param_names), collapse = ",")
      Qparams <-
        paste0(paste0('"', param_names, '"'), collapse = ",")

      paramValue <- paste0("params <- list(",
                           Qparams, ")")

      dataval <-  paste0("dat <- list(",
                         Qparams, ")")
      dataread <- paste0("datin <- read.csv(", input$filename, ")")

      res <-
        paste0(
          'library(R2jags)\n\n
          cat("model {

                # likelihood
                for (i in 1:N) {

                 }

                # priors

          ")\n\n',
          "# load data \n",
          dataread, '\n\n',
          "# initial values for the priors \n",
          dataval, '\n\n',
          "# parameters monitored \n",
          paramValue, '\n\n',
          "# MCMC settings \n",
          'nc <- ', input$nchains, '   # number of chains \n',
          'ns <- ', input$ns, '   # number of iterations \n',
          'nb <- ', input$burnin, '   # burn-in \n',
          'nt <- ', input$thin, '   # thinning of mcmc chain \n\n',
          "# call JAGS from R \n",
          'bug_out <- \nbugs(\n',
          '\t\tdata = dat,\n',
          '\t\tinits = inits,\n',
          '\t\tparameters.to.save = params,\n',
          '\t\tn.chains = nc,\n',
          '\t\tn.iter = ns,\n',
          '\t\tn.burnin = nb,\n',
          '\t\tn.thin = nt,\n',
          '\t\tDIC = ', input$dic, '\n',
          '\t\tworking.directory = getwd())'
        )

      # rstudioapi::insertText(text = res, id = id)
      rstudioapi::setDocumentContents(text = res, id = id)
      # stopApp(cat(noquote(res)))
      stopApp()
    })
  }

  runGadget(ui, server)
}

