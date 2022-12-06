
#' Recreate the View Function
#'
#' @param x default x View Parameter
#' @param title default title View Parameter
#'
#' @return View data object
#'
View1<-function (x, title ='')
{
  expr <- deparse(substitute(x), backtick = TRUE)
  if (missing(title))
    title <- paste(expr[1])
  expr <- paste(expr, collapse = " ")
  name <- ""
  env <- emptyenv()
  if (.rs.isViewOverride()) {
    name <- title
  }
  else if (is.name(substitute(x))) {
    name <- paste(deparse(substitute(x)))
    env <- .rs.findViewingEnv(name)
  }
  if (is.function(x)) {
    srcref <- .rs.getSrcref(x)
    if (!is.null(srcref)) {
      srcfile <- attr(srcref, "srcfile", exact = TRUE)
      filename <- .rs.nullCoalesce(srcfile$filename, "")
      if (!identical(filename, "~/.active-rstudio-document") &&
          file.exists(filename)) {
        .Call("rs_jumpToFunction", normalizePath(filename,
                                                 winslash = "/"), srcref[[1]], srcref[[5]],
              TRUE, PACKAGE = "(embedding)")
        return(invisible(NULL))
      }
    }
    title <- sub("^[^:]+:::?", "", title)
    namespace <- .rs.environmentName(environment(x))
    if (identical(namespace, "R_EmptyEnv") || identical(namespace,
                                                        ""))
      namespace <- "viewing"
    else if (identical(namespace, "R_GlobalEnv"))
      namespace <- ".GlobalEnv"
    invisible(.Call("rs_viewFunction", x, title, namespace,
                    PACKAGE = "(embedding)"))
    return(invisible(NULL))
  }
  else if (inherits(x, "vignette")) {
    file.edit(file.path(x$Dir, "doc", x$File))
    return(invisible(NULL))
  }
  if (.rs.dataViewer.shouldUseObjectExplorer(x)) {
    view <- .rs.explorer.viewObject(x, title = title, envir = env)
    return(invisible(view))
  }
  if (inherits(x, "pandas.core.frame.DataFrame"))
    x <- reticulate::py_to_r(x)
  coerced <- x
  eval(expr = substitute(as.data.frame(coerced, optional = TRUE)),
       envir = globalenv())
  cacheKey <- .rs.addCachedData(force(x), name)
  invisible(.Call(getNativeSymbolInfo("rs_viewData"), x, expr, title, name, env,
                  cacheKey, FALSE))
}


#' Override View to allow for Choices
#'
#' @param df WEhich dataframe to work on
#'
#' @return Void
#' @export
#'
View <-function(df) {
  a<-deparse(substitute(df))
  actiontype<-get_actiontype(a)
  if (actiontype=="Copy")
    clipr::write_clip(df)
  else if (actiontype =="View") View1(df,a)
  else if (actiontype =="Datatable")
    #DT::datatable(df) %>%
    DT::formatStyle(table =
                      DT::datatable(df
                                    ,options = list(
                                      dom = 'ftp',
                                      initComplete = DT::JS(
                                        "function(settings, json) {",
                                        "$('body').css({'font-size': '10px'});",
                                        "}")))
                    ,columns = colnames(df), `font-size` = "10px")
  #else if (actiontype =="Reset") resetView()

}

# Create MiniUI -----------------------------------------------------------
#' Create MiniUI for Selection
#'
#' @param a Dateaframe to act on
#'
#' @return A choice of action
#'
get_actiontype <- function(a) {
  title_bar <- paste("Choose an Action on",a)
  ui <- miniUI::miniPage(
    shinyjs::useShinyjs(),
    miniUI::gadgetTitleBar(title_bar),
    miniUI::miniContentPanel(
      shiny::radioButtons("ret_action", "Select an action",
                          c("View","Datatable","Copy"
                            #,"Reset"
                            ))
      #,shiny::actionButton("resettodefault", "Revert to Default")
    ),
    shiny::tags$script('
                $(document).keyup(function(event) {
                if ((event.keyCode == 13)) {
                $("#done").click();
                }
                });')
  )
  server <- function(input, output) {
    shiny::observeEvent(input$done, {
      shiny::stopApp(input$ret_action)
    })
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(FALSE)
    })
  }
  shiny::runGadget(ui, server)
}

