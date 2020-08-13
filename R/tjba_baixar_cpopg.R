#' Baixa dados processuais de primeira instância do TJBA
#'
#' @param processo Vetor de processos
#' @param diretorio Onde armazenar os dados
#'
#' @return htmls
#' @export
#'
tjba_baixar_cpopg <- function(processo, diretorio = "."){


  url1 <- "http://esaj.tjba.jus.br/cpopg/open.do"


  url2 <- "http://esaj.tjba.jus.br/cpopg/search.do"

  url_img <- "http://esaj.tjba.jus.br/cpopg/imagemCaptcha.do"
  url_som <- "http://esaj.tjba.jus.br/cpopg/somCaptcha.do"


  purrr::walk(processo,purrr::possibly(~{

    x <- .x %>%
      stringr::str_remove_all("\\D")

    p <- abjutils::build_id(x)

    unificado <- stringr::str_sub(p,1,15)
    foro <- stringr::str_sub(p,-4)

    temp <- tempfile()

    r <- httr::GET(url1)


    r2 <- httr::GET(url_img)

    httr::GET(url_som,httr::write_disk(temp,overwrite = TRUE))


    captcha <- decifrar(temp)

    query <-
      list(
        dadosConsulta.localPesquisa.cdLocal = "-1",
        cbPesquisa = "NUMPROC",
        dadosConsulta.tipoNuProcesso = "UNIFICADO",
        numeroDigitoAnoUnificado = unificado,
        foroNumeroUnificado = foro,
        dadosConsulta.valorConsultaNuUnificado = p,
        dadosConsulta.valorConsulta = "",
        vlCaptcha = captcha
      )

    url_processo <- httr::GET(url2,query = query) %>%
      purrr::pluck("url")

    arquivo <- file.path(diretorio,paste0(stringr::str_replace_all(Sys.Date(),"\\D","_"),"_cpopg_",x,".html"))

    httr::GET(url_processo,httr::write_disk(arquivo,overwrite = TRUE))

  },NULL))

}
