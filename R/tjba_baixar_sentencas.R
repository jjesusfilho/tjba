#' Baixa dados processuais de primeira instância do TJBA
#'
#' @param processo Vetor de processos
#' @param url Vetor de urls
#' @param diretorio Onde armazenar os dados
#'
#' @return pdfs
#' @export
#'
tjba_baixar_sentencas <- function(processo, url, diretorio = "."){


  url1 <- "http://esaj.tjba.jus.br/cpopg/open.do"


  url2 <- "http://esaj.tjba.jus.br/cpopg/search.do"

  url_img <- "http://esaj.tjba.jus.br/cpopg/imagemCaptcha.do"
  url_som <- "http://esaj.tjba.jus.br/cpopg/somCaptcha.do"


  purrr::walk2(processo,url,purrr::possibly(~{

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


   r1 <-  httr::GET(url_processo)

   r2 <- httr::GET(url)

   url_redirecionada <- r2$url

   r3 <- httr::GET(url_redirecionada)

   cd_processo <- stringr::str_extract(.y,"(?<=codigo\\=)\\w+")
   cd_documento <- stringr::str_extract(.y,"(?<=Documento\\=)\\d+")
   cd_foro <- as.numeric(foro)
   query <-
     list(
       tpOrigem = "2",
       numInicial = "1",
       cdProcesso = cd_processo,
       cdForo = cd_foro,
       nmAlias = "PG5BA",
       origemDocumento = "M",
       nuPagina = "0",
       deTipoDocDigital = "Senten\xe7a",
       flOrigem = "P",
       cdDocumento = cd_documento,
       cdFormatoDoc = "5"
     )

   url_pdf<- httr::parse_url("http://esaj.tjba.jus.br/pastadigital/getPDF.action")
   url_pdf$query <- query
   url_pdf<-httr::build_url(url_pdf)



  arquivo <- file.path(diretorio,paste0(stringr::str_replace_all(Sys.Date(),"\\D","_"),"_cpopg_",x,"_sentenca_",cd_documento,".pdf"))

  httr::GET(url_pdf,httr::write_disk(arquivo,overwrite = TRUE))

  },NULL))

}


