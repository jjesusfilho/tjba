#' Ler movimentação do TJBA
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Informar diretório se não informar arquivos
#'
#' @return tibble
#' @export
#'
tjba_ler_movimentacoes <- function(arquivos = NULL,diretorio = ".") {

  if (is.null(arquivos)){
    arquivos <- list.files(
      path = diretorio, pattern = ".html",
      full.names = TRUE
    )
  }

  pb <- progress::progress_bar$new(total = length(arquivos))

  purrr::map_dfr(arquivos, purrr::possibly(~{


    pb$tick()

    processo <- stringr::str_extract(.x, "\\d{20}")

    texto <- xml2::read_html(.x) %>%
      xml2::xml_find_first(xpath = "//table[@id='tabelaTodasMovimentacoes']")

    data <- xml2::xml_find_all(texto, ".//td[@width='120']") %>%
      xml2::xml_text(trim = TRUE)

    mov <- xml2::xml_find_all(texto, ".//td[@style='vertical-align: top; padding-bottom: 5px']") %>%
      xml2::xml_text(trim = TRUE)

    url <- xml2::xml_find_all(texto, ".//td[@width='120']/following-sibling::td[1]") %>%
      xml2::xml_child("a") %>%
      purrr::map_chr(~xml2::xml_attr(.x,"href")) %>%
      purrr::modify_if(!is.na(.),~paste0("http://esaj.tjba.jus.br/",.x))

    tibble::tibble(processo = processo, data = data, movimentacao = mov, url = url) %>%
      tidyr::separate(movimentacao, c("principal","secundario"),sep = "(\r\n)+",extra="merge") %>%
      dplyr::mutate(secundario = stringr::str_trim(secundario))
  }, otherwise = NULL))
}
