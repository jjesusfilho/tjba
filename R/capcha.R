#' Lê arquivo captcha som
#'
#' @param arquivo Arquivo
#'
#' @return código
#'
ler <- function(arquivo = NULL){

  tuneR::readMP3(arquivo)@left %>%
    paste(collapse = " ") %>%
    stringr::str_split('(0\\s){50,}') %>%
    magrittr::extract2(1) %>%
    purrr::map(~{
      stringr::str_split(.x,"\\s") %>%
        unlist() %>%
        as.numeric() %>%
        sum(na.rm=TRUE) %>%
        abs()

    }) %>%
    subset(. > 1000) %>%
    unlist()
}




#' Decifra arquivo captcha som
#'
#' @param arquivo Arquiv
#'
#' @return letras
#'
decifrar <- function(arquivo = NULL){

  letras <- structure(list(letra = c("p", "x", "f", "u", "s", "d", "m", "b",
                                "e", "k", "v", "r", "h", "y", "q", "a", "t", "j", "c", "w", "n",
                                "z", "i"), som = c(3159109, 106172, 13473, 1784417, 26149, 3026354,
                                                   712776, 1607335, 4128582, 10816, 2457078, 207783, 618148, 605513,
                                                   4099154, 633481, 816015, 2480423, 1982845, 621146, 2420564, 509752,
                                                   1494030)), row.names = c(NA, -23L), class = c("tbl_df", "tbl",
                                                                                                 "data.frame"))


  ler(arquivo) %>%
    pmatch(letras$som) %>%
    magrittr::extract(letras$letra,.) %>%
    stringr::str_c(collapse="")

}
