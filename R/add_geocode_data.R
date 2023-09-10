## TODO:
##
## * Document functions

require(checkmate, quietly = TRUE)
require(cli, quietly = TRUE)
require(dplyr, quietly = TRUE)

add_geocode_data <- function(data) {
  checkmate::assert_tibble(data)

  cli::cli_progress_step("Adding geocode data")

  out <- data |>
    dplyr::mutate(
      latitude = dplyr::case_when(
        state == "Acre" ~ -9.9862943, # Rio Branco
        state == "Alagoas" ~ -9.5945358, # Maceió
        state == "Amapá" ~ 0.1017719, # Macapá
        state == "Amazônas" ~ -3.0446589, # Manaus
        state == "Bahia" ~ -12.8753927, # Salvador
        state == "Ceará" ~ -3.7933105, # Fortaleza
        state == "Distrito Federal" ~ -15.7217003, # Brasília
        state == "Espírito Santo" ~ -20.2821867, # Vitória
        state == "Goiás" ~ -16.6958107, # Goiânia
        state == "Maranhão" ~ -2.5606296, # São Luís
        state == "Mato Grosso" ~ -15.4196336, # Cuiabá
        state == "Mato Grosso do Sul" ~ -20.4810804, # Campo Grande
        state == "Minas Gerais" ~ -19.9026404, # Belo Horizonte
        state == "Paraná" ~ -25.4950245, # Curitiba
        state == "Paraíba" ~ -7.1466015, # João Pessoa
        state == "Pará" ~ -1.3413464, # Belém
        state == "Pernambuco" ~ -8.043303, # Recife
        state == "Piauí" ~ -5.0937344, # Teresina
        state == "Rio de Janeiro" ~ -22.9137906, # Rio de Janeiro
        state == "Rio Grande do Norte" ~ -5.799913, # Natal
        state == "Rio Grande do Sul" ~ -30.1087672, # Porto Alegre
        state == "Rondônia" ~ -8.7565367, # Porto Velho
        state == "Roraima" ~ 2.8071961, # Boa Vista
        state == "Santa Catarina" ~ -27.5712063, # Florianópolis
        state == "Sergipe" ~ -11.0059634, # Aracaju
        state == "São Paulo" ~ -23.6820636, # São Paulo
        state == "Tocantins" ~ -10.2600493 # Palmas
      ),
      longitude = dplyr::case_when(
        state == "Acre" ~ -67.9869962, # Rio Branco
        state == "Alagoas" ~ -35.8514919, # Maceió
        state == "Amapá" ~ -51.2616744, # Macapá
        state == "Amazônas" ~ -60.049506, # Manaus
        state == "Bahia" ~ -38.6666115, # Salvador
        state == "Ceará" ~ -38.6020174, # Fortaleza
        state == "Distrito Federal" ~ -48.1021708, # Brasília
        state == "Espírito Santo" ~ -40.3269035, # Vitória
        state == "Goiás" ~ -49.4690802, # Goiânia
        state == "Maranhão" ~ -44.3405241, # São Luís
        state == "Mato Grosso" ~ -56.2240693, # Cuiabá
        state == "Mato Grosso do Sul" ~ -54.7179359, # Campo Grande
        state == "Minas Gerais" ~ -44.1288626, # Belo Horizonte
        state == "Paraná" ~ -49.4546099, # Curitiba
        state == "Paraíba" ~ -34.9639996, # João Pessoa
        state == "Pará" ~ -48.6116775, # Belém
        state == "Pernambuco" ~ -35.0166191, # Recife
        state == "Piauí" ~ -42.8234802, # Teresina
        state == "Rio de Janeiro" ~ -43.7756411, # Rio de Janeiro
        state == "Rio Grande do Norte" ~ -35.3046462, # Natal
        state == "Rio Grande do Sul" ~ -51.3419529, # Porto Alegre
        state == "Rondônia" ~ -63.9373089, # Porto Velho
        state == "Roraima" ~ -60.7789636, # Boa Vista
        state == "Santa Catarina" ~ -48.7999353, # Florianópolis
        state == "Sergipe" ~ -37.2679695, # Aracaju
        state == "São Paulo" ~ -46.9249578, # São Paulo
        state == "Tocantins" ~ -48.4296364 # Palmas
      ),
      region = dplyr::case_when(
        state == "Acre" ~ "North",
        state == "Alagoas" ~ "Northeast",
        state == "Amapá" ~ "North",
        state == "Amazônas" ~ "North",
        state == "Bahia" ~ "Northeast",
        state == "Ceará" ~ "Northeast",
        state == "Distrito Federal" ~ "Midwest",
        state == "Espírito Santo" ~ "Southeast",
        state == "Goiás" ~ "Midwest",
        state == "Maranhão" ~ "Northeast",
        state == "Mato Grosso" ~ "Midwest",
        state == "Mato Grosso do Sul" ~ "Midwest",
        state == "Minas Gerais" ~ "Southeast",
        state == "Paraná" ~ "South",
        state == "Paraíba" ~ "Northeast",
        state == "Pará" ~ "North",
        state == "Pernambuco" ~ "Northeast",
        state == "Piauí" ~ "Northeast",
        state == "Rio de Janeiro" ~ "Southeast",
        state == "Rio Grande do Norte" ~ "Northeast",
        state == "Rio Grande do Sul" ~ "South",
        state == "Rondônia" ~ "North",
        state == "Roraima" ~ "North",
        state == "Santa Catarina" ~ "South",
        state == "Sergipe" ~ "Northeast",
        state == "São Paulo" ~ "Southeast",
        state == "Tocantins" ~ "North"
      )
    )

  invisible(out)
}

add_labren_data <- function() {

}
