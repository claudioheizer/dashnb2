library(dplyr)
library(ggplot2)
library(httr)
library(lubridate)
library(shiny)
library(shinymanager)
library(htmlwidgets)
# library(shinyRenderWidget)
library(DT)
library(gtools)

result <- read.csv("base.csv")

summary(result)

# Data pre-processing ----
dados <- result %>% mutate(
  amostra = case_when(
    puerp_hosp %in% c(114, 115, 125, 127, 131, 132, 144, 145, 151, 152, 154, 155, 156, 182, 201, 202, 225, 226, 245, 246,
                      262, 263, 264, 265, 267, 268, 269, 272, 303, 304, 327, 328, 329, 337, 383, 384, 394, 395, 396, 412,
                      422, 423, 434, 435, 446, 447, 467, 468, 469, 472, 488, 492, 493, 505, 510, 511, 522, 529, 530, 531,
                      535, 537, 538, 542, 544, 545, 550, 551, 559, 560, 562, 564, 565) ~ 30,
    puerp_hosp %in% c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 116, 117, 118, 119, 120, 121, 122, 
                      123, 124, 126, 128, 129, 130, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 146, 147, 148, 
                      149, 150, 153, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 
                      174, 175, 176, 177, 178, 179, 180, 181, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 
                      195, 196, 197, 198, 199, 200, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 
                      217, 218, 219, 220, 221, 222, 223, 224, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238,
                      239, 240, 241, 242, 243, 244, 247, 248, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 259, 260, 
                      261, 266, 270, 271, 280, 281, 282, 283, 284, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 
                      296, 297, 298, 299, 300, 301, 302, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 
                      318, 319, 320, 321, 322, 323, 324, 349, 350, 351, 352, 353, 354, 355, 356, 357, 358, 359, 360, 361, 
                      362, 363, 364, 365, 366, 367, 368, 369, 370, 371, 372, 373, 374, 375, 376, 377, 378, 379, 380, 381, 
                      382, 385, 386, 390, 391, 392, 393, 397, 398, 399, 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 
                      410, 413, 414, 415, 416, 417, 418, 419, 420, 421, 424, 425, 426, 427, 428, 429, 430, 431, 432, 433, 
                      436, 437, 438, 439, 440, 441, 442, 443, 444, 445, 448, 449, 450, 451, 452, 453, 454, 455, 456, 457, 
                      458, 459, 460, 461, 462, 463, 464, 465, 466, 470, 471, 473, 474, 475, 476, 477, 478, 479, 480, 481, 
                      482, 483, 484, 485, 486, 487, 489, 490, 491, 494, 495, 496, 497, 498, 499, 500, 501, 502, 503, 504, 
                      506, 507, 508, 509, 512, 513, 514, 515, 516, 517, 518, 519, 520, 521, 523, 524, 525, 526, 527, 528, 
                      532, 533, 534, 536, 539, 540, 541, 543, 546, 547, 548, 549, 552, 553, 554, 555, 556, 557, 558, 561, 
                      563) ~ 50, 
    puerp_hosp %in% c(273, 274, 275, 276, 277, 278, 279, 387, 388, 389, 411) ~ 90, 
    TRUE ~ 50)) %>% 
  
  rename(Código = puerp_hosp,
         Macrorregião = puerp_macro,
         Data1 = puerp_lu_1, 
         Data2 = pront_bl18_280_2) %>% 
  
  mutate(
    País = "Brasil",
    Estado = case_when(
      puerp_estado_2 == 11 ~ "Rondônia",
      puerp_estado_2 == 12 ~ "Acre",
      puerp_estado_2 == 13 ~ "Amazonas",
      puerp_estado_2 == 14 ~ "Roraima",
      puerp_estado_2 == 15 ~ "Pará",
      puerp_estado_2 == 16 ~ "Amapá",
      puerp_estado_2 == 17 ~ "Tocantins",
      puerp_estado_2 == 21 ~ "Maranhão",
      puerp_estado_2 == 22 ~ "Piauí",
      puerp_estado_2 == 23 ~ "Ceará",
      puerp_estado_2 == 24 ~ "Rio Grande do Norte",
      puerp_estado_2 == 25 ~ "Paraíba",
      puerp_estado_2 == 26 ~ "Pernambuco",
      puerp_estado_2 == 27 ~ "Alagoas",
      puerp_estado_2 == 28 ~ "Sergipe",
      puerp_estado_2 == 29 ~ "Bahia",
      puerp_estado_2 == 31 ~ "Minas Gerais",
      puerp_estado_2 == 32 ~ "Espírito Santo",
      puerp_estado_2 == 33 ~ "Rio de Janeiro",
      puerp_estado_2 == 35 ~ "São Paulo 1",
      puerp_estado_2 == 36 ~ "São Paulo 2",
      puerp_estado_2 == 37 ~ "São Paulo 3",
      puerp_estado_2 == 41 ~ "Paraná",
      puerp_estado_2 == 42 ~ "Santa Catarina",
      puerp_estado_2 == 43 ~ "Rio Grande do Sul",
      puerp_estado_2 == 50 ~ "Mato Grosso do Sul",
      puerp_estado_2 == 51 ~ "Mato Grosso",
      puerp_estado_2 == 52 ~ "Goiás",
      puerp_estado_2 == 53 ~ "Distrito Federal",
      TRUE ~ redcap_data_access_group
    ),
    Hospital = case_when(
      Código == 101 ~ "Hospital de Base Porto Velho",
      Código == 102 ~ "Hospital Maternidade Mãe Esperança",
      Código == 103 ~ "Maternidade e Clínicas de Mulheres Bárbara Heliodora",
      Código == 104 ~ "Instituto da Mulher Dona Lindu",
      Código == 105 ~ "Maternidade Azilda da Silva Marreiro",
      Código == 106 ~ "Maternidade da Alvorada",
      Código == 107 ~ "Maternidade de Referencia Ana Braga",
      Código == 108 ~ "Hospital Materno Infantil N. Sra. de Nazareth",
      Código == 109 ~ "Santa Casa de Misericórdia do Pará",
      Código == 110 ~ "Hospital Municipal de Santarém",
      Código == 111 ~ "Hospital da Mulher - Amapá",
      Código == 112 ~ "Hospital e Maternidade Dona Regina Siqueira Campos de Palmas",
      Código == 113 ~ "Hospital Regional de Paraíso Dr. Alfredo O. Barros",
      Código == 114 ~ "Hospital Regional de Extrema",
      Código == 115 ~ "Unidade Hospitalar de Novo Airão",
      Código == 116 ~ "Hospital Santa Juliana",
      Código == 117 ~ "Associação Beneficente São José",
      Código == 118 ~ "Hospital Santa Maria De Ananindeua",
      Código == 119 ~ "Hospital D. Luiz I",
      Código == 120 ~ "Maternidade do Povo Filial",
      Código == 121 ~ "Associação Beneficente Espedito Magalhães",
      Código == 122 ~ "Hospital e Maternidade Sagrada Família",
      Código == 123 ~ "Hospital São Camilo e São Luís",
      Código == 124 ~ "Hospital Adventista de Belém",
      Código == 125 ~ "Hospital Maternidade Unimed",
      Código == 126 ~ "Hospital Unimed",
      Código == 127 ~ "Hospital Panamericano",
      Código == 128 ~ "Maternidade Saúde da Criança",
      Código == 129 ~ "Hospital Santa Terezinha",
      Código == 130 ~ "Hospital e Maternidade Cristo Rei Palmas",
      Código == 131 ~ "Hospital Unimed De Boa Vista",
      Código == 132 ~ "Hospital Unimed Oeste do Pará",
      Código == 133 ~ "Hospital Municipal Materno Infantil Cacoal",
      Código == 134 ~ "Hospital Regional Adamastor Teixeira De Oliveira",
      Código == 135 ~ "Hospital da Mulher e da Criança do Juruá",
      Código == 136 ~ "Hospital Municipal Dr. Pedro Paulo Barcauí - Redenção",
      Código == 137 ~ "Hospital Municipal Oscar de Dea",
      Código == 138 ~ "Hospital Municipal Maria Santana Rocha Franco",
      Código == 139 ~ "Hospital Municipal Daniel Goncalves",
      Código == 140 ~ "Hospital Municipal de Itaituba",
      Código == 141 ~ "Hospital Materno Infantil de Marabá",
      Código == 142 ~ "Hospital Geral de Parauapebas Manoel Evaldo Benevides Alves",
      Código == 143 ~ "Hospital Estadual de Laranjal do Jari - Amapá",
      Código == 144 ~ "Unidade Hospitalar de Alvarães",
      Código == 145 ~ "Unidade Mista de Saúde de Medicilândia",
      Código == 146 ~ "Clínica Dr. João Pedrosa",
      Código == 147 ~ "Hospital Santo Antônio Maria Zaccaria",
      Código == 148 ~ "Hospital de Cametá",
      Código == 149 ~ "Hospital Maternidade São Domingos Sávio",
      Código == 150 ~ "Hospital Dom Orione de Araguaína",
      Código == 151 ~ "Hospital das Bem-Aventuranças - Viseu",
      Código == 152 ~ "Hospital Dom Bosco",
      Código == 153 ~ "Hospital Bom Pastor Guajará-Mirim",
      Código == 154 ~ "Hospital Vida Mamaray",
      Código == 155 ~ "Hospital e Maternidade São Paulo",
      Código == 156 ~ "Hospital e Maternidade Santa Cecília - Espigão D'oeste",
      Código == 157 ~ "HRMI Hospital Regional Materno Infantil de Imperatriz",
      Código == 158 ~ "Hospital e Maternidade de São José de Ribamar",
      Código == 159 ~ "Complexo Hospitalar Materno Infantil do Maranhão",
      Código == 160 ~ "Maternidade de Alta Complexidade do Maranhão",
      Código == 161 ~ "Maternidade Dona Evangelina Rosa",
      Código == 162 ~ "Pronto Socorro Geral Dr. Antônio P. de A. Martins",
      Código == 163 ~ "Pronto Socorro Geral e Maternidade Dr. Antônio P. de A. Martins",
      Código == 164 ~ "Hospital Municipal Dr. João Elísio De Holanda",
      Código == 165 ~ "Maternidade Escola Januário Cicco",
      Código == 166 ~ "Hospital Maternidade Dr. Sadi Mendes Maternidade do Divino Amor",
      Código == 167 ~ "Hospital Regional Monsenhor Antônio Barros",
      Código == 168 ~ "Instituto de Saúde Elpídio De Almeida",
      Código == 169 ~ "Complexo de Saúde do Município de Guarabira",
      Código == 170 ~ "Hospital Distrital de Itaporanga Dr. José Gomes Da Silva",
      Código == 171 ~ "Maternidade Cândida Vargas",
      Código == 172 ~ "Maternidade Peregrino Filho",
      Código == 173 ~ "Centro Integrado de Saúde Amauri de Medeiros - CISAM",
      Código == 174 ~ "Hospital das Clínicas",
      Código == 175 ~ "Hospital da Mulher do Recife Dra. Mercês Pontes Cunha",
      Código == 176 ~ "Maternidade Nossa Senhora de Lourdes",
      Código == 177 ~ "Hospital Inácia Pinto dos Santos",
      Código == 178 ~ "Hospital Geral Menandro de Faria",
      Código == 179 ~ "Instituto De Perinatologia Da Bahia",
      Código == 180 ~ "Maternidade Albert Sabin",
      Código == 181 ~ "Maternidade Professor Jose Maria de Magalhães Neto",
      Código == 182 ~ "Hospital de Altos Inst. de Saúde Jose Gil Barbosa",
      Código == 183 ~ "Hospital Municipal de Candeias",
      Código == 184 ~ "Santa Casa de Misericórdia do Maranhão",
      Código == 185 ~ "Maternidade Nossa Senhora da Penha",
      Código == 186 ~ "Hospital Cura Dars",
      Código == 187 ~ "Hospital Maternidade São Vicente de Paulo",
      Código == 188 ~ "Santa Casa de Misericórdia De Sobral",
      Código == 189 ~ "Clipsi Serviços Hospitalares",
      Código == 190 ~ "Hospital Escola da FAP",
      Código == 191 ~ "Hospital Memorial Guararapes",
      Código == 192 ~ "IMIP",
      Código == 193 ~ "Hospital Regional de Arapiraca",
      Código == 194 ~ "Casa de Saúde Santo Antônio",
      Código == 195 ~ "Hospital Regional Santa Rita E Maternidade Santa Olimpia",
      Código == 196 ~ "Santa Casa De Misericórdia De Penedo",
      Código == 197 ~ "Hospital Santa Isabel",
      Código == 198 ~ "Hospital Regional José Franco Sobrinho",
      Código == 199 ~ "Hospital O Bom Samaritano",
      Código == 200 ~ "Unidade Mista Dr. José Carneiro de Campos",
      Código == 201 ~ "Maternidade Maria Luiza Dias Laudano - Pojuca",
      Código == 202 ~ "Santa Casa de Paracuru - Ceará",
      Código == 203 ~ "Hospital São Rafael",
      Código == 204 ~ "Clínica São Marcos",
      Código == 205 ~ "Hospital Gênesis",
      Código == 206 ~ "Hospital e Maternidade Gastroclínica",
      Código == 207 ~ "Hospital Geral Paraíba e Hapclinica João Pessoa",
      Código == 208 ~ "Hospital Alberto Urquiza Wanderley",
      Código == 209 ~ "Hospital Memorial Petrolina",
      Código == 210 ~ "Hospital Santa Joana",
      Código == 211 ~ "Hospital Esperança",
      Código == 212 ~ "Hospital Memorial São José",
      Código == 213 ~ "Clínica Luiza Coelho",
      Código == 214 ~ "Real Hospital Português",
      Código == 215 ~ "Hospital Memorial Arthur Ramos",
      Código == 216 ~ "Hospital Unimed de Maceió",
      Código == 217 ~ "Santa Casa de Maceió Unidade Farol",
      Código == 218 ~ "Clínica Santa Helena",
      Código == 219 ~ "Maternidade Santa Emília",
      Código == 220 ~ "Hospital Aliança",
      Código == 221 ~ "Hospital Jorge Valente",
      Código == 222 ~ "Hospital Português",
      Código == 223 ~ "Hospital Santo Amaro",
      Código == 224 ~ "Hospital Teresa De Lisieux",
      Código == 225 ~ "Hospital das Clínicas",
      Código == 226 ~ "Clínica Santa Ana",
      Código == 227 ~ "Hospital Geral de Alto Alegre do Maranhão",
      Código == 228 ~ "Hospital Materno Infantil",
      Código == 229 ~ "Hospital Geral de Barreirinhas",
      Código == 230 ~ "Hospital Municipal Dr. Zeca Moreira",
      Código == 231 ~ "Maternidade Carmosina Coutinho",
      Código == 232 ~ "Casa de Saúde e Maternidade de Coelho Neto",
      Código == 233 ~ "Hospital Regional Adélia Matos Fonseca",
      Código == 234 ~ "Hospital Santa Helena",
      Código == 235 ~ "Hospital Regional Tibério Nunes",
      Código == 236 ~ "Hospital Estadual Dirceu Arcoverde",
      Código == 237 ~ "Hospital Regional Justino Luz",
      Código == 238 ~ "Hospital Regional de Iguatu",
      Código == 239 ~ "Hospital Regional Ruy de Barros Correia",
      Código == 240 ~ "Hospital de Caruaru Jesus Nazareno",
      Código == 241 ~ "Hospital Regional de Palmares Dr. Silvio Magalhaes",
      Código == 242 ~ "Hospital Professor Agamenon Magalhães",
      Código == 243 ~ "Hospital Municipal Antônio Texeira Sobrinho",
      Código == 244 ~ "Hospital Municipal Dr. Gileno de Sá Oliveira",
      Código == 245 ~ "Hospital Municipal Dr. Jose Borba",
      Código == 246 ~ "Hospital e Maternidade Nossa Senhora Aparecida",
      Código == 247 ~ "Santa Casa Cururupu",
      Código == 248 ~ "Hospital e Maternidade José Pinto do Carmo",
      Código == 249 ~ "Hospital e Maternidade Regional São Francisco",
      Código == 250 ~ "Hospital São Lucas",
      Código == 251 ~ "Hospital Maternidade São Vicente de Paulo",
      Código == 252 ~ "Hospital Maternidade Almeida Castro",
      Código == 253 ~ "APAMI de Vitória de Santo Antão",
      Código == 254 ~ "Hospital Infantil Palmeira Sales de Garanhuns",
      Código == 255 ~ "Hospital e Maternidade São Jose",
      Código == 256 ~ "Maternidade Zacarias Junior",
      Código == 257 ~ "Hospital Manoel Novaes",
      Código == 258 ~ "Maternidade Otaciana Pinto (ant. Ester Gomes)",
      Código == 259 ~ "Santa Casa de Misericórdia São Judas Tadeu",
      Código == 260 ~ "Hospital Nair Alves de Souza",
      Código == 261 ~ "Hospital Nossa Senhora da Pompeia",
      Código == 262 ~ "Hospital São Vicente de Paulo",
      Código == 263 ~ "Hospital Maternidade Agenor Araújo",
      Código == 264 ~ "Hospital Dr. Jose Ramos de Oliveira",
      Código == 265 ~ "Materclinica Hospital e Maternidade",
      Código == 266 ~ "Hospital da Unimed Caruaru",
      Código == 267 ~ "Hospital São Paulo",
      Código == 268 ~ "L.A. Serviços Médicos",
      Código == 269 ~ "Hospital Agnus Dei",
      Código == 270 ~ "Hospital Metropolitano Odilon Bherens Hob",
      Código == 271 ~ "Hospital Público Regional Prefeito Osvaldo Rezende Franco",
      Código == 272 ~ "Hospital Municipal Dr. Celso Martins",
      Código == 273 ~ "Hospital Estadual Adão Pereira Nunes – SES RJ",
      Código == 274 ~ "Hospital Municipal de Piabetá",
      Código == 275 ~ "Complexo Reg. de Mesquita – SES RJ",
      Código == 276 ~ "Maternidade Municipal Mariana Bulhões",
      Código == 277 ~ "Hospital Maternidade Herculano Pinheiro – SMS Rio",
      Código == 278 ~ "Hospital Municipal Pedro II – SMS Rio",
      Código == 279 ~ "Hospital da Mulher Heloneida Studart – SES RJ",
      Código == 280 ~ "P.S. e Maternidade Nair Fonseca Leitão Arantes",
      Código == 281 ~ "Hospital Geral De Carapicuíba",
      Código == 282 ~ "Hospital Dr. Osiris Florindo Coelho Ferraz de Vasconcelos",
      Código == 283 ~ "Hospital Geral de Guarulhos Prof. Dr. Waldemar De Carvalho",
      Código == 284 ~ "Hospital Regional de Cotia",
      Código == 285 ~ "Hospital Geral de Itapevi",
      Código == 286 ~ "Hospital Maternidade Amador Aguiar",
      Código == 287 ~ "Complexo Hospitalar dos Estivadores",
      Código == 288 ~ "Hospital Guilherme Álvaro Santos",
      Código == 289 ~ "Hospital Estadual Caieiras",
      Código == 290 ~ "Hospital Municipal Dr. Ignacio Proença de Gouvea",
      Código == 291 ~ "Hospital da Mulher Maria Jose dos Santos Stein",
      Código == 292 ~ "Maternidade Cachoeirinha",
      Código == 293 ~ "Hospital Municipal Prof. Dr. Alípio Correa Netto",
      Código == 294 ~ "Hospital Municipal Tide Setúbal",
      Código == 295 ~ "Hospital Municipal Pimentas Bonsucesso Manuel de Paiva",
      Código == 296 ~ "Hospital Geral de Pedreira",
      Código == 297 ~ "Hospital Geral do Grajau Prof. Liber John Alphonse Di Dio",
      Código == 298 ~ "Hospital Maternidade Interlagos",
      Código == 299 ~ "Centro de Referência da Saúde da Mulher de Ribeirão Preto - Mater",
      Código == 300 ~ "Hospital e Maternidade Municipal de São Vicente",
      Código == 301 ~ "Hospital Geral Pirajussara Taboão da Serra",
      Código == 302 ~ "Hospital Municipal Universitário de Taubaté",
      Código == 303 ~ "Hospital Municipal José Rabello de Mello",
      Código == 304 ~ "Maternidade Municipal",
      Código == 305 ~ "Hospital Sofia Feldman",
      Código == 306 ~ "Hospital Nossa Senhora Auxiliadora",
      Código == 307 ~ "Hospital Marcio Cunha",
      Código == 308 ~ "Hospital Manoel Gonçalves",
      Código == 309 ~ "Hospital Nossa Senhora Das Graças",
      Código == 310 ~ "Promatre",
      Código == 311 ~ "Hospital Regional Darcy Vargas",
      Código == 312 ~ "Hospital Dos Fornecedores de Cana de Piracicaba",
      Código == 313 ~ "Maternidade De Campinas",
      Código == 314 ~ "Hospital Santo Amaro",
      Código == 315 ~ "Hospital Maternidade Jesus, José e Maria",
      Código == 316 ~ "Hospital Augusto de Oliveira Camargo",
      Código == 317 ~ "Santa Casa de Itu",
      Código == 318 ~ "Santa Casa de Misericórdia de Tatuí",
      Código == 319 ~ "Santa Casa de Ribeirão Preto",
      Código == 320 ~ "Hospital das Clínicas FAEPA",
      Código == 321 ~ "Irmandade da Santa Casa de Misericórdia de Rio Claro",
      Código == 322 ~ "Santa Casa de Misericórdia de São Jose Dos Campos",
      Código == 323 ~ "Hospital São Luiz Gonzaga",
      Código == 324 ~ "Hospital São Luiz de Araras",
      Código == 325 ~ "Hospital de Clínicas de São Sebastião",
      Código == 326 ~ "Hospital Santa Lucinda Sorocaba",
      Código == 327 ~ "Santa Casa de Itaguará",
      Código == 328 ~ "Santa Casa São Bento do Sapucaí",
      Código == 329 ~ "Santa Casa de Guararema",
      Código == 330 ~ "Hospital Belo Horizonte",
      Código == 331 ~ "Núcleo de Especialidade e Diagnóstico",
      Código == 332 ~ "Maternidade Santa Fé",
      Código == 333 ~ "Maternidade Unimed - Unidade Grajaú",
      Código == 334 ~ "Hospital Unimed - Unidade Betim",
      Código == 335 ~ "Nova Lima Hospital Vila da Serra",
      Código == 336 ~ "Vitoria Apart Hospital S.A.",
      Código == 337 ~ "Vila Velha Hospital",
      Código == 338 ~ "Praia da Costa Hospital e Maternidade",
      Código == 339 ~ "Maternidade Santa Úrsula",
      Código == 340 ~ "Hospital Daniel Lipp",
      Código == 341 ~ "Clissil - Clínica São Silvestre",
      Código == 342 ~ "Complexo Hospitalar de Niterói",
      Código == 343 ~ "Hospital Caxias D’Or",
      Código == 344 ~ "Maternidade São Francisco",
      Código == 345 ~ "Casa De Saúde Paracambi",
      Código == 346 ~ "Casa De Saúde Santa Lucia",
      Código == 347 ~ "Hospital Oeste D’Or",
      Código == 348 ~ "Perinatal Laranjeiras Casa de Saúde Laranjeiras",
      Código == 349 ~ "Hospital São Matheus",
      Código == 350 ~ "Perinatal Barra Casa de Saúde Laranjeiras",
      Código == 351 ~ "Hospital Cemeru",
      Código == 352 ~ "Hospital Sino-Brasileiro",
      Código == 353 ~ "Hospital Unimed Americana",
      Código == 354 ~ "Hospital e Maternidade Ipiranga Arujá",
      Código == 355 ~ "Mogi Mater Hospital e Maternidade",
      Código == 356 ~ "Unidade Hospitalar Geral e Maternidade Madre Maria Theodora",
      Código == 357 ~ "Complexo Hospitalar Oito De Dezembro Unidade II",
      Código == 358 ~ "Hospital Carlos Chagas",
      Código == 359 ~ "Hospital Mat Vidas",
      Código == 360 ~ "Hospital E Maternidade Unimed de Piracicaba",
      Código == 361 ~ "Hosp Alpha Med",
      Código == 362 ~ "Rede D’Or São Luiz S. A. Hospital Ribeirão Pires",
      Código == 363 ~ "Hospital e Maternidade Policlin Taubaté",
      Código == 364 ~ "Hospital Santa Maria de Suzano",
      Código == 365 ~ "Hospital e Maternidade Brasil",
      Código == 366 ~ "Hospital e Maternidade do Braz",
      Código == 367 ~ "Hospital São Lucas de Santos",
      Código == 368 ~ "Santa Helena Assistência Médica",
      Código == 369 ~ "Hospital São Luiz Anália Franco",
      Código == 370 ~ "Hospital Maternidade São Cristóvão",
      Código == 371 ~ "Hospital Metropolitano",
      Código == 372 ~ "Hospital e Maternidade Dr. Christovão da Gama",
      Código == 373 ~ "Hospital da Luz",
      Código == 374 ~ "Hospital e Maternidade Santa Joana",
      Código == 375 ~ "Hospital Israelita Albert Einstein",
      Código == 376 ~ "Hospital Bom Clima",
      Código == 377 ~ "Hospital Nipo Brasileiro",
      Código == 378 ~ "Hospital Sepaco",
      Código == 379 ~ "Hospital Vitória",
      Código == 380 ~ "Hospital e Maternidade Nossa Senhora do Rosário",
      Código == 381 ~ "Pro Matre Paulista",
      Código == 382 ~ "São Luiz Unidade Itaim",
      Código == 383 ~ "Hospital Central da Aeronáutica HCA",
      Código == 384 ~ "Hospital Santa Tereza",
      Código == 385 ~ "Hospital Municipal",
      Código == 386 ~ "Hospital Municipal DE Januária",
      Código == 387 ~ "Hospital Público Municipal de Macaé",
      Código == 388 ~ "Hospital Estadual dos Lagos Nossa Senhora de Nazareth – SES RJ",
      Código == 389 ~ "Hospital São João Batista",
      Código == 390 ~ "Hospital Regional de Assis",
      Código == 391 ~ "Maternidade Santa Isabel",
      Código == 392 ~ "Hospital das Clínicas Unidade Materno Infantil",
      Código == 393 ~ "Hospital de Clínicas da UFTM",
      Código == 394 ~ "Hospital Municipal Victor de Souza Breves",
      Código == 395 ~ "Hospital Municipal de Itapira",
      Código == 396 ~ "Hospital São Vicente de Paulo de Águas Formosas",
      Código == 397 ~ "Santa Casa de Misericórdia de Araguari",
      Código == 398 ~ "Fundação Hospitalar São Vicente de Paulo",
      Código == 399 ~ "Santa Casa De Misericórdia de Juiz de Fora",
      Código == 400 ~ "Hospital César Leite",
      Código == 401 ~ "Hospital Deraldo Guimaraes",
      Código == 402 ~ "Hospital Santa Casa de Montes Claros",
      Código == 403 ~ "Hospital Dr. Moisés Magalhães Freire",
      Código == 404 ~ "Hospital Queluz",
      Código == 405 ~ "Irmandade Santa Casa de Misericórdia Cataguases",
      Código == 406 ~ "Hospital Santo Antônio",
      Código == 407 ~ "Clínicas Integradas Hospital Universitário Mario Palmério",
      Código == 408 ~ "Hospital São José",
      Código == 409 ~ "Hospital Rio Doce",
      Código == 410 ~ "Hospital Madre Regina Protmann",
      Código == 411 ~ "Hospital Dos Plantadores De Cana",
      Código == 412 ~ "Beneficência Portuguesa De Teresópolis",
      Código == 413 ~ "Santa Casa de Barretos",
      Código == 414 ~ "Hospital Padre Albino Catanduva",
      Código == 415 ~ "Hospital Nossa Senhora da Piedade",
      Código == 416 ~ "Hospital Universitário de Marilia",
      Código == 417 ~ "Hospital Carlos Fernando Malzoni Matão",
      Código == 418 ~ "Santa Casa de Mogi Guaçu",
      Código == 419 ~ "Santa Casa de Ourinhos",
      Código == 420 ~ "Santa Casa de São Carlos",
      Código == 421 ~ "Santa Casa de Assis",
      Código == 422 ~ "Hospital Vaz Monteiro",
      Código == 423 ~ "Hospital São Vicente Turmalina",
      Código == 424 ~ "Hospital Monte Sinai",
      Código == 425 ~ "Hospital Santa Genoveva",
      Código == 426 ~ "Hospital E Maternidade Santa Clara",
      Código == 427 ~ "Hospital Unimed",
      Código == 428 ~ "Hospital Unimed Aracatuba",
      Código == 429 ~ "Hospital Geral Iamada Pres. Prudente",
      Código == 430 ~ "Hospital São Paulo - Araraquara",
      Código == 431 ~ "Hospital Unimed De Nova Friburgo",
      Código == 432 ~ "Clínica Cirúrgica E Maternidade Lilia Neves",
      Código == 433 ~ "Hospital Infante D. Henrique",
      Código == 434 ~ "IMESA",
      Código == 435 ~ "Hospital São Jorge",
      Código == 436 ~ "Hospital Universitário Do Oeste Do Paraná",
      Código == 437 ~ "Hospital E Maternidade Municipal De São Jose Dos Pinhais",
      Código == 438 ~ "Maternidade Municipal Lucilla Ballallai",
      Código == 439 ~ "Hospital Municipal Ruth Cardoso",
      Código == 440 ~ "Maternidade Darcy Vargas",
      Código == 441 ~ "Hospital Geral E Maternidade Tereza Ramos",
      Código == 442 ~ "Hospital Regional De São Jose Dr. Homero Miranda Gomes",
      Código == 443 ~ "Fundação de Saúde Pública de Novo Hamburgo",
      Código == 444 ~ "Hospital Centenário",
      Código == 445 ~ "Hospital Tramandaí",
      Código == 446 ~ "Hospital Municipal e Regional Sagrado Coração De Jesus Nova Esperança",
      Código == 447 ~ "Hospital Monsenhor Jose Locks",
      Código == 448 ~ "Santa Casa De Misericórdia De Cambe",
      Código == 449 ~ "Hospital Nossa Sra. das Graças Maternidade Mater Dei",
      Código == 450 ~ "Hospital Da Providência Materno Infantil",
      Código == 451 ~ "Hospital E Maternidade Maria Auxiliadora",
      Código == 452 ~ "Hoesp",
      Código == 453 ~ "Hospital Azambuja",
      Código == 454 ~ "Hospital Maicé",
      Código == 455 ~ "Hospital São Francisco",
      Código == 456 ~ "Hospital E Maternidade Marieta Konder Bornhausen",
      Código == 457 ~ "Hospital Bom Jesus",
      Código == 458 ~ "Hospital Regional Alto Vale",
      Código == 459 ~ "Hospital Alvorada",
      Código == 460 ~ "Associação Beneficente São Francisco De Assis - Umuarama",
      Código == 461 ~ "Hospital Universitário São Francisco De Paula - UCPEL",
      Código == 462 ~ "Hospital de Clínicas",
      Código == 463 ~ "Hospital Femina",
      Código == 464 ~ "Hospital Nossa Senhora da Conceição",
      Código == 465 ~ "Hospital Universitário Dr. Miguel Riet Correa Jr.",
      Código == 466 ~ "Hospital Sapiranga",
      Código == 467 ~ "Hospital da Fundação",
      Código == 468 ~ "Associação Fraiburguense de Saúde Coletiva",
      Código == 469 ~ "Hospital Fátima",
      Código == 470 ~ "Centro Médico-Hospitalar Gênesis",
      Código == 471 ~ "Hospital Rondon - Marechal Cândido Rondon",
      Código == 472 ~ "Hospital e Maternidade São Marcos de Maringá",
      Código == 473 ~ "Hospital do Círculo - Caxias Do Sul",
      Código == 474 ~ "Maternidade Curitiba",
      Código == 475 ~ "Hospital Paraná",
      Código == 476 ~ "Hospital Santa Catarina - Blumenau",
      Código == 477 ~ "Centro Hospitalar Unimed - Joinville",
      Código == 478 ~ "Hospital Unimed Chapecó",
      Código == 479 ~ "Hospital Unimed Criciúma",
      Código == 480 ~ "Clínica Santa Helena",
      Código == 481 ~ "Hospital Dona Helena",
      Código == 482 ~ "Hospital de Caridade São Jeronimo",
      Código == 483 ~ "Hospital Unimed Vale do Cai - Montenegro",
      Código == 484 ~ "Hospital Divina Providencia",
      Código == 485 ~ "Hospital Mae de Deus",
      Código == 486 ~ "Hospital Moinhos de Vento",
      Código == 487 ~ "Socimed - Tubarão",
      Código == 488 ~ "Hospital São Jose de Dois Irmãos",
      Código == 489 ~ "Hospital Regional do Litoral",
      Código == 490 ~ "Hospital Universitário Regional dos Campos Gerais",
      Código == 491 ~ "Hospital Universitário de Santa Maria",
      Código == 492 ~ "Hospital Nossa Senhora dos Navegantes",
      Código == 493 ~ "Hospital Municipal Cristo Redentor",
      Código == 494 ~ "Hospital Ministro Costa Cavalcanti",
      Código == 495 ~ "Santa Casa de Misericórdia de Ponta Grossa",
      Código == 496 ~ "Instituto Dr. Feitosa",
      Código == 497 ~ "Hospital Comunitário de Carazinho",
      Código == 498 ~ "Hospital São Luiz Gonzaga",
      Código == 499 ~ "Hospital Bruno Born",
      Código == 500 ~ "Hospital Cristo Redentor Marau",
      Código == 501 ~ "Hospital São Vicente de Paulo",
      Código == 502 ~ "Hospital Vida Saúde",
      Código == 503 ~ "Hospital Santa Cruz",
      Código == 504 ~ "Hospital Caridade Três Passos",
      Código == 505 ~ "Hospital e Maternidade Santa Clara",
      Código == 506 ~ "Hospital Ouro Branco",
      Código == 507 ~ "Policlínica São Vicente de Paula - Francisco Beltrão",
      Código == 508 ~ "Hospital De Caridade de Erechim",
      Código == 509 ~ "Hospital Unimed Noroeste - RS",
      Código == 510 ~ "Clínica São Paulo",
      Código == 511 ~ "Hospital Regional Santa Lucia",
      Código == 512 ~ "Hospital Estadual da Mulher",
      Código == 513 ~ "Maternidade Municipal Aristina Cândida",
      Código == 514 ~ "Maternidade Nascer Cidadão",
      Código == 515 ~ "Maternidade Nossa Senhora de Lourdes",
      Código == 516 ~ "Hospital Regional de Ceilândia",
      Código == 517 ~ "Hospital Regional do Gama",
      Código == 518 ~ "Hospital da Região Leste",
      Código == 519 ~ "Hospital Regional de Samambaia",
      Código == 520 ~ "Hospital Regional de Planaltina",
      Código == 521 ~ "Hospital e Pronto Socorro Municipal de Várzea Grande",
      Código == 522 ~ "Casa de Parto de São Sebastião",
      Código == 523 ~ "Maternidade Cândido Mariano",
      Código == 524 ~ "Hospital Universitário Maria Aparecida Pedrossian",
      Código == 525 ~ "Associação Beneficente Santa Casa de Campo Grande",
      Código == 526 ~ "Hospital Geral de Cuiabá",
      Código == 527 ~ "Hospital Santa Helena",
      Código == 528 ~ "Hospital Municipal Irma Fany Duran Goianésia",
      Código == 529 ~ "Hospital Buriti",
      Código == 530 ~ "Hospital Samaritano de Mineiros",
      Código == 531 ~ "Hospital Municipal de Formosa",
      Código == 532 ~ "Hospital Santa Rosa",
      Código == 533 ~ "Amparo Maternidade",
      Código == 534 ~ "Hospital Santa Rita",
      Código == 535 ~ "Instituto São Vicente de Paulo - Hospital Luciano Chaves",
      Código == 536 ~ "Hospital Santa Luzia",
      Código == 537 ~ "Hospital Nossa Senhora Aparecida Valparaiso",
      Código == 538 ~ "Hospital Santa Lucia Gama",
      Código == 539 ~ "Hospital Santa Helena",
      Código == 540 ~ "Hospital Santa Lucia",
      Código == 541 ~ "Hospital Santa Marta",
      Código == 542 ~ "Hospital São Francisco",
      Código == 543 ~ "Maternidade Brasília",
      Código == 544 ~ "Hospital Santa Rita - Várzea Grande",
      Código == 545 ~ "Hospital São Domingos",
      Código == 546 ~ "Hospital Universitário da UFGD - Dourados",
      Código == 547 ~ "Hospital Regional de Nova Andradina",
      Código == 548 ~ "Hospital Coração de Jesus/Asas",
      Código == 549 ~ "Hospital Municipal de Juína",
      Código == 550 ~ "Hospital Municipal De Ivinhema",
      Código == 551 ~ "Hospital Municipal Dr. Daércio Oliveira Moraes",
      Código == 552 ~ "Hospital Soriano Correa da Silva",
      Código == 553 ~ "Hospital Nossa Senhora Auxiliadora",
      Código == 554 ~ "Hospital e Maternidade Santa Ângela",
      Código == 555 ~ "Santa Casa de Misericórdia e Maternidade de Rondonópolis",
      Código == 556 ~ "Hospital Santo Antônio",
      Código == 557 ~ "Hospital das Clínicas Vida e Saúde",
      Código == 558 ~ "Santa Casa de Misericórdia de Anápolis",
      Código == 559 ~ "Hospital João Bigaton",
      Código == 560 ~ "Hospital e Maternidade Renato Sucupira Sapezal",
      Código == 561 ~ "Hospital CASSEMS Unidade Três Lagoas",
      Código == 562 ~ "Hospital Santa Terezinha",
      Código == 563 ~ "Clínica e Maternidade Modelo",
      Código == 564 ~ "Clínica Infantil Menino Jesus",
      Código == 565 ~ "Hospital Unimed Rio Verde"
    ),
    Semana <- ifelse(!is.na(Data1) & Data1 != "" & !is.na(as.Date(Data1)),
                     paste(as.numeric(round(difftime(as.Date(Data1), 
                                                            as.Date("2021-10-17"), units = "weeks")) + 1)), NA),
    Data1 = ifelse(!is.na(Data1) & Data1 != "" & !is.na(as.Date(Data1)), Data1, NA),
    Data2 = ifelse(!is.na(Data2) & Data2 != "" & !is.na(as.Date(Data2)), Data2, NA), 
    ent = ifelse(!is.na(Data1) & Data1 != "" & !is.na(as.Date(Data1)), 1, 0),
    pront = ifelse(!is.na(Data2) & Data2 != "" & !is.na(as.Date(Data2)), 1, 0))


dados1 <- dados %>% 
  
  group_by(País, Macrorregião, Estado, Código, Hospital) %>%
  summarize(ent1 = min(Data1, na.rm = TRUE),
            ent2 = max(Data1, na.rm = TRUE),
            pront1 = min(Data2, na.rm = TRUE),
            pront2 = max(Data2, na.rm = TRUE),
            ent = sum(ent),
            pront = sum(pront), 
            amostra = amostra) %>% slice(1)


dados2 <- dados %>% 
  
  group_by(País, Macrorregião, Estado) %>%
  summarize(ent1 = min(Data1, na.rm = TRUE),
            ent2 = max(Data1, na.rm = TRUE),
            pront1 = min(Data2, na.rm = TRUE),
            pront2 = max(Data2, na.rm = TRUE),
            ent = sum(ent),
            pront = sum(pront)) 

append <- dados1 %>% 
  
  group_by(Estado) %>%
  summarize(amostra = sum(amostra))

dados2 <- merge(dados2, append, by = "Estado", all = TRUE)

# Define a UI do aplicativo Shiny
ui <- fluidPage(
  navbarPage(
    "",
    tabPanel("Dados",
             fluidRow(
               column(
                 width = 12,
                 selectInput("estadoInput", "Estado", choices = c("Todos", sort(unique(dados$Estado)))),
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 style = "overflow-x: auto;",
                 dataTableOutput("tabelaDados")
               )
             )
    ),
    tabPanel("Visualização",
             fluidRow(
               column(
                 width = 12,
                 selectInput("estadoInputVis", "Estado", choices = c("Todos", sort(unique(dados$Estado)))),
               )
             ),
             fluidRow(
               column(
                 width = 6,
                 verbatimTextOutput("entrevistasOutput")
               ),
               column(
                 width = 6,
                 verbatimTextOutput("prontuariosOutput")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 style = "overflow-x: auto;",
                 dataTableOutput("tabelaDados1")
               )
             ), 
             fluidRow(
               column(
                 width = 12,
                 style = "overflow-x: auto;",
                 dataTableOutput("tabelaSemanas")
               )
             )
    )
  )
)

server <- function(input, output) {
  

  criarTabelaDados <- function(dadosFiltrados) {

    if (input$estadoInput != "Todos") {
      dadosFiltrados <- dados1 %>% filter(Estado == input$estadoInput)    }
    
    
  
    formatarData <- function(data) {
      ifelse(is.na(data), "", format(data, "%d/%m/%Y"))
    }

    # Cria a tabela de dados filtrados
    tabela <- data.frame(
      Código = dadosFiltrados$Código,
      Hospital = dadosFiltrados$Hospital,
      Entrevistas = dadosFiltrados$ent,
      "Início Ent." = dadosFiltrados$ent1,
      "Fim Ent." = dadosFiltrados$ent2,
      Prontuários = dadosFiltrados$pront,
      "Início Pront." = dadosFiltrados$pront1,
      "Fim Pront." = dadosFiltrados$pront2,
      stringsAsFactors = FALSE
    )
    
    return(tabela)
  }
  
   output$tabelaDados <- renderDataTable({
    tabela <- criarTabelaDados(dados1)
    tabela
  })
  
   
   criarTabelaDados1 <- function(dadosFiltrados1) {
     
     if (input$estadoInputVis != "Todos") {
       dadosFiltrados1 <- dados1 %>% filter(Estado == input$estadoInputVis)
     }
   }
  
  criarTabelaDados2 <- function(dadosFiltrados2) {
    
    if (input$estadoInputVis != "Todos") {
      dadosFiltrados2 <- dados2 %>% filter(Estado == input$estadoInputVis)
    }
  }


  output$entrevistasOutput <- renderPrint({
  
    dadosFiltrados1 <- criarTabelaDados1(dados1)
    dadosFiltrados2 <- criarTabelaDados2(dados2)
    
    total_hospitais <- nrow(dadosFiltrados1)
    hospitais_com_ent <- sum(dadosFiltrados1$ent > 0)
    hospitais_com_pront <- sum(dadosFiltrados1$pront > 0)
    total_entrevistas <- sum(dadosFiltrados2$ent)
    total_ent_planejadas <- dadosFiltrados2$amostra
    total_prontuarios <- sum(dadosFiltrados2$pront)
    total_pront_planejados <- dadosFiltrados2$amostra
    total_semanas_ent <- ceiling(as.numeric(difftime(max(dadosFiltrados2$ent2), min(dadosFiltrados2$ent1), units = "weeks")))
    total_semanas_pront <- ceiling(as.numeric(difftime(max(dadosFiltrados2$pront2), min(dadosFiltrados2$pront1), units = "weeks")))
    total_dias_ent <- as.numeric(difftime(max(dadosFiltrados2$ent2), min(dadosFiltrados2$ent1), units = "days"))
    total_dias_pront <- as.numeric(difftime(max(dadosFiltrados2$pront2), min(dadosFiltrados2$pront1), units = "days"))
    
    cat("Entrevistas:\n")
    cat("Início:", min(dadosFiltrados2$ent1), "\n")
    cat("Fim:", max(dadosFiltrados2$ent2), "\n")
    cat("Hospitais:", total_hospitais, "\n")
    cat("Em campo:", hospitais_com_ent, "\n")
    cat("Percentual de hospitais com entrevistas: ", (hospitais_com_ent / total_hospitais) * 100, "%\n")
    cat("Total de entrevistas concluídas: ", total_entrevistas, "\n")
    cat("Total de entrevistas planejadas: ", total_ent_planejadas, "\n")
    cat("Percentual de entrevistas: ", (total_entrevistas / dadosFiltrados2$amostra) * 100, "%\n")
    cat("Total de semanas de coleta: ", total_semanas_ent, "\n")
    cat("Total de dias de coleta: ", total_dias_ent, "\n")

    
  })
  

  output$prontuariosOutput <- renderPrint({
 
    dadosFiltrados1 <- criarTabelaDados1(dados1)
    dadosFiltrados2 <- criarTabelaDados2(dados2)
    
    total_hospitais <- nrow(dadosFiltrados1)
    hospitais_com_ent <- sum(dadosFiltrados1$ent > 0)
    hospitais_com_pront <- sum(dadosFiltrados1$pront > 0)
    total_entrevistas <- sum(dadosFiltrados2$ent)
    total_ent_planejadas <- dadosFiltrados2$amostra
    total_prontuarios <- sum(dadosFiltrados2$pront)
    total_pront_planejados <- dadosFiltrados2$amostra
    total_semanas_ent <- ceiling(as.numeric(difftime(max(dadosFiltrados2$ent2), min(dadosFiltrados2$ent1), units = "weeks")))
    total_semanas_pront <- ceiling(as.numeric(difftime(max(dadosFiltrados2$pront2), min(dadosFiltrados2$pront1), units = "weeks")))
    total_dias_ent <- as.numeric(difftime(max(dadosFiltrados2$ent2), min(dadosFiltrados2$ent1), units = "days"))
    total_dias_pront <- as.numeric(difftime(max(dadosFiltrados2$pront2), min(dadosFiltrados2$pront1), units = "days"))
    
    cat("Prontuários:\n")
    cat("Início:", min(dadosFiltrados2$pront1), "\n")
    cat("Fim:", max(dadosFiltrados2$pront2), "\n")
    cat("Hospitais:", total_hospitais, "\n")
    cat("Em campo:", hospitais_com_ent, "\n")
    cat("Percentual de hospitais com prontuários: ", (hospitais_com_pront / total_hospitais) * 100, "%\n")
    cat("Total de prontuários concluídos: ", total_prontuarios, "\n")
    cat("Total de prontuários planejados: ", total_pront_planejados, "\n")
    cat("Percentual de prontuários: ", (total_prontuarios / dadosFiltrados2$amostra) * 100, "%\n")
    cat("Total de semanas de coleta: ", total_semanas_pront, "\n")
    cat("Total de dias de coleta: ", total_dias_pront, "\n")
    

  })
  
  
  output$tabelaDados1 <- DT::renderDataTable({
    
    dadosFiltrados1 <- criarTabelaDados1(dados1)
    dadosFiltrados1 <- dadosFiltrados1[dadosFiltrados1$ent > 0 | dadosFiltrados1$pront > 0, ]
    
    dadosFiltrados2 <- criarTabelaDados2(dados2)
    
    dadosFiltrados1$s_ent <- ceiling(as.numeric(difftime(dadosFiltrados1$ent2, dadosFiltrados1$ent1, units = "weeks")))
    dadosFiltrados1$s_pront <- ceiling(as.numeric(difftime(dadosFiltrados1$pront2, dadosFiltrados1$pront1, units = "weeks")))
    dadosFiltrados1$d_ent <- as.numeric(difftime(dadosFiltrados1$ent2, dadosFiltrados1$ent1, units = "days"))
    dadosFiltrados1$d_pront <- as.numeric(difftime(dadosFiltrados1$pront2, dadosFiltrados1$pront1, units = "days"))
    
    N1 <- c(dadosFiltrados1$ent)
    N2 <- c(dadosFiltrados1$pront)
    
    N <- vector(length = length(N1) + length(N2))
    
    for (i in 1:length(N1)) {
      N[(2*i)-1] <- N1[i]
      N[2*i] <- N2[i]
    }
    
    A1 <- c(dadosFiltrados1$ent1)
    A2 <- c(dadosFiltrados1$pront1)
    
    A <- vector(length = length(A1) + length(A2))
    
    for (i in 1:length(A1)) {
      A[(2*i)-1] <- A1[i]
      A[2*i] <- A2[i]
    }
    
    B1 <- c(dadosFiltrados1$ent2)
    B2 <- c(dadosFiltrados1$pront2)
    
    B <- vector(length = length(B1) + length(B2))
    
    for (i in 1:length(B1)) {
      B[(2*i)-1] <- B1[i]
      B[2*i] <- B2[i]
    }

    C1 <- c(dadosFiltrados1$s_ent)
    C2 <- c(dadosFiltrados1$s_pront)
    
    C <- vector(length = length(C1) + length(C2))
    
    for (i in 1:length(C1)) {
      C[(2*i)-1] <- C1[i]
      C[2*i] <- C2[i]
    }

    D1 <- c(dadosFiltrados1$d_ent)
    D2 <- c(dadosFiltrados1$d_pront)
    
    D <- vector(length = length(D1) + length(D2))
    
    for (i in 1:length(D1)) {
      D[(2*i)-1] <- D1[i]
      D[2*i] <- D2[i]
    }
    
    
    tabela1 <- data.frame(
      Código = rep(dadosFiltrados1$Código, 2),
      Hospital = rep(dadosFiltrados1$Hospital, 2),
      "Instrumento" = c("Entrevistas", "Prontuários"),
      "n" = N,
      "Início" = A,
      "Fim" = B,
      "Semanas" = C,
      "Dias" = D,
      stringsAsFactors = FALSE
    ) 

    DT::datatable(
      tabela1,
      rownames = FALSE,
      options = list(
        columnDefs = list(
          list(
            className = "dt-center",
            targets = "_all"
          ),
          list(
            targets = c(0, 1),
            createdCell = JS(
              "function (td, cellData, rowData, row, col) {",
              "  if (row % 2 === 0) {",
              "    $(td).attr('rowspan', 2);",
              "    $(td).text(rowData[col]);",
              "  } else {",
              "    $(td).remove();",
              "  }",
              "}"
            )
          )
        )
      )
    )
  })
  
        output$tabelaSemanas <- renderDataTable({
        
        dadosFiltrados1 <- criarTabelaDados1(dados1)
        dadosFiltrados1 <- dadosFiltrados1[dadosFiltrados1$ent > 0 | dadosFiltrados1$pront > 0, ]
        
        menorData <- min(subset(c(dadosFiltrados1$ent1, dadosFiltrados1$pront1), c(dadosFiltrados1$ent1, dadosFiltrados1$pront1) > 0))
        maiorData <- max(subset(c(dadosFiltrados1$ent2, dadosFiltrados1$pront2), c(dadosFiltrados1$ent2, dadosFiltrados1$pront2) > 0))
        semanas_inicio <- floor(as.numeric(difftime(menorData, as.Date("2021-10-17"), units = "weeks"))) + 1
        calc <- ceiling(as.numeric(difftime(maiorData, menorData, units = "weeks")))
        semanas_fim <- semanas_inicio + calc - 1
        
        tabela2 <- data.frame(
          Código = rep(dadosFiltrados1$Código, each = 2),
          Hospital = rep(dadosFiltrados1$Hospital, each = 2),
          "Instrumento" = rep(c("Entrevistas", "Prontuários"), nrow(dadosFiltrados1)),
          stringsAsFactors = FALSE
        )
        
        semanas <- seq(semanas_inicio, semanas_fim)
        
        for (i in semanas) {
          tabela2[[as.character(i)]] <- ""
        }
        
        for (i in 1:nrow(dadosFiltrados1)) {
          hospital <- dadosFiltrados1$Hospital[i]
          sem_ini_hosp_ent <- floor(as.numeric(difftime(dadosFiltrados1$ent1[i], as.Date("2021-10-17"), units = "weeks"))) + 1
          sem_fim_hosp_ent <- ceiling(as.numeric(difftime(dadosFiltrados1$ent2[i], as.Date("2021-10-17"), units = "weeks"))) + 1
          sem_ini_hosp_pront <- floor(as.numeric(difftime(dadosFiltrados1$pront1[i], as.Date("2021-10-17"), units = "weeks"))) + 1
          sem_fim_hosp_pront <- ceiling(as.numeric(difftime(dadosFiltrados1$pront2[i], as.Date("2021-10-17"), units = "weeks"))) + 1
          
          semanas_ent <- seq(sem_ini_hosp_ent, sem_fim_hosp_ent)
          semanas_pront <- seq(sem_ini_hosp_pront, sem_fim_hosp_pront)
          
          # Lógica de coloração
          for (j in semanas_ent) {
            tabela2[(i - 1) * 2 + 1, as.character(j)] <- "green"
          }
          
          for (j in semanas_pront) {
            tabela2[(i - 1) * 2 + 2, as.character(j)] <- "yellow"
          }
        }
        
        datatable(
          tabela2,
          rownames = FALSE,
          options = list(
            columnDefs = list(
              list(
                className = "dt-center",
                targets = "_all"
              ),
              list(
                targets = c(0, 1),
                createdCell = JS(
                  "function (td, cellData, rowData, row, col) {",
                  "  if (row % 2 === 0) {",
                  "    $(td).attr('rowspan', 2);",
                  "    $(td).text(rowData[col]);",
                  "  } else {",
                  "    $(td).remove();",
                  "  }",
                  "}"
                )
              )
            )
          ),
          callback = JS(
            "table.on('draw.dt', function () {",
            "  var table = $(this).DataTable();",
            "  table.cells().every(function () {",
            "    var cell = this;",
            "    var data = cell.data();",
            "    if (data === 'green') {",
            "      $(cell.node()).css({'background-color': 'green', 'color': 'green'});",
            "    } else if (data === 'yellow') {",
            "      $(cell.node()).css({'background-color': 'yellow', 'color': 'yellow'});",
            "    }",
            "  });",
            "});"
          )
        )
      })
      
}
  
shinyApp(ui, server)