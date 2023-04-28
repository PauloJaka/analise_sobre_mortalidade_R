#https://datasus.saude.gov.br/transferencia-de-arquivos/
  # depois instalei o pacote install.packages("read.dbc") para ler o arquivo
  #usei os dados da sergipe, do ano de 2019
  
  library(tidyverse)

base <- read.dbc::read.dbc("C:/Users/Paulo/Downloads/base2019.dbc")
#lembrar de pegar so os dados de SE

#"descodifiacando" alguns dados, usei de referencia a Estrutura_SIM_Anterior.pdf
# que baixei no mesmo lugar
attach(base)

#filtrando sergipe na base
baseSE <- base %>%
  filter(NATURAL == '828')

# descodificando a idade
baseSE<- baseSE %>%
  transform(IDADE = as.numeric(as.character(IDADE))) %>%
  mutate(IDADE = case_when(IDADE >= '400' ~ IDADE-400,
                           IDADE <= 0 ~ 0))

#retirando algumas linhas que ficaram bugadas, com idades negativas
#ia ferrar toda parte da media
# so tem dado de 10 bebes que morreram com menos de 6 meses então n irei usar
baseSE <- subset(baseSE, IDADE > '0')

table(SEXO)
#apartir daqui vou começar a tentar plotar alguns graficos com certas informações 
#essa vai ser, se morre mais homens ou mulheres
genero <- tribble(
  ~sexo, ~mortes,
  'Homens', 7646,
  'Mulheres',5817 
)

# Calcular porcentagens
genero$fração <- genero$mortes / sum(genero$mortes)

# Calcula as porcentagens cumulativas (topo de cada retângulo)
genero$ymax <- cumsum(genero$fração)

# Calcula a base de cada retângulo
genero$ymin <- c(0, head(genero$ymax, n=-1))

# Calcula a posição do rótulo
genero$labelPosition <- (genero$ymax + genero$ymin) / 2

# Calcule um bom rótulo
genero$label <- paste0(round(100*genero$mortes/sum(genero$mortes),1), "%\n", genero$sexo)

# Make the plot

ggplot(genero, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=sexo)) +
  geom_rect() +
  geom_text( x=c(0.7, 1.5) , aes(y=labelPosition, label=label, color=sexo), size=c(9, 5.5)) + 
  scale_fill_manual(values = c("#000000","#ea1404")) +
  scale_color_manual(values = c("#000000","#ea1404")) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")

#agora vou fazer pessoas por faixa de idade
#usei esse tipo de codigo pra ir pegando as faixas

teste <- subset(baseSE, IDADE >=81 & IDADE <= 114)
length(teste$IDADE)

#Apenas mudando a faixa de idadee


faixa_de_idade <- tribble(
  ~idades, ~mortes, ~ordem,
  '1 a 2 anos', 40, 1,
  '3 a 12 anos', 85, 2,
  '13 a 17 anos', 117, 3,
  '18 a 30 anos', 839, 4,
  '31 a 50 anos', 1508, 5,
  '51 a 65 anos', 2091, 6,
  '66 a 80 anos', 3046, 7,
  '80 anos ou mais', 2868, 8
)

#mortalidade por faixa de idade

faixa_de_idade %>%
  mutate(prop = 100*mortes/sum(mortes)) -> faixa_etaria

ggplot(data = faixa_etaria, mapping = aes(x = reorder(idades, ordem), y = mortes)) +
  geom_col(fill = '#000000') +
  labs(x = '', y = '') +
  coord_flip() +
  geom_text(aes(label = sprintf("%1.1f%%", round(prop, 1))) , nudge_y = 280, 
            color = '#ea1404', size = 5, fontface = "bold") +
  theme_minimal() +
  theme(text=element_text(size = 18))


#causas de mortes
tipo_de_mortes <- tribble(
  ~causas, ~mortes, ~ ordem,
  'homicidio', 847, 1,
  'acidente', 559, 2,
  'suicidio', 94, 3,
  'outros', 97+97 , 4
)
tipo_de_mortes %>%
  mutate(prop_S = 100*mortes/sum(mortes)) -> faixa_etaria_S

ggplot(data = faixa_etaria_S,mapping = aes(y = reorder(causas, -ordem), x = mortes, fill = reorder(causas, mortes))) +
  geom_col() +
  geom_text(aes(label = sprintf("%1.1f%%", round(prop_S, 1))), nudge_y = 0.03, nudge_x = 50,
            color = '#ea1404', size = 4.5, fontface = "bold") +
  labs(x = "", y = '') +
  cowplot::theme_minimal_hgrid(line_size = 1) +
  theme(legend.position = "none")+
  scale_fill_manual(values = c('#000000',
                                        '#000000','#000000','#000000')) +
                                          theme(text=element_text(size = 1))


#FAZENDO AGORA O TIPO DE DOENÇAS QUE MAIS MATAM

quantidadeLinhaA <- table(LINHAA) 
quantidadeLinhaA <- data.frame(quantidadeLinhaA)
quantidadeLinhaA <- quantidadeLinhaA %>%
  mutate(quantidade = sum(Freq))

quantidadeLinhaB <- table(LINHAB) 
quantidadeLinhaB <- data.frame(quantidadeLinhaB)
quantidadeLinhaB <- quantidadeLinhaB %>%
  mutate(quantidade = sum(Freq))

quantidadeLinhaC <- table(LINHAC) 
quantidadeLinhaC <- data.frame(quantidadeLinhaC)
quantidadeLinhaC <- quantidadeLinhaC %>%
  mutate(quantidade = sum(Freq))

quantidadeLinhaD <- table(LINHAD) 
quantidadeLinhaD <- data.frame(quantidadeLinhaD)
quantidadeLinhaD <- quantidadeLinhaD %>%
  mutate(quantidade = sum(Freq))

tipo_de_mortes_por_doencas <- tribble(
  ~causas, ~mortes, ~ ordem,
  'doenças infecciosas e parasitárias', 13098, 1,
  'doenças virais', 9583, 2,
  'Tumores', 6424, 3,
  'doenças de sangue e transtornos imunitários', 2662 , 4
)
tipo_de_mortes_por_doencas %>%
  mutate(prop_S = 100*mortes/sum(mortes)) -> faixa_etaria_S

ggplot(data = faixa_etaria_S,mapping = aes(y = reorder(causas, -ordem), x = mortes, fill = reorder(causas, mortes))) +
  geom_col() +
  geom_text(aes(label = sprintf("%1.1f%%", round(prop_S, 1))), nudge_x = 1000,
            color = '#ea1404', size = 4.5, fontface = "bold") +
  labs(x = "", y = '') +
  cowplot::theme_minimal_hgrid(line_size = 0.5) +
  theme(legend.position = "none")+
  theme(axis.text = element_text(size = 10)) +
  scale_fill_manual(values = c('#000000','#000000','#000000','#000000')) +
  theme(text=element_text(size = 1))

#ESTADO CIVIL


table(baseSE$ESTCIV)

est_civ <- tribble(
  ~est, ~freq,
  'Casado(a)', 2729,
  'Solteiro(a)', 4296,
  'Divorciado(a)', 653,
  'união estável', 407,
  'ignorado', 233,
  'Viúvo(a)', 1983
)

est_civ %>%
  mutate(prop = 100*freq/sum(freq)) -> est_civ

ggplot(data = est_civ,mapping = aes(x = reorder(est, -freq), y = freq, fill = reorder(est, freq))) +
  geom_col() +
  geom_text(aes(label = sprintf("%1.1f%%", round(prop, 1))), nudge_y = 250,
            color = '#ea1404', size = 5, fontface = "bold") +
  labs(x = "", y = '') +
  cowplot::theme_minimal_hgrid(line_size = 0.5) +
  theme(legend.position = "none")+
  scale_fill_manual(values = c('#000000','#000000','#000000','#000000','#000000',"#000000")) +
  theme(text=element_text(size = 18))

# ESCOLARIDADE

table(baseSE$ESC)

escolaridade <- tribble(
  ~est, ~freq, ~ordem, 
  'Jardim incompleto', 3021, 1,
  'Fundamental incompleto', 2121, 2,
  'Médio incompleto', 2673, 3,
  'Médio completo', 1597, 4,
  'Superior ou técnico', 329, 5,
  'ignorado', 396, 6
)

escolaridade %>%
  mutate(prop = 100*freq/sum(freq)) -> escolaridade


ggplot(data = escolaridade,mapping = aes(x = reorder(est, ordem), y = freq, fill = reorder(est, freq))) +
  geom_col() +
  geom_text(aes(label = sprintf("%1.1f%%", round(prop, 1))), nudge_y = 250,
            color = '#ea1404', size = 5, fontface = "bold") +
  labs(x = "", y = '') +
  cowplot::theme_minimal_hgrid(line_size = 0.5) +
  theme(axis.text = element_text(size = 8.5)) +
  theme(legend.position = "none")+
  scale_fill_manual(values = c('#000000','#000000','#000000','#000000','#000000',"#000000")) +
  theme(text=element_text(size = 18))


View(baseSE)
