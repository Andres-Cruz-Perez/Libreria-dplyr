################################################################################
############################### librer韆s ######################################
################################################################################
# install.packages("dplyr")
# install.packages("gapminder")
library(dplyr)      # manipulaci髇 de datos
library(magrittr)
library(gapminder)  # base de datos

# cargar y adjuntar la base de datos gap
data(gapminder)

# duplicar la base de datos
gap <- gapminder

rm(gapminder)

# nombre de las variables
colnames(gap)

# clase de objeto
class(gap)

gap

# ojear algunos registros al azar
sample_n(tbl = gap, size = 5)

# dimensi髇 de los datos
dim(gap)

# numero de registros

n = nrow(gap)

gap %>% nrow() -> n


# n鷐ero de variables

p = ncol(gap)

gap %>% ncol() -> p

# Atajo para operador pipe: Shift + Control + m


# numero de registros completos
sum(complete.cases(gap))

gap %>% 
        complete.cases() %>%
        sum()

media = round(mean(gap$lifeExp),3)

gap$lifeExp %>% mean() %>% round(3) -> media


# clase de variable
sapply(X = gap, FUN = class)

# otra manera
gap %>% sapply(class)

# resumen 
gap %>% summary()

################################################################################
#################### GESTI覰 DE DATOS USANDO dplyr #############################
################################################################################

# Seleccionar y reorganizar variables------------

# ej.
# seleccionar variables
gap %>% 
        select(country, year, lifeExp, gdpPercap) -> gap2 

# Otra manera equivalente de seccionar variables:
variables <- c("country", "year", "lifeExp", "gdpPercap")

gap %>% 
        select(variables) -> gap2

# gap2 = data.frame(country, year, lifeExp, gdpPercap)

# Esta tambi閚 es equivalente:
gap %>% 
        select(all_of(variables)) -> gap2

# any_of solamente selecciona aquellas que encuentre en la base de datos
variables2 <- c("country", "year", "area", "income_level")

gap %>% 
        select(any_of(variables2)) -> gap3

# select funciona con algunas funciones de soporte:
# ej. starts_with()
gap %>% 
        select(starts_with("c")) -> gap4

# ej. ends_with()
gap %>% 
        select(ends_with("p")) -> gap5

# ej. contains()
gap %>% 
        select(contains("nt")) -> gap7

# Una funci髇 cercana a select es relocate:

gap %>% relocate(pop)

gap %>% relocate(starts_with("c"), before = gdpPercap)

# Filtrar observaciones -----

# ej.
# filtrar por un continente especifico.
gap %>%
        filter(continent == "Americas") -> gap11
# ej.
# filtrar por pa韘 Colombia
gap %>% 
        filter(country == "Colombia")
# ej.
# filtrar por paises Colombia o Venezuela
gap %>%
        filter(country == "Colombia" | country == "Venezuela")

# La funci贸n a continuaci贸n es equivalente a la anterior:
gap %>%
        filter(country %in% c("Colombia", "Venezuela"))

#ej. 
# gapminder paises alianza del pacifico
gap %>%
        filter(country %in% c("Chile", "Colombia", "Mexico", "Peru")) ->
        gap_ap

# ej.
# Construir una base de datos con las variables `country`, `year`, `gdpPercap`
# y entonces, filtrar por paises de la Alianza del Pacifico y a帽os en el siglo XXI.
gap %>%
        select(country, year, gdpPercap) %>%
        filter(country %in% c("Chile", "Colombia", "Mexico", "Peru") & year >= 2000) -> gap12

# otra forma
gap %>%
        select(country, year, pop) %>%
        filter(country %in% c("Colombia", "Venezuela")) %>%
        filter(year == 2007)

# ej. 
# filtrar observaciones que est茅n 
# entre 1990 y 2010

# Opci贸n 1:

gap %>% 
        filter(year >= 1990 & year <= 2010)
# Opci贸n 2
gap %>% 
        filter(between(year, 1990, 2010))

# Resumen de variables -----

gap %>% 
        filter(year == 2007) %>% 
        summarise(promedio = mean(gdpPercap))

gap %>% 
        filter(year == 2007) %>% 
        summarise(promedio = mean(gdpPercap), 
                  desviacion = sd(gdpPercap))

# ej.
# agrupar por continente
# y entonces, contar cuantos registros tiene cada continente,
# y entonces, contar cuantos paises distintos hay en cada continente
gap %>%
        group_by(continent) %>%
        summarize(n_observaciones = n(), n_paises = n_distinct(country))

gap %>%
        select(continent, lifeExp, year) %>% 
        group_by(continent) %>%
        filter(year == 2007) %>%
        summarise(prom_lifeExp = mean(lifeExp))

# ej.
# construir una base de datos con las siguientes variables: continent, lifeExp
# y entonces, agrupar por continente
# y entonces, calcular el promedio de lifeExp

gap %>%
        select(continent, lifeExp) %>%
        group_by(continent) %>%
        summarise(min_vida = min(lifeExp), max_vida = max(lifeExp)) -> tab

# ej.
# filtrar por a帽o 2007
# y entonces, agrupar por continente
# y entonces calcular la mediana de lifeExp
gap %>%
        filter(year == 2007) %>%
        group_by(continent) %>%
        summarise(lifeExp = median(lifeExp))

# ej. 
# Aplicar una funci贸n de resumen a varias variables

gap %>% 
        filter(year == 2002) %>%
        group_by(continent) %>%
        summarise(across(c("lifeExp", "pop", "gdpPercap"), median))
        
# Aplicar VARIAS funciones de resumen a VARIAS variables

gap %>% 
        filter(year == 2002) %>%
        group_by(continent) %>%
        summarise(across(c("lifeExp", "pop", "gdpPercap"), 
                         list(mediana = median, 
                              maximo = max)))


# Crear o transformar variables ----

# ej.
# construir una base de datos con las siguientes variables: country, year, gdpPercap
# y entonces, filtrar por pa铆s Estados Unidos
# y entonces, construir la variable logaritmo de gdpPercap
gap %>%
        select(country, year, gdpPercap) %>%
        filter(country == "United States") %>%
        mutate(log_gdpPercap = log(gdpPercap) ) -> gap2

# ej.
# construir una base de datos con las siguientes variables: country, pop
# y entonces, filtrar por paises de Merco Sur
# y entonces, contruir la variable pop en millones
# y entonces, agrupar por pais
# y entonces, calcular el max de popMill
gap %>%
        select(country, pop) %>%
        filter(country %in% c("Argentina", "Brazil", "Paraguay", "Uruguay", "Venezuela")) %>%
        mutate(popMill = pop/1000000) %>%
        group_by(country) %>%
        summarise(max_popMill = max(popMill))


# Organizar observaciones -----

# ej.
# construir una base de datos con las siguientes variables: continent, country, year, gdpPercap
# y entonces, filtrar por continente America y a帽o 2007
# y entonces, organizar ascendentemente por gdpPercap
gap %>%
        select(continent, country, year, gdpPercap) %>%
        filter(continent == "Americas" & year == 2007) %>%
        arrange(gdpPercap)


# ej.
# construir una base de datos con las siguientes variables: continent, country, year, gdpPercap
# y entonces, filtrar por continente America y a帽o 2007
# y entonces, organizar descendentemente por gdpPercap
gap %>%
        select(continent, country, year, gdpPercap) %>%
        filter(continent == "Americas" & year == 2007) %>%
        arrange(desc(gdpPercap))

# Renombrar variables -----

gap %>% 
        rename(a駉 = year) -> gap

gap %>% 
        rename(pais = country, continente = continent) -> gap
