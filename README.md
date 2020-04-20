
<!-- README.md is generated from README.Rmd. Please edit that file -->

![SOS
Estatística](https://sosestatistica.com.br/wp-content/uploads/2018/08/logoSOSnew.png)

Pacote em R com funções utilizadas nos relatórios da consultoria
estatística [SOS Estatística](https://www.sosestatistica.com.br)

<!-- badges: start -->

<!-- badges: end -->

## Instalação

Você pode instalar via [GitHub](https://github.com/) usando:

``` r
# install.packages("devtools")
devtools::install_github("gabrielfranco89/sosestatistica")
```

## Exemplos

Algumas funções úteis que vem com o pacote, como a `fancytable`:

``` r
library(magrittr)
library(knitr)
library(sosestatistica)
#> Registered S3 method overwritten by 'rvest':
#>   method            from
#>   read_xml.response xml2

my_tab <- table(iris$Species, iris$Sepal.Length>5)
fancytable(my_tab) %>% kable()
```

|            | FALSE    | TRUE     |
| ---------- | :------- | :------- |
| setosa     | 28 (56%) | 22 (44%) |
| versicolor | 3 (6%)   | 47 (94%) |
| virginica  | 1 (2%)   | 49 (98%) |
