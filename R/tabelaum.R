#' Create big table for reports
#'
#' @param dep_var
#' @param list_cont
#' @param list_cat
#' @param data
#'
#' @return
#' @import tidyverse
#' @export
#'
#' @examples
#' tabelaum(dep_var = "vs",
#' list_cont=c("mpg","cyl"),
#' list_cat = "am",
#' data = mtcars)
#'
tabelaum <- function(dep_var,
                     list_cont=NULL,
                     list_cat=NULL,
                     paired=FALSE,
                     data){
  cat_dd = cont_dd = NULL
  ## continous
  if(!is.null(list_cont)){
    dd1 <- data.frame(y=data[[dep_var]],
                      select(data,list_cont)) %>%
      filter(!is.na(y))    %>%
      gather(Var, Valor, -y) %>%
      split(.$Var)
    tmp <- lapply(dd1,function(x){
      stat = x %>%
        group_by(y) %>%
        summarise(med = mean(Valor, na.rm=TRUE),
                  dp = sd(Valor, na.rm=TRUE)) %>%
        mutate(med_dp = paste(round(med,2),
                              " (",
                              round(dp,2),
                              ")",
                              sep="")) %>%
        select(y,med_dp) %>%
        spread(y,med_dp)
      stat$p = round(t.test(x$Valor~x$y, paired = paired)$p.value,4)
      stat$p = as.character(ifelse(stat$p<0.0001, "<0.0001", stat$p))
      stat <- cbind(valor = "méd (d.p.)",stat)
      stat
    })
    cont_dd <- do.call(rbind, tmp)
    cont_dd <- cbind(var = names(tmp), cont_dd)
  }
  ## cateegories
  if(!is.null(list_cat)){
    dd2 <- data.frame(y=data[[dep_var]],
                      select(data,list_cat)) %>%
      gather(Var, Valor, -y) %>%
      split(.$Var)
    tmp <- lapply(dd2,function(x){
      tab <- with(x,table(Valor, y))
      p <- ifelse(all(tab>4),
                  chisq.test(tab)$p.value,
                  fisher.test(tab)$p.value)
      p <- ifelse(p<0.0001, "<0.0001", round(p,4))
      p <- as.character(p)
      cat_dd <- cbind(var = x$Var[1],
                      valor = rownames(tab),
                      fancytable(tab),
                      p)
      cat_dd
    })
    cat_dd <- do.call(rbind,tmp)
  }
  ans <- rbind(cat_dd,cont_dd)
  rownames(ans) <- NULL
  ans
}






