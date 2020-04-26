#' Create big table for reports
#'
#' @param dep_var
#' @param list_cont
#' @param list_cat
#' @param data
#'
#' @return
#' @import magrittr
#' @import tidyr
#' @import dplyr
#' @export
#'
#' @examples
#' tabelaum(dep_var = "vs",
#' list_cont=c("mpg","cyl"),
#' list_cat = "am",
#' data = mtcars)
#'
#' tabelaum(dep_var = "Species",
#'          list_cont=c(1:4),
#'          data=iris)
#'
tabelaum <- function(dep_var,
                     list_cont=NULL,
                     list_cat=NULL,
                     paired=FALSE,
                     just_expl=FALSE,
                     trunc_p = FALSE,
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
      if(!just_expl){
        if(length(unique(data[[dep_var]]))==2){
          stat$p = round(t.test(x$Valor~x$y, paired = paired)$p.value,4)
        } else {
          if(paired) stop("Não dá pra fazer pareado e ANOVA aqui...")
          stat$p = try(round(anova(lm(x$Valor~x$y))$`Pr(>F)`[1],4))
        }
        if(trunc_p)
          stat$p = as.character(ifelse(stat$p<0.0001, "<0.0001", stat$p))
      }
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
      if(!just_expl){
        p <- try(ifelse(all(tab>4),
                    chisq.test(tab)$p.value,
                    fisher.test(tab)$p.value))
        if(trunc_p){
          p <- try(ifelse(p<0.0001, "<0.0001", round(p,4)))
          p <- as.character(p)
        }
      }
      cat_dd <- cbind(var = x$Var[1],
                      valor = rownames(tab),
                      fancytable(tab))
      cat_dd <- as.data.frame(cat_dd)
      if(!just_expl) cat_dd$p <- p
      cat_dd
    })
    cat_dd <- do.call(rbind,tmp)
  }
  ans <- rbind(cat_dd,cont_dd)
  rownames(ans) <- NULL
#  colnames(ans)[-c(1:2,ncol(ans))] <- levels(as.factor(data[[dep_var]]))
  as.data.frame(ans)
}






