

create_n_table <- function(my.df, var, covs_v, names_ep_l){
  
  my.df <- my.df %>% 
    filter(!is.na(get(var))) %>%
    group_by_at(var)
  
  ns.n.tmp <- my.df %>% 
    summarise(n=n()) 
  
  ns.all.tmp <- my.df %>%
    summarise_at(covs_v, unlist(names_ep_l), 
                 ~str_glue("{sum(.,na.rm=T)} ({round(mean(., na.rm=T)*100,1)}%)")) %>%
    select(-all_of(var))
  
  ns.p.tmp <- lapply(as.list(c(covs_v, unlist(names_ep_l))), function(x){
    chisq.test(table(select(my.df, all_of(c(var_exp,x)) )))$p.val
  }) %>% 
    unlist %>%
    signif(3)
  
  bind_cols(ns.n.tmp, ns.all.tmp) %>% 
    t() %>%
    as.data.frame() %>%
    setNames(ns.n.tmp[[var]]) %>%
    rownames_to_column(var=var) %>% 
    slice(-1) %>%
    mutate(pval=c(NA_real_, ns.p.tmp))  %>% 
    mutate(pval=if_else(pval < 2e-16, "<2e-16", as.character(pval))) %>%
    rename(`P-value`=pval)
}


create_n_table2 <- function(my.df, var_exp, vars){
  
  my.df <- my.df %>% 
    filter(!is.na(get(var_exp))) %>%
    group_by_at(var_exp)
  
  ns.n.tmp <- my.df %>% 
    summarise(n=n()) 
  
  ns.all.tmp <- my.df %>%
    summarise_at(vars, 
                 ~str_glue("{sum(.,na.rm=T)} ({round(mean(., na.rm=T)*100,1)}%)")) %>%
    select(-all_of(var_exp))
  
  ns.p.tmp <- lapply(as.list(vars), function(x){
    chisq.test(table(select(my.df, all_of(c(var_exp,x)) )))$p.val
  }) %>% 
    unlist %>%
    signif(3)
  
  bind_cols(ns.n.tmp, ns.all.tmp) %>% 
    t() %>%
    as.data.frame() %>%
    setNames(ns.n.tmp[[var_exp]]) %>%
    rownames_to_column(var_exp) %>% 
    slice(-1) %>%
    mutate(pval=c(NA_real_, ns.p.tmp))  %>% 
    mutate(pval=if_else(pval < 2e-16, "<2e-16", as.character(pval))) %>%
    rename(`P-value`=pval)
}


my.kable <- function(my.table, font=16) {
  my.table %>%
    knitr::kable() %>%
    kable_classic() %>%
    row_spec(0,bold=TRUE) #%>%
    #kable_styling(font_size = font)
  
}


#Draws histograms of endpoints by age. Cases and controls are coloured separately-

my_endpoint_histograms <-  function(endpoints_all, endpoint_ages_all, df=df, ncol=4, xlab="age", ylim=NA){

  df_long_ep <-   df %>%
    select("FINNGENID", all_of(endpoints_all)) %>%
    pivot_longer(cols=all_of(endpoints_all), names_to = "endpoint",values_to = "status")
  
  df_long <-   df %>%
    select("FINNGENID", all_of(endpoint_ages_all)) %>%
    pivot_longer(cols=all_of(endpoint_ages_all), names_to = "endpoint",values_to = "age") %>%
    mutate(endpoint = str_replace(endpoint,"(\\w+)_.+", "\\1")) %>%
    left_join(df_long_ep, by=c("FINNGENID", "endpoint"))
  
  ggplot(df_long, aes(x=age, color=as.factor(status), fill=as.factor(status)), xlab = xlab) +
    geom_histogram(alpha=0.25, position="identity") +
    theme(legend.title = element_blank()) +
    facet_wrap(~endpoint, ncol=ncol) +
    xlab(xlab) + 
    coord_cartesian(ylim=c(0,ylim))
    #ylim(0,ylim)
  
}



#Function to extract tables
extr_table <- function(fit, var_select, outcome="Variable"){ 
  cbind(summary(fit)$conf.int, pval = summary(fit)$coefficients[,5]) %>%
    as.data.frame %>%
    rownames_to_column("Variable") %>%
    rename(est ="exp(coef)", low = "lower .95", high="upper .95") %>%
    filter(str_detect(Variable, var_select)) %>%
    mutate_at(c("est","low","high"), round,2) %>%
    mutate_at("pval", signif,2) %>%
    mutate(HR = str_glue("{est} ({low}-{high})")) %>% 
    select(Variable, HR, pval) %>%
    rename(!!outcome := Variable)
}  