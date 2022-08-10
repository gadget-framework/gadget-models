## convenience functions
von_b_formula <- function(a,linf='Linf',k='k',recl='recl'){
  a %>% 
    map(~infuser::infuse("{{linf}} * (1 - exp(-1 * (0.01 * {{k}}) * ({{a}} - (1 + log(1 - {{recl}}/{{linf}})/(0.01 * {{k}})))))",
                         a=.,linf=linf,k=k,recl=recl)) %>% 
    map(~parse(text=.) %>% 
          map(to.gadget.formulae)) %>% 
    unlist()
}

init_guess <- function(dat,pattern, value,  lower, upper, optimise){
  dat[grepl(pattern,dat$switch),'value'] <- value
  dat[grepl(pattern,dat$switch),'upper'] <- upper
  dat[grepl(pattern,dat$switch),'lower'] <- lower
  dat[grepl(pattern,dat$switch),'optimise'] <- optimise
  return(dat)
}

gadget_areafile2 <- function (size, temperature, area = attr(size, "area")){
    temperature$area <- as.factor(temperature$area)
    levels(temperature$area) <- vapply(levels(temperature$area), 
        function(n) which(names(area) == n), 0)
    temperature$mean <- round(temperature$mean, 3)
    mapped_sizes <- vapply(names(area), function(n) {
        out <- size[size$area == n, "size"]
        if (length(out) == 0) 
            0
        else out
    }, 0)
    structure(list(labels = names(area), areas = seq_len(length(area)), 
        size = mapped_sizes, temperature = temperature), class = "gadget_areafile")
}
