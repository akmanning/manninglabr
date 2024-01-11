cohorts_summary_demographics <- function(new.data.set)
{

  lapply(c('table1','IRdisplay'),
         function(pkg_name) { if(! pkg_name %in% installed.packages()) { install.packages(pkg_name)} } )


  library(table1)
  library(IRdisplay)

  my.render.cont <- function(x) {
    with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                             "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
  }
  my.render.cat <- function(x) {
    c("", sapply(stats.default(x), function(y) with(y,
                                                    sprintf("%d (%0.0000f %%)", FREQ, PCT))))
  }

  #options(repr.vector.quote=FALSE)
  label(new.data.set$age) <- "Age"
  units(new.data.set$age) <- "Years"
  label(new.data.set$gender) <- "Reported Gender"
  label(new.data.set$sex_at_birth) <- "Biological Sex"
  label(new.data.set$race) <- "Reported Race"
  label(new.data.set$ethnicity) <- "Reported Ethnicity"


  to.html <- table1(~ age + gender + sex_at_birth + race + ethnicity, data=new.data.set,
                    render.continuous=my.render.cont,
                    render.categorical=my.render.cat)

  display_html(to.html)
}
