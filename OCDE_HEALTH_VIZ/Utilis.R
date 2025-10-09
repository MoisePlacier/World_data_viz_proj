euclid_sim <- function(x, y) {
  1 / (1 + sqrt(sum((x - y)^2)))
}

calc_similarity <- function(dt, selected_country, vars) {
  dt_scaled <- copy(dt)
  # On ne scale pas ici si on veut garder les unités réelles
  x_ref <- as.numeric(dt_scaled[REF_AREA == selected_country, ..vars])
  sims <- dt_scaled[, .(REF_AREA, sim = apply(.SD, 1, function(y) euclid_sim(x_ref, as.numeric(y)))), .SDcols = vars]
  sims[]
}

calc_contrib <- function(dt, ref_country, clicked_country, vars){
  x <- as.numeric(dt[REF_AREA == ref_country, ..vars])
  y <- as.numeric(dt[REF_AREA == clicked_country, ..vars])
  
  diff <- y - x
  contrib <- diff / sum(abs(diff))  # pondération en conservant le signe
  
  data.table(variable = vars,
             difference = diff,
             contribution = contrib)[order(-abs(contribution))]
}