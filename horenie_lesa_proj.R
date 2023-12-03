#pre zaciatok par funkcii urcenych na spinavu roboru 
create_matrix <- function(L, init_value = 0){
  M <- matrix(init_value, nrow = L, ncol = L)
  return(M)
} 

#typy stavov: 0 - prazdna, 1 - strom, 2- horiaci strom
#Tato funkcia posluzi najma na prezistenie, ci sused stromu so suradnicami [i,j] nehori, chudak
neighbour_ownership <- function(M, coords, state){
  if(coords[1] > 1) {
    if(M[coords[1] - 1, coords[2]] == state) return(TRUE)
  }
  if(coords[1] < length(M[1,])) {
    if(M[coords[1] + 1, coords[2]] == state) return(TRUE)
  }
  if(coords[2] > 1) {
    if(M[coords[1], coords[2] - 1] == state) return(TRUE)
  }
  if(coords[2] < length(M[1,])) {
    if(M[coords[1], coords[2] + 1] == state) return(TRUE)
  }
  return(FALSE)
}

#jeden krok simulacneho behu (podla zadanych podmienok) rozhoduje, ci bude strom v dalsom kroku simulacie v poriadku, 
#alebo bude horiet, pripadne ukonci svoju zivotnu put)
one_step_sim <- function(playground, f, p) {
  temp_playground <- playground
  for (i in 1: length(playground[1, ])) {
   for (j in 1: length(playground[1, ])) {
    current_state <- playground[i, j]
    if(current_state == 2) {
      temp_playground[i, j] <- 0
      next
    }
    if(current_state == 0) {
      if(runif(1) < p) {
        temp_playground[i, j] <- 1
        next
      }
    }
    if(current_state == 1){
      owns_neighbourd <- neighbour_ownership(playground, c(i, j), 2)
      if(owns_neighbourd == TRUE) {
        temp_playground[i, j] <- 2
        next
      }
      if(runif(1) < f) {
        temp_playground[i, j] <- 2
      } 
    }
   }
  }
  return(temp_playground)
}

#spocitame, kolko zivych stromov na konci simulacneho behu ostava -> ziskavame r
count_trees <- function(playground){
  r <-sum(playground==1)
  return(r)
}

#jeden simulacny beh o dlzke N krokov, vracia len hodnotu r
sim_run <- function(f, p, L, N) {
  playground <- create_matrix(L)
  colors <- c ("black", "green", "red")
  breaks <- c(-0.5, 0.5, 1.5, 2.5) #len fixujem, aby farby Black, Green, Red prisluchali jednotlivym stavom v bunkach 0,1,2 
  for (time_step in 1:N) {
    playground <- one_step_sim(playground, f, p)
  }
  image(z = playground, col = colors, breaks = breaks, axes = FALSE, main = "Forest Fire Simulation Result")
  r <- count_trees(playground)
  return(r)
}

#vypocet CI klasickou standardizaciou, vracia dolnu a hornu hranicu CI
calculate_CI <- function(data) {
  mean_value <- mean(data)
  std_dev <- sd(data)
  z_score <- qnorm(0.975)  #obostranny 95% CI
  interval <- z_score * (std_dev / sqrt(length(data)))
  
  return(c(mean_value - interval, mean_value + interval))
}

#viacero simulacnych behov, ziskanie r a ratanie 95% CI
n_sim_and_CI <- function(f, p, L, N, delta) {
  max_simulations <- 1000000 #maximalny pocet iteracii, tiez sa mi nechce bezat donekonecna, sorry not sorry
  if(p == 0) {   #ak by niekto skusal hranicne pripady, nech mi nevyhadzuje errory
    sim_run(f, p, L, N)
    print('Nulova pp. rastu stromov. Ziaden strom nemoze vyrast, interval spolahlivosti je preto (0,0)')
    return(c(0, 0))
    }
  if(f==0){    #ak by niekto skusal hranicne pripady, nech mi nevyhadzuje errory
    sim_run(f, p, L, N)
    print('Nulova pp. vyhorenia stromov, teda pri nenulovej pp. rastu stromov, bude hodnota r pre dostatocne vela krokov simulacii rovna L^2')
    return(c(L^2, L^2))
  }
  r_from_n_sim <- c()  #i-ta zlozka vektora prislucha hodnote r v i-tom simulacnom behu
  for (i in 1:max_simulations) {
    r_from_n_sim <- cbind(r_from_n_sim, sim_run(f, p, L, N))
    Sys.sleep(3/L)  #cim vacsia matica, tym pomalsi model, preto nepotrebujeme predlzit cas na zobrazenie :)
    if(i>1){
      CI_l <- calculate_CI(r_from_n_sim)[1]
      CI_u <- calculate_CI(r_from_n_sim)[2]
      CI_length <- CI_u - CI_l
      if(CI_length <= delta){
        break
      }
    } 
  }
  return(list(r_from_n_sim = r_from_n_sim, CI = c(CI_l, CI_u)))
}

#funkcia, ktorej prvym vystupom bude hodnota r v jednotlivych simulacnych behoch, az do zastavenia 
#druhym vystupom je hodnota 95% intervalu spolahlivosti, ktory splna maximalnu dlzku CI rovnu delte

n_sim_and_CI(f = 0.2, p = 0.1, L = 50, N = 50, delta =10)

#trosku komentarov (radsej okomentujem f2f): pre zdovodnenie v kode jednoducho predpokladame, ze N a L sa nebude menit 
#a pracujeme len s parametrami f a p

#nech fixneme f, a hybeme s p - pri zvyseni p -> vyssi rast stromov, rovnaka pp. vznietenia, CI sa 'posunie doprava' - vyssi pocet stromov na konci s.behu
#ak p poklesne, stromy vznikaju pomalsie, tak ich pocet na konci s.behu bude nizsi, t.j. CI sa 'posunie nalavo, blizsie k nule'

#ak fixneme p a f navysime, stromy budu horiet rychlejsie, nakonci ich zostane menej -> posun CI 'nalavo, blizsie k nuke', 
#pri znizeni f opacne, posun 'doprava, dalej od nuly'.
