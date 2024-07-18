score <- function(x, x_min, A) {
  z <- (x-x_min) * (10/A)
  return(z)
}



#' Desenha um Diagrama de Ramo e Folha usando TikZ
#'
#' Esta função cria um código TikZ para desenhar um diagrama de ramo e folha.
#' 
#' @param x Um vetor numérico com os dados a serem plotados.
#' @param k Um multiplicador que converte o vetor x para números inteiros. Padrão é 1.
#' @param d Um divisor que separa os caules das folhas. Padrão é 10.
#' @param lowhi Se TRUE, os caules serão marcados com 'L' (baixo) ou 'H' (alto) com base nas folhas. Padrão é FALSE.
#' @param main Um título opcional para o diagrama.
#' @param clab Um rótulo opcional para os caules.
#' @param flab Um rótulo opcional para as folhas.
#' 
#' @return Um string contendo o código TikZ para o diagrama de ramo e folha.
#' 
#' @examples
#' x <- c(12, 23, 23, 34, 45, 56, 67, 78, 89)
#' draw_stemleaf_plot(x)
#' 
draw_stemleaf_plot <- function(x, k=1, d=10, lowhi=FALSE, main=NULL, clab=NULL, flab=NULL){
  # Ordenar a lista de valores
  x <- sort(x)
  # O multiplicador k converte o vetor x para números inteiros
  x <- x * k
  # O divisor d separa os caules das folhas
  caules <- as.character(x%/%d)
  folhas <- x%%d
  
  if (lowhi) {
    caules_tmp <- c()
    for (i in 1:length(caules)){
      if (folhas[i] < 5) {
        caules_tmp <- append(caules_tmp, sprintf("%sL", caules[i]))
      } else {
        caules_tmp <- append(caules_tmp, sprintf("%sH", caules[i]))
      }
    }
  }
  caules <- caules_tmp
  caules_unicos <- unique(caules) # Caules únicos
  
  # Desenha um diagrama TikZ
  tikz <- "\\begin{tikzpicture}"

  # Título do diagrama
  if (!is.null(main)) {
    main <- sprintf("\\node at (0, 0.25) {%s};", main)
    tikz <- sprintf("%s%s", tikz, main)
  }
  
  # Rótulo do Caule
  if (!is.null(clab)) {
    clab <- sprintf("\\node[right] at (-2, -%.2f) {Caule: %s};", 0.75+length(caules_unicos)/2, clab)
    tikz <- sprintf("%s%s", tikz, clab)
  }
  
  # Rótulo da Folha
  if (!is.null(flab)) {
    flab <- sprintf("\\node[right] at (-2, -%.2f) {Folha: %s};", 1.25+length(caules_unicos)/2, flab)
    tikz <- sprintf("%s%s", tikz, flab)
  }
  
  # Linha vertical separando caules e folhas
  vline <- sprintf("\\draw[thick] (0, -.25) -- (0, -%2f);", 0.25+length(caules_unicos)/2)
  tikz <- sprintf("%s%s", tikz, vline)
  
  # Insere os caules
  for (i in 1:length(caules_unicos)){
    caule_node <- sprintf("\\node at (-0.5, -%.2f) {%s};", i/2, caules_unicos[i])
    tikz <- sprintf("%s%s", tikz, caule_node)
  }
  
  # Insere as folhas
  for (i in 1:length(caules_unicos)) {
    folhas_i <- folhas[caules==caules_unicos[i]]
    folha_node <- ""
    for (fi in folhas_i) {
      if (d==10) {
        folha_node <- sprintf("%s%d", folha_node, fi)
      }
      if (d==100) {
        folha_node <- sprintf("%s%02d ", folha_node, fi)
      }
      
    }
    folha_node <- sprintf("\\node at (0.25, -%.2f)[right] {%s};", i/2, folha_node)
    tikz <- sprintf("%s%s", tikz, folha_node)
  }
  
  tikz <- sprintf("%s\\end{tikzpicture}", tikz)
  return(tikz)
}


#' Desenha um Gráfico de Pontos usando TikZ
#'
#' Esta função cria um código TikZ para desenhar um gráfico de pontos.
#' 
#' @param x Um vetor numérico com os dados a serem plotados.
#' @param k Um valor para determinar os limites dos ticks no eixo x. Padrão é 5.
#' @param lab Um rótulo opcional para o eixo x.
#' 
#' @return Uma string contendo o código TikZ para o gráfico de pontos.
#' 
#' @examples
#' x <- c(12, 23, 23, 34, 45, 56, 67, 78, 89)
#' draw_dots_plot(x)
#' 
draw_dots_plot <- function(x, k = 5, lab=NULL) {
  get_lim <- function(x, k=5) {
    l1 <- k*(x%/%k)
    dl1 <- abs(x-l1)
    l2 <- k*((x+k)%/%k)
    dl2 <- abs(x-l2)
    if (dl1<=dl2) {
      return(l1)
    } else {
      return(l2)
    }
  }
  x <- sort(x)
  x_min <- min(x)
  x_max <- max(x)
  A <- x_max - x_min
  
  x_ticks <- seq(from=get_lim(x_min-k/2, k), to=get_lim(x_max+k/2, k), length=5)
  z_ticks <- score(x_ticks, x_min, A)
  print(x_ticks)
  print(z_ticks)
  z <- score(x, x_min, A)
  z_min <- z_ticks[1]-1
  z_max <- z_ticks[5]+1
  tikz <- "\\begin{tikzpicture}"
  tikz <- sprintf("%s\\draw (%.2f, 0) -- (%.2f, 0) node[midway, yshift=-25pt] {%s};", tikz, z_min, z_max, lab)
  for (i in 1:length(x_ticks)) {
    tikz <- sprintf("%s\\draw (%.2f, 2pt) -- (%.2f, -2pt) node[below] {%.2f};", tikz, z_ticks[i], z_ticks[i], x_ticks[i])
  }
  
  y <- 0.5
  tikz <- sprintf("%s\\filldraw (%.5f, %.2f) circle (2pt);", tikz, z[1], y)
  for (i in 2:length(x)){
    if (x[i] == x[i-1]) {
      y <- y + 0.5
    } else {
      y <- 0.5
    }
    tikz <- sprintf("%s\\filldraw (%.5f, %.2f) circle (2pt);", tikz, z[i], y)
  }
  tikz <- sprintf("%s\\end{tikzpicture}", tikz)
  
  return(tikz)
}



draw_histogram <- function(x, x_scale=1, y_scale=1, lab="", x_skip=1, y_skip=1) {
  x_min <- min(x)
  x_max <- max(x)
  x_ticks <- seq(from=x_min, to=x_max, by=x_skip)
  freq_tbl <- table(x)
  y_ticks <- seq(from=0, to=max(freq_tbl)+y_skip, by=y_skip)
  tikz <- sprintf("\\begin{tikzpicture}[x=%.5fcm, y=%.5fcm]", x_scale, y_scale)
  tikz <- sprintf("%s\\draw[-latex] (%.2f, 0) -- (%.2f, 0) node[midway, yshift=-25pt] {%s};", tikz, x_min, x_max+1, lab)
  for (x_tick in x_ticks) {
    tikz <- sprintf("%s\\draw (%.2f, 0) -- (%.2f, -2pt) node[below] {%d};", tikz, x_tick, x_tick, x_tick)
  }
  tikz <- sprintf("%s\\draw[-latex] (%.2f, 0) -- (%.2f, %.2f) node[midway, left, rotate=90, yshift=35pt] {Frequência};", tikz, x_min-0.5, x_min-0.5, max(y_ticks)+1)
  for (y_tick in y_ticks) {
    tikz <- sprintf("%s\\draw (%.2f, %d) -- (%.2f, %.2f) node[left] {%d};", tikz, x_min-0.5, y_tick, x_min-0.75, y_tick, y_tick)
  }
  for (xi in names(freq_tbl)) {
    fi <- freq_tbl[[xi]]
    xi <- as.numeric(xi)
    tikz <- sprintf("%s\\draw[fill=gray!25!white] (%.2f, 0) rectangle (%.2f, %d);", tikz, xi-0.5, xi+0.5, fi)
    
  }
  
  tikz <- sprintf("%s\\end{tikzpicture}", tikz)
  return(tikz)
}
