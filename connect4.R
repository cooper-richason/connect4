rm(list = ls())

four.in.a.row = function(player, v, debug=FALSE) {
  if (debug) {
    cat(sep="", "four.in.a.row(player=", player, ", v=", v, ")\n")
  }
  vector_as_string <- paste0(v, collapse = "")
  
  if (player == "X" & grepl("XXXX",vector_as_string)) {
    return(TRUE)
    
  } else if (player == "O" & grepl("OOOO",vector_as_string)) {
    return(TRUE)
    
  } else {
    return(FALSE)
  }
}

won = function(player, board, r, c, debug=FALSE) {
  if (debug) {
    cat(sep="", "won(player=", player, ", board=\n")
    print(board)
    cat(sep="", ", r=", r, ", c=", c, ")\n")
  }
  col_vector <- board[col(board) == c]
  row_vector <- board[row(board) == r]
  diag_vector <- board[row(board) + col(board) == r + c]
  reverse_diag_vector <- board[row(board) - col(board) == r - c]
  
  return(    four.in.a.row(player, col_vector) |
               four.in.a.row(player, row_vector) |
               four.in.a.row(player, diag_vector) |
               four.in.a.row(player, reverse_diag_vector))
}

largest.empty.row = function(board, col, debug=FALSE) {
  if (debug) {
    cat(sep="", "largest.empty.row(board=\n")
    print(board)
    cat(sep="", ", col=", col, ")\n")
  }
  row <- NULL
  for(i in seq(1:6)) {
    if (board[i,col]=="E") {
      row <- i
    }
  }
  return(row)
}

active_board <- matrix(data = "E",nrow = 6, ncol = 7)

x <- rep(x=1:7, each=1)
y <- rep(x=1, time=7)

holes_x <- rep(x=1:7, each=6)
holes_y <- rep(x=1:6, time=7)

plot(x,y,col = '#CCCCCC', xlim= c(0,8), ylim= c(7,0))
rect(xleft = 0.5,ybottom = 0.5,ytop = 6.5,xright = 7.5,col='#FFF5E0', border = NA)
points(x= holes_x, y= holes_y, pch = 19, col = "white", cex = 7)

# Horizontal Lines:
#segments(x0= c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5), y0 = c(0.5,1.5,2.5,3.5,4.5,5.5,6.5),
#         x1= c(7.5,7.5,7.5,7.5,7.5,7.5,7.5,7.5), y1= c(0.5,1.5,2.5,3.5,4.5,5.5,6.5))

# Vertical Lines:
#segments(x0= c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5), y0 = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5),
#         x1= c(0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5), y1= c(6.5,6.5,6.5,6.5,6.5,6.5,6.5,6.5))

turns_remaining <- 56
winner <- NA
player <- 'X'

repeat {
  cat(sep = "","There are ",turns_remaining," turns remaining, and the player is ",player,". \n")
  cat(sep = "", "Cick to play.","\n")
  
  if (player == 'X') { #Human Player
    
    repeat {
      index <- identify(x, y, n=1)
      col <- x[index]; print(col)
      row <- largest.empty.row(active_board,col)
      if ((!is.null(row))){
        break
      }
      cat("That column is full, please try again...")
    }
    active_board[row,col] <- player
    points(x=col, y=row, pch = 19, col = "#BB2525", cex = 5)
    
  } else { #Computer Player
    col_pref <- c(4,5,3,6,2,7,1)
    
    human_col <- col
    human_row <- row
    
    # Lowest Strategy - Random assignment based on play
    for (i in 1:7) {
      col <- col_pref[i]
      row <- largest.empty.row(active_board,col)
      if ((!is.null(row))){
        computer_play <- col
        break
      }
    }
    
    # Check for Connections:
    
    if (human_col != 1 & human_col != 7) {
      
      if (!is.null(largest.empty.row(active_board,(human_col+1)))){
        if(active_board[human_row,human_col-1] == 'X' & largest.empty.row(active_board,(human_col+1)) == human_row){
          computer_play <- human_col + 1
        } 
      } 
      if (!is.null(largest.empty.row(active_board,(human_col-1)))) {
        if(active_board[human_row,human_col+1] == 'X' & largest.empty.row(active_board,(human_col-1)) == human_row){
          computer_play <- human_col - 1
        }
        
      }
      if (!is.null(largest.empty.row(active_board,(human_col+1)))) {
        if(active_board[human_row-1,human_col-1] == 'X' & largest.empty.row(active_board,(human_col+1)) == (human_row+1)){
          computer_play <- human_col + 1
        }
      } 
      if (!is.null(largest.empty.row(active_board,(human_col-1)))) {
        if(active_board[human_row-1,human_col+1] == 'X' & largest.empty.row(active_board,(human_col-1)) == (human_row+1)){
          computer_play <- human_col - 1
        }
      }
    }
    
    if (!is.null(largest.empty.row(active_board,(human_col)))) {
      if(active_board[human_row-1,human_col] == 'X'){
        computer_play <- human_col
      }
    }  
    # Check for XEX Strategy
    
    if (human_row ==6) {
      check_row <- active_board[row(active_board) ==6]
      check_row <- paste0(check_row, collapse = "")
      check <- grepl("XEXE",check_row) | grepl("EXEX",check_row)
      if (check){
        check_row <- active_board[row(active_board) ==6]
        x_indices <- grep("X",check_row)
        e_indices <- grep("E",check_row)
        
        for (i in 1:(length(x_indices)-1)) {
          for (k in 1:length(e_indices)) {
            if (e_indices[k] == (x_indices[i]+1) & x_indices[(i+1)] == (e_indices[k]+1)) {
              computer_play <- e_indices[k]
              break
            } 
          }
        }
      }
    }
    
    # Checks if human is one move away from winner. If found, player there
    for (i in 1:7) {
      test_board <- active_board
      test_col <- col_pref[i]
      test_row <- largest.empty.row(test_board,test_col)
      test_board[test_row,test_col] = "X"
      if (won("X",test_board,test_row,test_col)){
        computer_play <- test_col
        break
      }
    }
    
    # Checks if it is one move away from winning. If found, it plays there
    for (i in 1:7) {
      test_board <- active_board
      test_col <- col_pref[i]
      test_row <- largest.empty.row(test_board,test_col)
      test_board[test_row,test_col] = "O"
      if (won("O",test_board,test_row,test_col)){
        computer_play <- test_col
        break
      }
    }
    
    # Computer Makes Move:
    col <- computer_play
    row <- largest.empty.row(active_board,computer_play)
    active_board[row,col] <- player
    points(x=col, y=row, pch = 19, col = "#141E46", cex = 5)
    
  } #### End of Computer Play Code
  
  ### Move Maintenance Code:
  
  if (won(player,active_board,row,col)) {
    winner <- player
  }
  turns_remaining <- turns_remaining - 1
  player <- ifelse(test= (player == 'X'),'O','X'); player
  
  if (turns_remaining == 0 | (!is.na(winner))) {
    if (!is.na(winner)) {
      cat("The Winner is ", winner)
      break
    } else {
      cat("No body winns...")
      break
    }
  }
}

