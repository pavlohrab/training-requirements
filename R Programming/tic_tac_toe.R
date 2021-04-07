# Tic-Tac-Toe game
# Developed by Pavlo Hrab

############ See if input is OK ##############
good_symbol <- function(x){
  x <- toupper(x)
  if (x == '0'){
    x <- 'O'
  } else if (x == 'х'){
    x <- 'X'
  } else if (x == 'Х'){
    x <- 'X'
  }
  else if (x == 'X'){}
  else if (x == 'O'){}
  else{
    x <- NULL
  }
  return(x)
}
good_number <- function(x){
  if (is.na(x)){
    x <- NULL
    cat("You entered not a number, please re-enter the value \n")
  }
  else if (x >3){
    x <- NULL
    cat("You entered the number > 3, please re-enter the value \n")
  }
  return(x)
}

########## Helper functions ###############
exit <- function() { invokeRestart("abort") }
print_board <- function(board){
  cat("###################### \n#### CURRENT BOARD ###\n")
  print(board)
}
bot_turn <- function(board, bot_symbol){
  numbers <- seq(1:3)
  good_cols <- apply(board, 2, function(r) any(r %in% NA))
  chosen_col <- bot_random(numbers[good_cols])
  chosen_rows <- bot_random(numbers[is.na(board[,chosen_col])])
  board[chosen_rows, chosen_col] <- bot_symbol
  return(board)
}
bot_random <- function(num){
  if (length(num)==1){
    random_num <- num
  }else{
    random_num <- sample(as.numeric(num), 1)
  }
  return(random_num)
}
human_turn <- function(board, hum_symbol){
  row_1 = NULL
  col_1 = NULL
  is_occupied = T
  while ( is_occupied == T) {
    while (is.null(row_1)) {
      cat("Please choose the row (1-3) : ")
      row_1 <- good_number(as.numeric(readLines(con = con, n = 1)))
    }
    while (is.null(col_1)) {
      cat("Please choose the column (1-3) : ")
      col_1 <- good_number(as.numeric(readLines(con = con, n = 1)))
    }
    if (is.na(board[row_1, col_1])){
      is_occupied = F
    }else{
      cat("You have chosen the occupied row and column \nPlease make another choice!\n")
      row_1 = NULL
      col_1 = NULL
    }
}
  cat("You made the choice! \n")
  board[row_1, col_1] = hum_symbol
  return(board)
}
check_victory <- function(board){
  numbers <- seq(1:3)
  good_rows <- apply(board, 1, function(r) any(r %in% NA))
  good_cols <- apply(board, 2, function(r) any(r %in% NA))
  results <- sapply(c("X", "O"), function(x){
    results <- c()
    log_board <- board == x
    log_board[is.na(as.data.frame(log_board))] <- F
    cols_res <- apply(log_board, 2,function(y){
      F %in% y
    })
    rows_res <- apply(log_board, 1,function(y){
      F %in% y
    })
    diag1 <- c(log_board[1,1], log_board[2,2], log_board[3,3])
    if (F %in% diag1){
      diag1 <- T
    } else{
      diag1 <- F
    }
    diag2 <- c(log_board[1,3], log_board[2,2], log_board[3,1])
    if (F %in% diag2){
      diag2 <- T
    } else{
      diag2 <- F
    }
    results <- c(cols_res, rows_res, diag1, diag2)
  })
  if (F %in% results[,'X']){
    return('X')
  } else if (F %in% results[,'O']){
    return('O')
  }
  else{
    return(NULL)
  }
}
if (interactive()) {
  con <- stdin()
} else {
  con <- "stdin"
}

######### Game Logic ###################
cat("X or O? : ")
symbol <- readLines(con = con, n = 1)
symbol <- good_symbol(symbol)
if (is.null(symbol)){
  cat("The input does not match X or O. Please re-enter the desired starting letter! \n")
  exit()
}
cat("You begin the game! \n")
board <- matrix(nrow = 3, ncol = 3)
if (symbol == 'O') {
  bot_symbol <- 'X'
  hum_symbol <- 'O'
} else {
  bot_symbol <- 'O'
  hum_symbol <- 'X'
  board <- bot_turn(board, bot_symbol)
  cat("Bot has made it's move!\n")
}
victory <- check_victory(board)
while (is.null(victory)){
  if (T %in% is.na(board)){
    print_board(board)
    board <- human_turn(board, hum_symbol)
    victory <- check_victory(board)
    if (is.null(victory)){
      board <- bot_turn(board, bot_symbol)
      victory <- check_victory(board)
    } else{
    }
  } else{
    victory <- 'R'
  }
}

if (victory == bot_symbol){
  cat("You lost :( \nGood luck next time!\n")
} else if (victory == hum_symbol){
  cat("You won! \nCongrats!\n")
} else {
  cat("It's a tie!\n")
}


