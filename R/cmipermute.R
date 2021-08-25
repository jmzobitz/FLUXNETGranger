cmipermute <- function(Data,k_perm,mx,my,mz=0) {

  if (mz>0) {

    n <- dim(Data)[1]  # length of data
    m_col <- dim(Data)[2]  # columns of data

    U <- array(dim=1) # used indices
    Data_perm <- matrix(nrow=n,ncol=m_col)

    for (i in 1:n) {

      curr_matrix <- abs(Data[,(mx+my+1):m_col]-Data[i,(mx+my+1):m_col])

      # maximum norm of each point from the ith point in the subspace Z
      dist <- apply(curr_matrix, 1, max, na.rm = TRUE)


      dist_sorted <- sort(dist)
      epsilon <- dist_sorted[k_perm] # distance to the k_perm nearest neighbor in the z space

      # Identify nearest neighbors in the subspace Z
      N <- which(dist<=epsilon); #neighbors

      # Shuffle the neighbors
      N_n <- N[sample.int(length(N))]
      N <- N_n;

      j <- N[1]  # ??????
      m <- 0;
      while (j %in% U & m<(k_perm-1) ) {
        m <- m+1
        j <- N[m]
      }


      Data_perm[i,1]= Data[j,1];
      U <- c(U, j)

    }



    Data_perm[,2:m_col] <-  Data[,2:m_col]

  } else {

    n <- dim(Data)[1]  #length of data
    m_col <- dim(Data)[2]
    X <- Data[,1]
    Data_perm <- matrix(nrow=n,ncol=m_col)  # Matrix of NAs
    Data_perm[,1] <- X[sample.int(length(X))]  # permute X to destroy dependence with Y
    Data_perm[,2] <-Data[,2]  # keep Y the same

  }


  return(Data_perm)


}





