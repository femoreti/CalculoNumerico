SUBROUTINE init(matrixIn, matrixSize)
IMPLICIT NONE
	Integer :: matrixSize
	INTEGER :: i , j
	
	!matriz A(n,n), matriz S (n,1), resultado(n,1)
	DOUBLE PRECISION, DIMENSION(:,:) :: matrixIn
	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: A, S, CurrentX, lastX
	DOUBLE PRECISION :: stopValue = 1E-5
	DOUBLE PRECISION :: m = 0.5

	!Define os tamanhos das matrizes
	allocate (A(matrixSize, matrixSize))
	allocate (S(matrixSize, 1))
	allocate (CurrentX(matrixSize, 1))
	allocate (lastX(matrixSize, 1))

	!pegando valores do Python
	A = matrixIn
	
	!(1-m) * A
	do i=1,matrixSize
		do j=1,matrixSize
			A(i,j) = (1.0-m) * A(i,j)  
		end do
	end do
	
	!inicia a matriz S e a matriz de resultado com tamanho (n,1)
	do i = 1, matrixSize
		S(i,1) = m * (1.0 / matrixSize)
		CurrentX(i,1) = 1.0 / matrixSize
	end do
	
	! Multiplica a matriz A pela matriz S ate a precisao ser atingida
	do while (ABS(lastX(1,1) - CurrentX(1,1)) > stopValue)
		lastX = CurrentX
		
		!A * Xk
		CurrentX = matmul(A,lastX)
		! + mX0
		do i= 1, matrixSize
			CurrentX(i, 1) = CurrentX(i, 1) + S(i, 1)
		end do
	end do

	!Imprime os resultados dos pesos de cada nó do grafo
	do i = 1, matrixSize
		print *, CurrentX(i, 1)
	end do
	
	!remove da memoria as matrizes criadas
	deallocate(A)
	deallocate(S)
	deallocate(CurrentX)
	deallocate(lastX)
END SUBROUTINE
