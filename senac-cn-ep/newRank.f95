PROGRAM rank
IMPLICIT NONE
	INTEGER :: matrixSize
	INTEGER :: i , j , iterations
	
	!matriz A(n,n), matriz S (n,1), resultado(n,1)
	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: A, S, CurrentX, lastX
	DOUBLE PRECISION :: stopValue = 1E-5
	DOUBLE PRECISION :: temporaryValue
	DOUBLE PRECISION :: m = 0.5

	!Temporario, modificar quando vir do python
	matrixSize = 4

	!Define os tamanhos das matrizes
	allocate (A(matrixSize, matrixSize))
	allocate (S(matrixSize, 1))
	allocate (CurrentX(matrixSize, 1))
	allocate (lastX(matrixSize, 1))

	!Temporario, ler do python depois
	A(1,1) = 0
	A(1,2) = 0
	A(1,3) = 1.0
	A(1,4) = 1.0 / 2
	A(2,1) = 1.0 / 3
	A(2,2) = 0
	A(2,3) = 0
	A(2,4) = 0
	A(3,1) = 1.0 / 3
	A(3,2) = 1.0 / 2
	A(3,3) = 0
	A(3,4) = 1.0 / 2
	A(4,1) = 1.0 / 3
	A(4,2) = 1.0 / 2
	A(4,3) = 0
	A(4,4) = 0
	
	!(1-m) * A
	do i=1,dimensao
		do j=1,dimensao
			A(i,j) = (1.0-m) * A(i,j)  
		end do
	end do
	
	!inicia a matriz S e a matriz de resultado com tamanho (n,1)
	do i = 1, matrixSize
		S(i,1) = m * (1.0 / matrixSize)
		CurrentX(i,1) = 1.0 / matrixSize
	end do
	
	
	!iterations = 0
	temporaryValue = 1
	! Multiplica a matriz A pela matriz S ate a precisao ser atingida
	do while (ABS(lastX(1,1) - CurrentX(1,1)) > stopValue)
		lastX = CurrentX
		
		CurrentX = matmul(A,lastX)
		do i= 1, matrixSize
			CurrentX(i, 1) = CurrentX(i, 1) + S(i, 1)
		end do
		
		!A = matmul(A,A)
	    !CurrentX = matmul(A,S)
		!iterations = iterations + 1
	end do

	!Imprime os resultados dos pesos de cada nó do grafo
	do i = 1, matrixSize
		print *, CurrentX(i, 1)
	end do

	!print *, iterations
	
	!remove da memoria as matrizes criadas
	deallocate(A)
	deallocate(S)
	deallocate(CurrentX)
	deallocate(lastX)
END PROGRAM rank