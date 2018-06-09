PROGRAM rank
IMPLICIT NONE
	INTEGER :: matrixSize
	INTEGER :: i , j , iterations
	
	!matriz A(n,n), matriz S (n,1), resultado(n,1)
	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: A, S, matrixResult
	DOUBLE PRECISION :: stopValue = 1E-5
	DOUBLE PRECISION :: temporaryValue

	!Temporario, modificar quando vir do python
	matrixSize = 4

	!Define os tamanhos das matrizes
	allocate (A(matrixSize, matrixSize))
	allocate (S(matrixSize, 1))
	allocate (matrixResult(matrixSize, 1))

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
	
	!inicia a matriz S e a matriz de resultado com tamanho (n,1)
	do i = 1, matrixSize
		S(i,1) = (1.0 / matrixSize) 
		matrixResult(i,1) = 1.0 / matrixSize
	end do
	
	!iterations = 0
	temporaryValue = 1
	! Multiplica a matriz A pela matriz S ate a precisao ser atingida
	do while (ABS(temporaryValue -  matrixResult(1,1)) > stopValue)
		temporaryValue = matrixResult(1,1)
		A = matmul(A,A)
	    matrixResult = matmul(A,S)
		!iterations = iterations + 1
	end do

	!Imprime os resultados dos pesos de cada nó do grafo
	do i = 1, matrixSize
		print *, matrixResult(i, 1)
	end do

	!print *, iterations
	
	!remove da memoria as matrizes criadas
	deallocate(A)
	deallocate(S)
	deallocate(matrixResult)
END PROGRAM rank