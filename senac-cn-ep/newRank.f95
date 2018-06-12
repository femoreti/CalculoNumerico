SUBROUTINE init(matrixIn, matrixSize)
IMPLICIT NONE
	Integer :: matrixSize
	INTEGER :: i, j, totalPrinted, currentPage
	DOUBLE PRECISION :: greaterValue
	
	!matriz A(n,n), matriz S (n,1), resultado(n,1)
	DOUBLE PRECISION, DIMENSION(:,:) :: matrixIn
	DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: A, S, CurrentX, lastX
	DOUBLE PRECISION :: stopValue = 1E-5
	DOUBLE PRECISION :: m = 0.15

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
		CurrentX(i,1) = 1 !iniciar como um para nao dar erro de precisao logo no inicio
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
	
	totalPrinted = 0
	
	Print *, "Iteração"," -", " Pagina", " ---", " Peso"
	!Imprime os resultados de acordo com sua importancia
	do while(totalPrinted < matrixSize)
		greaterValue = 0
		currentPage = 0
		do i = 1, matrixSize
			if (CurrentX(i, 1) > 0 .AND. CurrentX(i, 1) > greaterValue) then
				greaterValue = CurrentX(i, 1)
				currentPage = i
			end if
		end do

		CurrentX(currentPage, 1) = -1
		totalPrinted = totalPrinted + 1
		print *, totalPrinted,"- Pagina",currentPage,"--- peso",greaterValue
	end do
	
	
	
	!remove da memoria as matrizes criadas
	deallocate(A)
	deallocate(S)
	deallocate(CurrentX)
	deallocate(lastX)
END SUBROUTINE
