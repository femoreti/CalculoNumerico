program rank
implicit none

	!dimensoes de entrada, sempre devem ser iguais
	integer :: matrixSize
	integer :: i , j , iterations
	
	!matriz A, matriz S  |  vetor dimensao_1
	double precision, dimension(:,:), allocatable :: matrix_A, matrix_S,vetor_pesos_aux
	double precision :: precisao = 0.00001
	double precision :: valor_antigo = 1

	!Recebe a matriz do python ou le de um arquivo
	matrixSize = 4

	!Aloca dinamicamente a matriz com as dimensoes da matriz recebida
	allocate ( matrix_A(matrixSize,matrixSize) )
	allocate ( matrix_S(matrixSize,1) )
	allocate ( vetor_pesos_aux(matrixSize,1) )


	matrix_A(1,1) = 0
	matrix_A(1,2) = 0
	matrix_A(1,3) = 1.0
	matrix_A(1,4) = 1.0/2
	matrix_A(2,1) = 1.0/3
	matrix_A(2,2) = 0
	matrix_A(2,3) = 0
	matrix_A(2,4) = 0
	matrix_A(3,1) = 1.0/3
	matrix_A(3,2) = 1.0/2
	matrix_A(3,3) = 0
	matrix_A(3,4) = 1.0/2
	matrix_A(4,1) = 1.0/3
	matrix_A(4,2) = 1.0/2
	matrix_A(4,3) = 0
	matrix_A(4,4) = 0



	! Multiplica os valores da matrix_A por (1-m)

	do i=1,matrixSize
		matrix_S(i,1) = (1.0/matrixSize) 
		vetor_pesos_aux(i,1) = 1.0/matrixSize
	end do


	


	iterations = 0
	! Multiplica a matrix_A por matrix_S até um threshold
	do while (ABS(valor_antigo-vetor_pesos_aux(1,1)) > precisao )
		valor_antigo = vetor_pesos_aux(1,1)
		matrix_A = matmul(matrix_A,matrix_A)
	    vetor_pesos_aux = matmul(matrix_A,matrix_S)
		iterations = iterations + 1
	end do


	do i=1,matrixSize
		print *,vetor_pesos_aux(i,1)
	end do

	!print *, iterations


	deallocate( matrix_A )
	deallocate( matrix_S )
	deallocate( vetor_pesos_aux)
end program rank