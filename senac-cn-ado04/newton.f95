PROGRAM newton
IMPLICIT NONE

    REAL :: input, sqr, errAprox
    INTEGER :: count, maxIterations
    READ(*,*) input
    IF (input < 0) THEN
        PRINT *, "Input precisa ser maior que 0"
        STOP 1
    END IF

    count = 0
    maxIterations = 10000
    errAprox = 1E-6
    sqr = 1

    DO WHILE (ABS(sqr ** 2 - input) > errAprox .AND. count < maxIterations)
        sqr = ((input / sqr) + sqr) / 2
        count = count + 1
    END DO

    PRINT *, sqr

END PROGRAM Newton