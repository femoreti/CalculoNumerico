PROGRAM bissec
IMPLICIT NONE

    REAL :: lowerBound, upperBound, errAprox, input, root
    INTEGER :: maxIterations, count
    CHARACTER(len = 32) :: arg
    CALL get_command_argument(1, arg)
    READ (arg, *) input
    IF (input < 0) THEN
        PRINT *, "Input must be greater then 0"
        STOP 1
    END IF

    count = 0
    maxIterations = 10000
    errAprox = 1E-6
    lowerBound = 0
    upperBound = input
    root = (upperBound + lowerBound) / 2


    DO WHILE (ABS(upperBound - root) > errAprox .AND. count < maxIterations)
        IF ((root ** 2) > input) THEN
            upperBound = root
        ELSE
            lowerBound = root
        END IF

        root = (upperBound + lowerBound) / 2
        count = count + 1
    END DO

    PRINT *, root
END PROGRAM bissec