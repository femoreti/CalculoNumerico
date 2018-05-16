PROGRAM bhaskara

    IMPLICIT NONE

    INTEGER :: i
    CHARACTER(len = 32) :: arg, x_real, x_real2, x_imaginario
    DOUBLE PRECISION :: inputs(0:2), delta, x1, x2, xi
	
    DO i = 1, 3 
        CALL get_command_argument(i, arg)
        READ(arg, *) inputs(i - 1)
    END DO

    delta = CalculaDelta(inputs(0), inputs(1), inputs(2))

    IF(delta >= 0.0) THEN
        x1 = (-inputs(1) + (delta ** 0.5)) / (2 * inputs(0))
        x2 = (-inputs(1) - (delta ** 0.5)) / (2 * inputs(0))
        WRITE(x_real, '(F0.16)') x1
        WRITE(x_real2, '(F0.16)') x2
        x_real = strReplace(x_real)
        x_real2 = strReplace(x_real2)
        WRITE(*, '(A,A)') 'r1 = ', x_real
        WRITE(*, '(A,A)') 'r2 = ', x_real2
    ELSE
        delta = -delta
        x1 = -inputs(1) / (2 * inputs(0))
        xi = (delta ** 0.5) / (2 * inputs(0))
        WRITE(x_real, '(F0.16)') x1
        WRITE(x_imaginario, '(F0.16)') xi
        x_real = strReplace(x_real)
        x_imaginario = strReplace(x_imaginario)
        WRITE(*, '(A,A,A,A,A)') 'r1 = ', trim(x_real), ' + ', trim(x_imaginario), 'i'
        WRITE(*, '(A,A,A,A,A)') 'r2 = ', trim(x_real), ' - ', trim(x_imaginario), 'i'
    END IF
    
    CONTAINS !implementa metodos
    FUNCTION strReplace(str) RESULT(output)
        CHARACTER(len = 32), intent(in) :: str
        CHARACTER(len = 32) :: output
	
        IF(str(1:1) == ".") THEN
            output = "0" // trim(str)
        ELSE IF(str(1:2) == "-.") THEN
            output = "-0" // trim(str(3:))
        ELSE
            output = str
        END IF 
    END FUNCTION
    
    FUNCTION CalculaDelta(a, b, c) RESULT(output)
        DOUBLE PRECISION, intent(in) :: a, b, c
        DOUBLE PRECISION :: output
	
        output = (b ** 2) - (4 * a * c)
    END FUNCTION

END PROGRAM
