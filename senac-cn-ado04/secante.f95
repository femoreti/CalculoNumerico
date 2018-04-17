PROGRAM secante
    IMPLICIT NONE

    INTEGER :: maxIteration = 1000, i
    DOUBLE PRECISION :: input, p0, p, p1, errorValue = 1E-6
    READ(*, *) input

    p0 = input
    p = input - 1.0
    DO i = 1, maxIteration
        p1 = (p0 * p + input) / (p0 + p)
        IF (abs(p1 - p) < errorValue .OR. p1 ** 2 == input) THEN
            PRINT *, REAL(p1)
            EXIT
        END IF
        p0 = p
        p = p1   
    END DO

END PROGRAM