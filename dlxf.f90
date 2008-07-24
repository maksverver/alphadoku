PROGRAM Sudoku
!    INTEGER, PARAMETER :: DM = 3, CSTART = IACHAR('1'), ONESOLUTION=1, &
    INTEGER, PARAMETER :: DM = 5, CSTART = IACHAR('A'), ONESOLUTION=0, &
        SZ = DM*DM, AR=SZ*SZ, VO=AR*SZ, WI = 4*AR

    TYPE Node
        TYPE(Node), POINTER :: left, right, up, down, column
        INTEGER :: data
    END TYPE Node

    TYPE(Node), TARGET :: header(0:WI + SZ*WI)
    INTEGER :: rows(0:VO), solution(AR), solution_size = 0

    CALL ConnectHeaders
    CALL ConnectOneNodes
    Call Process

CONTAINS

    SUBROUTINE ConnectHeaders
        DO n=0,WI
            header(n)%left  => header(MOD(n + WI + 1 - 1, WI + 1))
            header(n)%right => header(MOD(n          + 1, WI + 1))
            header(n)%up    => header(n)
            header(n)%down  => header(n)
            NULLIFY(header(n)%column)
        END DO
        header(0   )%data = HUGE(0)
        header(1:WI)%data = 0
    END SUBROUTINE

    SUBROUTINE ConnectOneNodes
        INTEGER :: base, row, r, c, b, x, cols(0:3)
        TYPE(Node), POINTER :: cur

        base = 1 + WI
        DO row=0,VO-1
            rows(row) = base

            r = row/AR; c = MOD(row/SZ, SZ); b = DM*(r/DM) + c/DM; x = MOD(row, SZ)

            cols = (/ row/SZ, SZ*r + x, SZ*c + x, SZ*b + x /) + (AR)*(/ 0, 1, 2, 3 /) + 1

            DO n=0,3
                cur => header(base + n)
                cur%left   => header(base + MOD(n + 4 - 1, 4))
                cur%right  => header(base + MOD(n     + 1, 4))
                cur%up     => header(cols(n))%up
                cur%down   => header(cols(n))
                cur%down%up => cur
                cur%up%down => cur

                cur%data = row

                cur%column => header(cols(n))
                cur%column%data = cur%column%data + 1
            END DO
            base = base + 4
        END DO
        rows(VO) = base
    END SUBROUTINE

    SUBROUTINE Process
        INTEGER :: i
        CHARACTER(AR) :: puzzle

        DO
            READ (*,*,IOSTAT=i) puzzle
            IF (i /= 0) EXIT
            CALL PlaceClues(puzzle)
            IF (Search() /= ONESOLUTION) PRINT "('No solution for ""'A'""!')", puzzle
            CALL Reset
        END DO
    END SUBROUTINE

    SUBROUTINE PlaceClues(s)
        CHARACTER(AR) :: s

        DO i=1,AR
            j = IACHAR(s(i:i))
            IF (j >= CSTART .AND. j < CSTART + SZ) CALL Place(j - CSTART, i - 1)
        END DO
    END SUBROUTINE

    SUBROUTINE Place(x, pos)
        INTEGER, INTENT(IN) :: x, pos
        INTEGER :: row, r

        row = SZ*pos + x
        r = rows(row)

        !PRINT "('Digit 'I1' at 'I2)", 1 + x, pos

        DO r=rows(row),rows(row+1)-1
            CALL CoverColumn(header(r)%column)
        END DO

        solution_size = solution_size + 1
        solution(solution_size) = row

    END SUBROUTINE

    RECURSIVE INTEGER FUNCTION Search() RESULT(res)
        TYPE(Node), POINTER :: r, c, d

        c => header(0)%right

        IF (ASSOCIATED(c, header(0))) THEN
            res = Callback()
            RETURN
        END IF

        ! Pick the best column c
        IF (c%data > 1) THEN
            d => c
            DO WHILE (.NOT.ASSOCIATED(d, header(0)))
                IF (d%data < c%data) THEN
                    c => d
                    IF (d%data <= 1) EXIT
                END IF
                d => d%right
            END DO
        END IF

        IF (c%data == 0) THEN
            res = 0
            RETURN
        END IF

        ! Cover column
        solution_size = solution_size + 1
        CALL CoverColumn(c)

        ! Try all rows
        r => c%down
        DO WHILE (.NOT.ASSOCIATED(r, c))
            solution(solution_size) = r%data

            ! Cover row
            d => r%right
            DO WHILE (.NOT.ASSOCIATED(d, r))
                CALL CoverColumn(d%column)
                d => d%right
            END DO

            res = Search()

            ! Uncover row
            d => r%left
            DO WHILE (.NOT.ASSOCIATED(d, r))
                CALL UncoverColumn(d%column)
                d => d%left
            END DO

            ! Abort?
            IF (res /= 0) EXIT

            r => r%down
        END DO

        ! Uncover column
        CALL UncoverColumn(c)
        solution_size = solution_size - 1
    END FUNCTION


    SUBROUTINE CoverColumn(col)
        TYPE(Node), POINTER :: col, i, j

        col%left%right => col%right
        col%right%left => col%left

        i => col%down
        DO WHILE (.NOT.ASSOCIATED(i, col))
            j => i%right
            DO WHILE(.NOT.ASSOCIATED(j, i))
                j%down%up => j%up
                j%up%down => j%down
                j%column%data = j%column%data - 1
                j => j%right
            END DO
            i => i%down
        END DO
    END SUBROUTINE

    SUBROUTINE UncoverColumn(col)
        TYPE(Node), POINTER :: col, i, j

        i => col%up
        DO WHILE (.NOT.ASSOCIATED(i, col))
            j => i%left
            DO WHILE(.NOT.ASSOCIATED(j, i))
                j%down%up => j
                j%up%down => j
                j%column%data = j%column%data + 1
                j => j%left
            END DO
            i => i%up
        END DO

        col%left%right => col
        col%right%left => col
    END SUBROUTINE

    INTEGER FUNCTION Callback() RESULT(res)
        CHARACTER(AR) :: s = REPEAT('.', 81)

        DO i=1,solution_size
            j= 1 + solution(i)/SZ
            s(j:j) = ACHAR(CSTART + MOD(solution(i),SZ))
        END DO
        PRINT ("(A)"), s
        res = ONESOLUTION
    END FUNCTION

    SUBROUTINE Reset
        INTEGER :: n, row
        DO WHILE (solution_size > 0)
            row = solution(solution_size)
            DO n=rows(row+1)-1,rows(row),-1
                CALL UncoverColumn(header(n)%column)
            END DO
            solution_size = solution_size - 1
        END DO
    END SUBROUTINE

END PROGRAM
