module SFT_Assert
    use SFT_Utils

    public :: assertEqual, assertTrue, assertFalse

    interface assertEqual
        module procedure assertEqual_real, assertEqual_int
    end interface assertEqual

    contains

        function assertEqual_real(A, B) result(res)
            implicit none
            real :: A
            real :: B
            logical :: res
            character(len=512) strA, strB
            write(strA,*) A
            write(strB,*) B
            
            res = (A .eq. B)
            if (.not. res) then
                call printError('Expected [' // trim(strA) // '] was [' // trim(strB) // ']')
            end if


        end function assertEqual_real

        function assertEqual_int(A, B) result(res)
            implicit none
            integer :: A
            integer :: B
            logical :: res
            character(len=512) strA, strB
            write(strA,*) A
            write(strB,*) B

            res = (A .eq. B)
            if (.not. res) then
                call printError('Expected [' // trim(strA) // '] was [' // trim(strB) // ']')
            end if
            
        end function assertEqual_int

        function assertTrue(A) result(res)
            implicit none
            logical :: res
            logical :: A

            res = A
            if (.not. res) then
                call printError('Expected [.TRUE.] was [.FALSE.]')
            end if
        end function assertTrue

        function assertFalse(A) result(res)
            implicit none
            logical :: res
            logical :: A

            res = .not. A
            if (A) then
                call printError('Expected [.FALSE.] was [.TRUE.]')
            end if
        end function assertFalse


end module SFT_Assert
