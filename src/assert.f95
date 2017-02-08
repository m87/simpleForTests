module sft_AssertModule
    use sft_UtilsModule

    public :: sft_assertEqual, sft_assertTrue, sft_assertFalse

    interface sft_assertEqual
        module procedure sft_assertEqual_real, sft_assertEqual_int
    end interface sft_assertEqual

    contains

        function sft_assertEqual_real(A, B) result(res)
            implicit none
            real :: A
            real :: B
            logical :: res
            character(len=512) strA, strB
            write(strA,*) A
            write(strB,*) B
            
            res = (A .eq. B)
            if (.not. res) then
                call sft_printError('Expected [' // trim(strA) // '] was [' // trim(strB) // ']')
            end if


        end function sft_assertEqual_real

        function sft_assertEqual_int(A, B) result(res)
            implicit none
            integer :: A
            integer :: B
            logical :: res
            character(len=512) strA, strB
            write(strA,*) A
            write(strB,*) B

            res = (A .eq. B)
            if (.not. res) then
                call sft_printError('Expected [' // trim(strA) // '] was [' // trim(strB) // ']')
            end if
            
        end function sft_assertEqual_int

        function sft_assertTrue(A) result(res)
            implicit none
            logical :: res
            logical :: A

            res = A
            if (.not. res) then
                call sft_printError('Expected [.TRUE.] was [.FALSE.]')
            end if
        end function sft_assertTrue

        function sft_assertFalse(A) result(res)
            implicit none
            logical :: res
            logical :: A

            res = .not. A
            if (A) then
                call sft_printError('Expected [.FALSE.] was [.TRUE.]')
            end if
        end function sft_assertFalse

end module sft_AssertModule
