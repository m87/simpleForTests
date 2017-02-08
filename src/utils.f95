module sft_UtilsModule
    
    public :: sft_printError, sft_printSuccess
    
    contains

        subroutine sft_printError(message)
            implicit none
            character(len=*), intent(in) :: message

            write (*,*) achar(27), '[31m>>', message, achar(27), '[0m'

        end subroutine sft_printError

        subroutine sft_printSuccess(message)
            implicit none
            character(len=*), intent(in) :: message

            write (*,*) achar(27), '[32m>>', message, achar(27), '[0m'

        end subroutine sft_printSuccess

end module sft_UtilsModule
