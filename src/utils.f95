module SFT_Utils
    
    public :: printError, printSuccess
    
    contains

        subroutine printError(message)
            implicit none
            character(len=*), intent(in) :: message

            write (*,*) achar(27), '[31m>>', message, achar(27), '[0m'

        end subroutine printError

        subroutine printSuccess(message)
            implicit none
            character(len=*), intent(in) :: message

            write (*,*) achar(27), '[32m>>', message, achar(27), '[0m'

        end subroutine printSuccess


end module SFT_Utils
