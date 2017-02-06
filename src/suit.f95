module SFT_Suit
      use SFT_Utils
      public :: run, init, summary

      type suit
            integer :: passed
            integer :: allTests
      end type suit

      contains
        
            subroutine run(this, test)
                interface
                    function test() result(res)
                        logical :: res
                    end function test
                end interface
                type(suit) :: this
                logical :: res
                
                if (test()) then
                    this%passed = this%passed + 1
                end if
                    
                this%allTests = this%allTests + 1

            end subroutine run

            subroutine init(this)
                type(suit) :: this
                
                this%passed = 0
                this%allTests = 0
            end subroutine init

            subroutine summary(this)
                type(suit) :: this
                character(len=100) strFails, strAllTests
                write(strFails,*) this%allTests - this%passed
                write(strAllTests,*) this%allTests 
            
                if (this%passed .ne. this%allTests) then
                    call printError('Failures :: ' // adjustl(trim(strFails)))
                else
                    call printSuccess('All tests passed! :: ' // adjustl(trim(strAllTests)))
                end if
            end subroutine
    

end module SFT_Suit
