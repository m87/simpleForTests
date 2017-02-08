module sft_SuiteModule
      use sft_UtilsModule
      public :: sft_run, sft_init, sft_summary

      type sft_Suite
            integer :: passed
            integer :: allTests
      end type sft_Suite

      contains
        
            subroutine sft_run(this, test)
                interface
                    function test() result(res)
                        logical :: res
                    end function test
                end interface
                type(sft_Suite) :: this
                logical :: res
                
                if (test()) then
                    this%passed = this%passed + 1
                end if
                    
                this%allTests = this%allTests + 1

            end subroutine sft_run

            subroutine sft_init(this)
                type(sft_Suite) :: this
                
                this%passed = 0
                this%allTests = 0
            end subroutine sft_init

            subroutine sft_summary(this)
                type(sft_Suite) :: this
                character(len=100) strFails, strAllTests
                write(strFails,*) this%allTests - this%passed
                write(strAllTests,*) this%allTests 
            
                if (this%passed .ne. this%allTests) then
                    call sft_printError('Failures :: ' // adjustl(trim(strFails)))
                    stop 9
                else
                    call sft_printSuccess('All tests passed! :: ' // adjustl(trim(strAllTests)))
                end if
            end subroutine
    

end module sft_SuiteModule
