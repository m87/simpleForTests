program assertExample
    use sft_AssertModule
    use sft_SuiteModule

    type(sft_Suite) :: testSuite
    call sft_init(testSuite)
    
    !call sft_run(testSuite, example)
    !call sft_run(testSuite, example2)
    !call sft_run(testSuite, example3)
    !call sft_run(testSuite, example4)
    call sft_run(testSuite, example4)
    
    call sft_summary(testSuite)

    contains 
        
        function example() result(res)
            logical :: res
            res = sft_assertEqual(10, 120)
        end function example
        
        function example2() result(res)
            logical :: res
            res = sft_assertEqual(10.0, 10.0) .and. sft_assertEqual(10, 10)
        end function example2

        function example3() result(res)
            logical :: res
            res = sft_assertFalse(.TRUE.)
        end function example3
 
        function example4() result(res)
            logical :: res
            res = sft_assertArrayEqual((/1,2,3/),(/1,2,3/))
        end function example4
               

end program assertExample
