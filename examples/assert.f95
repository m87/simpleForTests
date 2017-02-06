program assertExample
    use SFT_Assert
    use SFT_Suit

    type(suit) :: testSuit
    call init(testSuit)
    
    call run(testSuit, example)
    call run(testSuit, example2)
    call run(testSuit, example3)
    call run(testSuit, example4)
    
    call summary(testSuit)

    contains 
        
        function example() result(res)
            logical :: res
            res = assertEqual(10, 120)
        end function example
        
        function example2() result(res)
            logical :: res
            res = assertEqual(10.0, 10.0) .and. assertEqual(10, 10)
        end function example2

        function example3() result(res)
            logical :: res
            res = assertFalse(.TRUE.)
        end function example3
 
        function example4() result(res)
            logical :: res
            res = assertTrue(.FALSE.)
        end function example4
               

end program assertExample
