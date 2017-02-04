program assertExample
    use SFT_Assert
    use SFT_Suit

    type(suit) :: testSuit
    call init(testSuit)
    
    call run(testSuit, example2)
    call run(testSuit, example2)
    call run(testSuit, example2)
    
    call summary(testSuit)

    contains 
        
        function example() result(res)
            logical :: res
            res = assertEqual(10.0, 120.0) .and. assertEqual(10, 120)
        end function example
        
        function example2() result(res)
            logical :: res
            res = assertEqual(10.0, 10.0) .and. assertEqual(10, 10)
        end function example2


end program assertExample
