fun factorial(x: int) = 
    if x = 0
    then 1
    else if x < 0
    then x * factorial(x + 1)
    else x * factorial(x - 1)

    