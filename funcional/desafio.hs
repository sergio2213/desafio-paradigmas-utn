-- length
length :: [a] -> Int
length [] = 0
length lista = foldr (\_ acumulador -> acumulador + 1) 0 lista

-- map
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f lista = foldr (\elemento resParcial -> (f elemento : resParcial)) [] lista
map f = foldr ((:) . f) [] -- implementación corta

-- all
all :: (a -> Bool) -> [a] -> Bool
all _ [] = False
all f lista = foldr (\elemento resParcial -> resParcial && f elemento) True lista
all f = foldr ((&&) . f) True -- implementación corta

-- any
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f lista = foldr (\elemento resParcial -> resParcial || f elemento) False lista
any f = foldr ((||) . f) False -- implementación corta

-- filter
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f lista = foldr (\elemento resParcial -> if f elemento then (elemento : resParcial) else resParcial) [] lista

-- find
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f lista = foldr (\elemento resParcial -> if (resParcial == Nothing && f elemento) then Just elemento else Nothing) Nothing lista