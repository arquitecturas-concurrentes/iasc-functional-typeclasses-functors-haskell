-- data Maybe a = Just a | Nothing
inversa 0 = Nothing
inversa a = Just (1/a)

--data Either a b = Left a | Right b
inversaEither 0 = Left "Division por cero"
inversaEither a = Right (1/a)
