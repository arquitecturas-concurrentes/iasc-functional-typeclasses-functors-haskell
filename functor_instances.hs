-- instance Functor [] where
--     fmap = map

instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing  

instance Functor Tree where  
    fmap f EmptyTree = EmptyTree  
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)


