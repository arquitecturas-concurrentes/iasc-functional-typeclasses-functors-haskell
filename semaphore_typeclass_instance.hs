ata Semaforo = Verde | Amarillo | Rojo

instance Eq Semaforo where
    Rojo == Rojo = True  
    Amarillo == Amarillo = True  
    Verde == Verde = True  
    _ == _ = False  
