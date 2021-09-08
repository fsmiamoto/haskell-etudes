module Box where

data SomeValues = First | Second | Third | Fourth Int | Fifth [Bool] (Int, String)

data Box a = Box a
    deriving ( Show, Eq )

matamosca :: Box Bool -> String
matamosca (Box True)  = "Eh o true boxado"
matamosca (Box False) = "Eh o false boxado"
matamosca (Box x)     = "Eh algo boxado"
matamosca x           = "oi"

bottom :: a
bottom = bottom
