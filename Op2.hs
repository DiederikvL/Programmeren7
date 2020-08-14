{-|
Module : Op2
Omschrijving : Opdracht 2
Inleverpoging : Eerste kans
Student naam : Ricardo Mokveld, Diederik van Linden
Nummer : 0971051, 0970665
Klas : TI2C
-}

import Data.Char


-- Grootste gemene deler
euclid::Integer->Integer->Integer
euclid x y
    |modu == 0 = y
    |modu > 0 = euclid y modu
    where modu = x `mod` y
 
-- Extended euclidean algoritm 

egcd::Integer->Integer->(Integer,Integer,Integer)
egcd 0 b = (b, 0, 1)
egcd a b =  
    let (g, s, t) = egcd (b `mod` a) a
    in (g, t - (b `div` a) * s, s)
   
mid (_, x, _) = x


--Alleen positieve uitkomst met ecgd
posegcd::Integer->Integer->Integer
posegcd e m
    |d<0 = d+m
    |otherwise = d
    where d = mid $egcd e m 
   
--2
p = 101
q = 317
m = p*q
m' = (p-1)*(q-1)
e = 24053
d = posegcd e m'
 


-- Encryptie en depcryptie van 1 letter.
rsaencrypt::(Integer,Integer)->Integer->Integer
rsaencrypt (e,m) x = ((x))^e `mod` m
 
rsadecrypt::(Integer,Integer)->Integer->Char
rsadecrypt (d,m) x = chr(fromIntegral(x^d `mod` m))
 


--5
--  Bij het versleutelen van het bericht met de prive sleutel van Alcie en Bob zijn 
--  publieke sleutel kan het bericht worden ontsleutelt met Bob zijn prive sleutel en die van Alice.