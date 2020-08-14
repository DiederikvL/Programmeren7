{-|
Module : Op1
Omschrijving : Opdracht 1
Inleverpoging : Eerste kans
Student naam : Ricardo Mokveld, Diederik van Linden
Nummer : 0971051, 0970665
Klas : TI2C
-}

module Op1 where
 
-- Bereken de faculteit
faca::Int->Int
faca 0 = 1
faca n = n * faca (n - 1)
 


-- Bereken faculteit met guards
facb::Int->Int
facb n  |  n < 2 = 1 
        |  otherwise = n * facb (n - 1) 

   
-- Bereken nulpunten met 'let' 'in'
nulpuntena::Double->Double->Double->[Double]
nulpuntena a b c = 
    let
        discriminant = b*b - (4 * a * c)
        rootp = (-b + sqrt(discriminant))/ (2 * a)
        rootm = (-b - sqrt(discriminant))/ (2 * a)
    in
        [rootm, rootp] 


-- Bereken nulpunten met 'where' en guards
nulpuntenb a b c    
            | discriminant < 0 = error "function has no roots" 
            | discriminant == 0 = [rooto]
            | discriminant > 0 = [rootm, rootp]
        where
        discriminant = (b*b) - (4 * a * c)
        rooto = (-b) / (2 * a)
        rootp = (-b + sqrt(discriminant))/ (2 * a)
        rootm = (-b - sqrt(discriminant))/ (2 * a)


 


--De uitkomsten die deelbaar zijn door 5.
worpen::[(Int,Int,Int)]
worpen = [(x,y,z)|x<-[1..6],y<-[1..6],z<-[1..6],((x+y+z) `mod` 5) == 0]
 
worpenaantal::Int
worpenaantal = length worpen


-- De uitkomsten die deelbaar zijn door n. 
worpenN::Int->[(Int,Int,Int)]
worpenN n = [(x,y,z)|x<-[1..6],y<-[1..6],z<-[1..6],((x+y+z) `mod` n) == 0]
 
worpenNaantal::Int->Int
worpenNaantal n = length (worpenN n)