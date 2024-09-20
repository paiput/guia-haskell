import Test.HUnit


{-
Puedo tener los archivos separados y llamarlos por modulo
ejemplo: 

un archivo MultiplosDeN.hs donde defino
module MultiplosDeN where

y luego lo importo en otro archivo
import MultiplosDeN (tiene que estar en la misma carpeta)
-}


-- La funcion que queremos probar
valorAbsoluto :: Int -> Int
valorAbsoluto x 
  | x >= 0 = div 1 x
  | otherwise = (-x)
-- Las pruebas unitarias
testSuiteValorAbs = test [
    "casoPositivo" ~: (valorAbsoluto 1) ~?= 1,
    "casoNegativo" ~: (valorAbsoluto (-5)) ~?= 5,
    "casoCero" ~: (valorAbsoluto 0) ~?= 0
  ]
-- Corre todas las pruebas
correrTests = runTestTT testSuiteValorAbs





-- La funcion que queremos probar
multiplosDeN :: Int -> [Int] -> [Int]
multiplosDeN _ [] = []
multiplosDeN n (x:xs)
  | n == 0 && x == 0 = [0]
  | n /= 0 && mod x n == 0 = x : pasoRecursivo
  | otherwise = pasoRecursivo
  where pasoRecursivo = multiplosDeN n xs



-- Las pruebas unitarias
testSuiteMultiplosDeN = test [
    "lista vacia" ~: (multiplosDeN 4 []) ~?= [],
    "valor 0, mult 0" ~: (multiplosDeN 0 [1,3]) ~?= [],
    "valor 0, mult 1" ~: (multiplosDeN 0 [-1,0,9]) ~?= [0],
    "valor < 0, mult 0" ~: (multiplosDeN (-3) [20,13,-4]) ~?= [],
    "valor < 0, mult 1" ~: (multiplosDeN (-8) [9,-16,7]) ~?= [-16],
    "valor < 0, mult > 1" ~: (multiplosDeN (-7) [0,-14,15]) ~?=
    [0,-14],
    "valor > 0, mult 0" ~: (multiplosDeN 5 [4,-7,9]) ~?= [],
    "valor > 0, mult 1" ~: (multiplosDeN 7 [7,8,-9]) ~?= [7],
    "valor > 0, mult > 1" ~: (multiplosDeN 11 [-22,10,33]) ~?= [-22,33]
  ]
-- Correr todas las pruebas
correrTests2 = runTestTT testSuiteMultiplosDeN