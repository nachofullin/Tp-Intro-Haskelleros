module Iap1 where

-- Completar con los datos del grupo
--
-- Nombre de Grupo: xx
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios

-- describir qué hace la función: La funcion ultiliza una funcion auxiliar la cual recibe una secuencia (usuarios) y
-- extrae el nombre de cada usuario de esa secuencia, luego en "nombresDeUsuarios" simplemente recibe la secuencia de
-- la red social y extrae el primer elemento que es usuarios y llama a la funcion auxiliar.

nombresDeUsuariosAux :: [Usuario] -> [String]
nombresDeUsuariosAux [] = []
nombresDeUsuariosAux (x:xs) = nombreDeUsuario x : nombresDeUsuariosAux xs

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios (us,rs,ps) = nombresDeUsuariosAux us


-- Toma una RedSocial y un Usuario y devuelve la lista de usuarios que se relacionan con el primero
-- Para ello usamos una auxiliar "sonAmigos" que toma las relaciones de la RedSocial y chequea quienes son los amigos usuario dado
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (us, rs, ps) n = amigos (relaciones(us, rs, ps)) n

amigos :: [Relacion] -> Usuario -> [Usuario]
amigos [] nm = []
amigos (x:xs) n | n == fst x = snd x : amigos xs n
                | n == snd x = fst x : amigos xs n
                | otherwise = amigos xs n

-- Toma una red social y un usuario y devuelve la cantidad de relaciones/amigos que tiene ese usuario
-- Usa una funcion auxiliar que cuenta la cantidad de relaciones que tiene dicho usuario
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos (us, rs, ps) n = contadorDeAmigos (amigosDe (us, rs, ps) n)

contadorDeAmigos :: [Usuario] -> Int
contadorDeAmigos [] = 0
contadorDeAmigos (x:xs) | xs == [] = 1
                        | otherwise = contadorDeAmigos xs + 1

-- Toma una red social y compara la cantidad de amigos de cada usuario para devolver el usuario con mayor cantidad
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ((x:xs), rs, ps) | xs == [] = x
                                     | cantidadDeAmigos ((x:xs), rs, ps) x > cantidadDeAmigos ((x:xs), rs, ps) (head(xs)) = usuarioConMasAmigos ((x : tail(xs)), rs, ps)
                                     | otherwise = usuarioConMasAmigos (xs, rs, ps)

-- Toma una red social y revisa si algun usuario tiene mas de 1000000 de amigos
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([], _, _) = False
estaRobertoCarlos ((x:xs), rs, ps) | (cantidadDeAmigos ((x:xs), rs, ps) x) > 1000000 = True
                                   | otherwise = estaRobertoCarlos (xs, rs, ps)

-- describir qué hace la función: .....
-- estaRobertoCarlos :: RedSocial -> Bool
-- estaRobertoCarlos [] = False
-- estaRobertoCarlos (x:xs) | x == "RobertoCarlos" = True
--                          | otherwise = estaRobertoCarlos (xs)

-- Dada una red social y un usuario, la funcion principal extrae las publicaciones de la red social
-- Y una funcion auxiliar que devuelve todas las publicaciones de ese usuario
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe rd n = publicacionesDelUsuario (publicaciones(rd)) n

publicacionesDelUsuario :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDelUsuario [] _ = []
publicacionesDelUsuario (x:xs) n | usuarioDePublicacion x == n = x : publicacionesDelUsuario xs n
             | otherwise = publicacionesDelUsuario xs n

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA (_, _, []) n = []
publicacionesQueLeGustanA (us, rs, (x:xs)) n | leGustaLaPublicacionA x n == True = x : publicacionesQueLeGustanA (us, rs, xs) n
                                             | otherwise = publicacionesQueLeGustanA (us, rs, xs) n

leGustaLaPublicacionA :: Publicacion -> Usuario -> Bool
leGustaLaPublicacionA (us, txt, []) n = False
leGustaLaPublicacionA (us, txt, (x:xs)) n | x == n = True
                                          | otherwise = leGustaLaPublicacionA (us, txt, xs) n

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones rs us1 us2 = publicacionesQueLeGustanA rs us1 == publicacionesQueLeGustanA rs us2

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel rs a = usuariosFieles (publicacionesDe rs a) (likesDePublicacion (head(publicacionesDe rs a)))

usuariosFieles :: [Publicacion] -> [Usuario] -> Bool
usuariosFieles pbs [] = False
usuariosFieles pbs likes | esFiel pbs (head(likes)) == True = True
                         | otherwise = usuariosFieles pbs (tail(likes))

esFiel :: [Publicacion] -> Usuario -> Bool
esFiel [] n = True
esFiel (x:xs) n | leGustaLaPublicacionA x n == True = esFiel xs n
                | leGustaLaPublicacionA x n == False = False

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos rs us1 us2 = verificador rs us2 (amigosDe rs us1) []

verificador :: RedSocial -> Usuario -> [Usuario] -> [Usuario] -> Bool
verificador rs us2 [] lista = False
verificador rs us2 as lista | pertenece us2 (listaVerificada as lista) = True
                            | otherwise = verificador2 rs us2 as (listaVerificada as lista)

verificador2 :: RedSocial -> Usuario -> [Usuario] -> [Usuario] -> Bool
verificador2 rs us2 as lista | verificador rs us2 (amigosDe rs (head(as))) lista = True
                             | otherwise = pertenece (amigosDe rs (head(as))) lista = verificador rs us2 tail(as) lista

pertenece :: Usuario -> [Usuario] -> Bool
pertenece us [] = False
pertenece us (x:xs) | x == us = True
                    | otherwise = pertenece us xs

listaVerificada :: [Usuario] -> [Usuario] -> [Usuario]
listaVerificada [] ys = ys
listaVerificada (x:xs) ys | pertenece x ys = listaVerificada xs ys
                          | otherwise = listaVerificada xs (x:ys)