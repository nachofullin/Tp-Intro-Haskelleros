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
amigosDe (us, rs, ps) n = sonAmigos (relaciones(us, rs, ps)) n

sonAmigos :: [Relacion] -> Usuario -> [Usuario]
sonAmigos [] nm = []
sonAmigos (x:xs) n | n == fst x = snd x : sonAmigos xs n
                   | n == snd x = fst x : sonAmigos xs n
                   | otherwise = sonAmigos xs n

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
tieneUnSeguidorFiel rs a = aux1 (publicacionesDe rs a) (likesDePublicacion (head(publicacionesDe rs a)))

aux1 :: [Publicacion] -> [Usuario] -> Bool
aux1 pbs [] = False
aux1 pbs likes | aux2 pbs (head(likes)) == True = True
               | otherwise = aux1 pbs (tail(likes))

aux2 :: [Publicacion] -> Usuario -> Bool
aux2 [] n = True
aux2 (x:xs) n | leGustaLaPublicacionA x n == True = aux2 xs n
              | leGustaLaPublicacionA x n == False = False

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined

