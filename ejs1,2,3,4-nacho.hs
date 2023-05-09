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

--
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios ([], _, _) = []
nombresDeUsuarios ((x:xs), rs, ps) | xs == [] = [nombreDeUsuario x]
                                   | otherwise = nombreDeUsuario x : nombresDeUsuarios (xs, rs, ps)

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe (us, rs, ps) n = sonAmigos (relaciones(us, rs, ps)) n

sonAmigos :: [Relacion] -> Usuario -> [Usuario]
sonAmigos [] nm = []
sonAmigos (x:xs) n | xs == [] && n == fst x = [snd x]
                   | xs == [] && n == snd x = [fst x]
                   | xs == [] && n /= fst x &&  n /= snd x = []
                   | n == fst x = snd x : sonAmigos xs n
                   | n == snd x = fst x : sonAmigos xs n
                   | otherwise = sonAmigos xs n

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos (us, rs, ps) n = contadorDeAmigos (amigosDe (us, rs, ps) n)

contadorDeAmigos :: [Usuario] -> Int
contadorDeAmigos [] = 0
contadorDeAmigos (x:xs) | xs == [] = 1
                        | otherwise = contadorDeAmigos xs + 1

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos ((x:xs), rs, ps) | xs == [] = x
                                     | cantidadDeAmigos ((x:xs), rs, ps) x > cantidadDeAmigos ((x:xs), rs, ps) (head(xs)) = usuarioConMasAmigos ((x : tail(xs)), rs, ps)
                                     | otherwise = usuarioConMasAmigos (xs, rs, ps)

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined
