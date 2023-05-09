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
nombresDeUsuariosAux :: [Usuario] -> [String]
nombresDeUsuariosAux [] = []
nombresDeUsuariosAux (x:xs) = nombreDeUsuario x : nombresDeUsuariosAux xs

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios (us,rs,ps) = nombresDeUsuariosAux us

-- describir qué hace la función: La funcion ultiliza una funcion auxiliar la cual recibe una secuencia (usuarios) y 
-- extrae el nombre de cada usuario de esa secuencia, luego en "nombresDeUsuarios" simplemente recibe la secuencia de
-- la red social y extrae el primer elemento que es usuarios y llama a la funcion auxiliar.

amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe = undefined

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos = undefined

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

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


-- RED SOCIAL

-- ([(1, "Paulo"), (2, "Jose"), (3, "Ana"), (4, "Maria"), (5, "Juan"),(6,"Roberto")],
-- [[(1, "Paulo"), (2, "Jose")],[(3, "Ana"), (4, "Maria")],[(5, "Juan"),(6,"Roberto")]],
-- [[(2, "Jose"),"PublicacionJose",[(3, "Ana"), (4, "Maria")]],[(1, "Paulo"),"PublicacionPaulo",[(2, "Jose"), (3, "Ana")]]])