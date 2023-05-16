module TestSolucion where
import Test.HUnit
import Solucion

main = runTestTT tests
-------------- test nombres de usuarios ------------------------
testnombresUsuarios = runTestTT testsNombresUsuarios
testsNombresUsuarios = test [
    "red social vacía" ~: (nombresDeUsuarios redSocialVacia) ~?= [],

    "red social un usuario" ~: (nombresDeUsuarios redSocialUnUsuario) ~?= ["Juan"],

    "red social varios usuarios" ~: (nombresDeUsuarios redA) ~?= ["Juan", "Natalia", "Pedro", "Mariela"]
 ]

-------------- test amigosDe ------------------------
testAmigosDe = runTestTT testsAmigosDe
testsAmigosDe = test [
    "red social vacía" ~: (amigosDe redSocialVacia usuario1) ~?= [],

    "red social sin amistades del usuario" ~: (amigosDe redB usuario5 ) ~?= [],

    "red social con amistades del usuario" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4]
 ]

-------------- test amigosDe ------------------------
testCantidadDeAmigos = runTestTT testsCantidadDeAmigos
testsCantidadDeAmigos = test [
    "red social sin usuarios" ~: (cantidadDeAmigos redSocialVacia usuario1) ~?= 0,
    "red social sin amigos de" ~: (cantidadDeAmigos redB usuario5) ~?= 0,
    "red social con amigos de" ~: (cantidadDeAmigos redA usuario1) ~?= 2
 ]

 -------------- test usuarioConMasAmigos ------------------------
testUsuarioConMasAmigos = runTestTT testsUsuarioConMasAmigos
testsUsuarioConMasAmigos = test [
    "red social con un solo usuario" ~: (usuarioConMasAmigos redSocialUnUsuario ) ~?= usuario1,
    "red social sin relaciones" ~: (usuarioConMasAmigos redSinRelaciones ) ~?= usuario1,
    "red social con misma cantidad de amigos en todos los usuarios" ~: (usuarioConMasAmigos redConMismaCantAmigos ) ~?= usuario1,
    "red social con alguien con mas amigos" ~: (usuarioConMasAmigos redB ) ~?= usuario2
 ]

-------------- test estaRobertoCarlos ------------------------
testEstaRobertoCarlos = runTestTT testsEstaRobertoCarlos
testsEstaRobertoCarlos = test [
    "red social sin usuarios" ~: (estaRobertoCarlos redSocialVacia ) ~?= False,
    "red social con usuarios que no cumplan" ~: (estaRobertoCarlos redB ) ~?= False,
    "red social con usuarios que cumplan" ~: (estaRobertoCarlos redA ) ~?= True
 ]

 -------------- test PublicacionesDe ------------------------
testPublicacionesDe = runTestTT testsPublicacionesDe
testsPublicacionesDe = test [
    "red social sin publicaciones" ~: (publicacionesDe redSinPublicaciones  usuario2) ~?= [],
    "red social con una sola publicacion del usuario" ~: (publicacionesDe redUnaPublicacionDeUsuario  usuario3) ~?= [publicacion3_1],
    "red social con muchas publicaciones del usuario" ~: (publicacionesDe redA usuario1) ~?= [publicacion1_1, publicacion1_2]
 ]

 -------------- test PublicacionesQueLeGustanA ------------------------
testPublicacionesQueLeGustanA = runTestTT testsPublicacionesQueLeGustanA
testsPublicacionesQueLeGustanA = test [
    "red social sin publicaciones" ~: (publicacionesQueLeGustanA redSinPublicaciones  usuario2) ~?= [],
    "red social sin publicaciones que le gusten al usuario" ~: (publicacionesQueLeGustanA redB  usuario1) ~?= [],
    "red social con una sola publicacion que le guste al usuario" ~: (publicacionesQueLeGustanA redUnaPublicacionDeUsuario usuario2) ~?= [publicacion1_3],
    "red social con varias publicaciones que le gusten al usuario" ~: (publicacionesQueLeGustanA redA usuario4) ~?= [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2]
 ]
 -------------- test estaRobertoCarlos ------------------------
testLesGustanLasMismasPublicaciones = runTestTT testsLesGustanLasMismasPublicaciones
testsLesGustanLasMismasPublicaciones = test [
    "red social sin publicaciones" ~: (lesGustanLasMismasPublicaciones redSinPublicaciones  usuario2 usuario1) ~?= True,
    "red social sin publicaciones que le gusten a los usuarios" ~: (lesGustanLasMismasPublicaciones redB  usuario1 usuario3) ~?= True,
    "red social con una sola publicacion que le guste a ambos usuarios" ~: (lesGustanLasMismasPublicaciones redMismaCantPublicacionesLikeadasUnaSola usuario2 usuario4) ~?= True,
    "red social con varias publicaciones que le gusten a ambos usuarios" ~: (lesGustanLasMismasPublicaciones redMismaCantPublicacionesLikeadas usuario2 usuario5) ~?= True,
    "red social que le gusten mas publicacion al primer usuario" ~: (lesGustanLasMismasPublicaciones redA usuario4 usuario2) ~?= False,
    "red social que le gusten mas publicacion al segundo usuario" ~: (lesGustanLasMismasPublicaciones redA usuario2 usuario4) ~?= False
 ]
tests = test [
    " nombresDeUsuarios 1" ~: (nombresDeUsuarios redA) ~?= ["Juan","Natalia","Pedro","Mariela"],

    " amigosDe 1" ~: (amigosDe redA usuario1) ~?= [usuario2, usuario4],

    " cantidadDeAmigos 1" ~: (cantidadDeAmigos redA usuario1) ~?= 2,

    " usuarioConMasAmigos 1" ~: expectAny (usuarioConMasAmigos redA) [usuario2, usuario4],

    " estaRobertoCarlos 1" ~: (estaRobertoCarlos redA) ~?= False,

    " publicacionesDe 1" ~: (publicacionesDe redA usuario2) ~?= [publicacion2_1, publicacion2_2],

    " publicacionesQueLeGustanA 1" ~: (publicacionesQueLeGustanA redA usuario1) ~?= [publicacion2_2, publicacion4_1],

    " lesGustanLasMismasPublicaciones 2" ~: (lesGustanLasMismasPublicaciones redB usuario1 usuario3) ~?= True,

    " tieneUnSeguidorFiel 1" ~: (tieneUnSeguidorFiel redA usuario1) ~?= True,

    " existeSecuenciaDeAmigos 1" ~: (existeSecuenciaDeAmigos redA usuario1 usuario3) ~?= True
 ]

expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)

-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

redSocialVacia = ([],[],[])
redSocialUnUsuario = ([usuario1],[],[])
redSinRelaciones = (usuariosB, [], publicacionesB)
redConMismaCantAmigos = ([usuario1, usuario2, usuario3], [relacion1_2, relacion2_3, relacion1_3], publicacionesB)
redSinPublicaciones = (usuariosB,relacionesB,[])
redUnaPublicacionDeUsuario = (usuariosB, relacionesB, [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1])
redMismaCantPublicacionesLikeadasUnaSola = (usuariosA, relacionesA, [publicacion1_1, publicacion3_1, publicacion4_2,publicacion4_3])
redMismaCantPublicacionesLikeadas = (usuariosB, relacionesB, [publicacion1_3, publicacion1_4, publicacion3_1, publicacion3_3])