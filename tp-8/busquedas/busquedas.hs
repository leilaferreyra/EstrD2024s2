--1 
siguientesN :: Busqueda -> Int -> [(String,Int)]
--PROPOSITO: Dada una busqueda y un numero obtiene dicha cantidad de pares nombre de producto, precio (o tantas como hayas en la busqueda si
--hay menos que dicho numero). Se puede suponer que todos los productos de la busqueda tienen precio como atributo. 
--EFICIENCIA : O (n* (P + log P + log A))
siguientesN _ 0 = []
siguientesN bus n = let (prod, siguienteBus) = busqueda bus
                  in case prod of
                   Just p -> p : siguientesN siguienteBus (n-1)
                   Nothing -> []

--3 Definir como implementador de busqueda
Data Busqueda = B (Map String (Map String Int))  [Filtro]
--a 
registrar :: String -> Int -> Map String Int -> Busqueda -> Busqueda
-- PROPOSITO: Que dado un nombre de producto, su precio y un diccionario de atributos, si el producto y sus atributos satisfacen todos los filtros,
-- agrega el producto a la busqueda dada. Si el producto ya existe no lo agrega.
registrar nombre precio at (B mp filtros) =
    let atributos = assocM "precio" precio at
    in case lookupM nombre mp of
        Just _ -> B mp filtros
        Nothing -> agregarSiCumpleFiltros nombre atributos mp filtros

agregarSiCumpleFiltros :: String -> Map String Int -> Map String (Map String Int) -> [Filtro] -> Busqueda
agregarSiCumpleFiltros nombre atributos prods filtros =
    if aplicaAFiltros atributos filtros
    then B (assocM nombre atributos prods) filtros
    else B prods filtros

aplicaAFiltros :: Map String Int -> [Filtro] -> Bool
aplicaAFiltros _ [] = True
aplicaAFiltros atributos (f:fs) = aplica atributos f && aplicaAFiltros atributos fs


--B 
filtrar :: Filtro -> Busqueda -> Busqueda 
-- PROPOSITO: Que dados un filtro y una busqueda aplica el filtro a la lista actual de productos, pudiendo eliminar algunos, 
filtrar f(B mp fs) = B (filtrarBusqueda f (keys ps) ps) (f:fs)
ghjk
filtrarBusqueda :: Filtro -> [String] -> Map String (Map String Int) -> Map String (Map String Int)
filtrarBusqueda    _ [] mp = mp 
filtrarBusqueda    f (p:ps) mp = let arg = fromJust (lookupM p mp)
                                  in if aplica f arg 
                                    then filtrarBusqueda f ps mp 
                                    else filtrarBusqueda f ps (deleteM p mp)

