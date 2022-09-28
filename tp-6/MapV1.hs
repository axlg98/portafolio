module MapV1
 (Map,emptyM,isEmptyM,assocM,lookupM,deleteM,keys,m1,m2)
where

--------[(a,b)]Int sin  repetidos.

data Map k v = M [(k,v)]
m1 :: Map String Int
m1 = M [("Axel",1998),("Ivan",2000)]

m2 :: Map String Int
m2 = M []

instance (Show k, Show v) => Show (Map k v) where
    show (M kvs) = "{ " ++ mostrar kvs ++ " }"

mostrar []       = ""
mostrar [(k,v)]    = showKV (k,v) 
mostrar((k, v):kvs) =  showKV (k,v) ++ " , " ++ mostrar kvs

showKV (k,v) = " " ++ show k ++ "->" ++ show v

emptyM :: Map k v
--Propósito: devuelve un map vacío
emptyM = M []

isEmptyM :: Map k v ->Bool
isEmptyM (M kvs) = null kvs

assocM :: Eq k => k -> v -> Map k v -> Map k v
--Propósito: agrega una asociación clave-valor al map.
assocM x y (M kvs) = M (agregarAlMap x y kvs)

agregarAlMap :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
agregarAlMap x y []          = [(x,y)]
agregarAlMap x y ((k,v):kvs) = if x==k
                         then (k,y) : kvs
                         else (k,v) : agregarAlMap x y kvs

lookupM :: Eq k => k -> Map k v -> Maybe v
--Propósito: encuentra un valor dado una clave.
lookupM x (M kvs) = lookIn x kvs

lookIn :: Eq k => k -> [(k,v)] -> Maybe v
lookIn x []          = Nothing
lookIn x ((k,v):kvs) = if x==k
                       then Just v
                       else lookIn x kvs

deleteM :: Eq k => k -> Map k v -> Map k v
--Propósito: borra una asociación dada una clave.
deleteM x (M kvs) = M (quitarElElemento x kvs) 

quitarElElemento :: Eq k => k -> [(k,v)] -> [(k,v)]
quitarElElemento x []          = []
quitarElElemento x ((k,v):kvs) = if x==k
                                 then kvs
                                 else (k,v) : quitarElElemento x kvs

keys :: Map k v -> [k]
--Propósito: devuelve las claves del map.
keys (M kvs) = cantDeKeys kvs 

cantDeKeys :: [(k,v)] -> [k]
cantDeKeys []          = []
cantDeKeys ((k,v):kvs) = k : cantDeKeys kvs