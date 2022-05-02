module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes Empty = S.empty
nodes (Node n) = S.fromList [n]
nodes (Overlay a1 a2) = S.union (nodes a1) (nodes a2)
nodes (Connect a1 a2) = S.union (nodes a1) (nodes a2)

{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges Empty = S.empty
edges (Node n) = S.empty
edges (Overlay a1 a2) = S.union (edges a1) (edges a2)
edges (Connect a1 a2) = S.union (S.union (S.cartesianProduct (nodes a1) (nodes a2)) (edges a1)) (edges a2)

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors node Empty = S.empty
outNeighbors node (Node n) = S.empty
outNeighbors node (Overlay a1 a2) = S.union (outNeighbors node a1) (outNeighbors node a2)
outNeighbors node (Connect a1 a2) = S.union (S.union (S.map snd (S.filter (\x -> fst x == node) (S.cartesianProduct (nodes a1) (nodes a2)))) (outNeighbors node a1)) (outNeighbors node a2)

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node Empty = S.empty
inNeighbors node (Node n) = S.empty
inNeighbors node (Overlay a1 a2) = S.union (inNeighbors node a1) (inNeighbors node a2)
inNeighbors node (Connect a1 a2) = S.union (S.union (S.map fst (S.filter (\x -> snd x == node) (S.cartesianProduct (nodes a1) (nodes a2)))) (inNeighbors node a1)) (inNeighbors node a2)

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node graph = removeAux graph
    where
    removeAux Empty = Empty
    removeAux (Node n) = if n == node then Empty else Node n
    removeAux (Overlay a1 a2) = Overlay (removeAux a1) (removeAux a2)
    removeAux (Connect a1 a2) = Connect (removeAux a1) (removeAux a2)

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode old news graph = splitAux graph
    where
    splitAux Empty = Empty
    splitAux (Node n) = if n == old then foldl (Overlay) Empty (map Node news) else Node n
    splitAux (Overlay a1 a2) = Overlay (splitAux a1) (splitAux a2)
    splitAux (Connect a1 a2) = Connect (splitAux a1) (splitAux a2)

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node graph = mergeAux graph
    where
    mergeAux Empty = Empty
    mergeAux (Node n) = if prop n then Node node else Node n
    mergeAux (Overlay a1 a2) = Overlay (mergeAux a1) (mergeAux a2)
    mergeAux (Connect a1 a2) = Connect (mergeAux a1) (mergeAux a2)
