module GameSep29 exposing (..)

import Browser
import Html.Events
import Html.Attributes as Attr
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import TypesSep29 exposing (..)
import LightToolsSep29 exposing (..)
import Random
import AssocList as ADict 
import Dict as Dict exposing (Dict)

main = Browser.element {init = init
                       ,update = update
                       ,view = view
                       ,subscriptions = subscriptions
                       }

size = 6 -- if the size is an even integer and p >2, then the laplacian is invertible.
p = 3
initLitX = 0
initLitY = 0
init: () -> (Model, Cmd Msg)
init _ = ( allOffButOne(initLitX,initLitY,1) size p
            --allOffButOne ((size//4), (size//4), 1) size p
          ,Cmd.none
          )

randomSeq: Random.Generator (List Int)
randomSeq  =
    Random.list (size//2) (Random.int 0 (p-1))

allOffButOne: Position -> Int -> Int -> Model
allOffButOne pos boardSize levelNum =
    let
        pos0 = List.concat <|
                        List.map (\x ->
                                      List.concat <| 
                                      List.map (\y ->
                                                    if x+y < boardSize then
                                                        [(x, y, 0)]
                                                    else
                                                        []
                                               )
                                      (List.range 0 (boardSize-1))
                                 )
                       (List.range 0 (boardSize-1))
        pos1 = List.concat <|
                  List.map (\x ->
                                List.concat <|
                                List.map (\y ->
                                              if x+y+1 < boardSize then
                                                  [(x, y, 1)]
                                              else
                                                  []
                                         )
                                (List.range 0 (boardSize-1))
                           )
                      (List.range 0 (boardSize-1))
        positions = pos0++pos1
        lights= List.foldl (\pt dict -> if pt == pos then
                                            Dict.insert pt {brightness=1, numClick=Nothing} dict
                                        else
                                            Dict.insert pt {brightness=0, numClick=Nothing} dict
                           ) Dict.empty  positions
    in
        Model lights [] Dict.empty boardSize levelNum
    
        
initialize: List Int -> Int -> Int -> Model
initialize halfSeq boardSize levelNum =
    let
        undetermined0 = List.concat <|
                        List.map (\x ->
                                      List.concat <| 
                                      List.map (\y ->
                                                    if x+y < boardSize then
                                                        [(x, y, 0)]
                                                    else
                                                        []
                                               )
                                      (List.range 0 (boardSize-1))
                                 )
                       (List.range 1 (boardSize-1))
        undetermined1 = List.concat <|
                        List.map (\x ->
                                      List.concat <|
                                      List.map (\y ->
                                                    if x+y+1 < boardSize then
                                                        [(x, y, 1)]
                                                    else
                                                        []
                                               )
                                      (List.range 0 (boardSize-1))
                                 )
                       (List.range 0 (boardSize-1))
        undetermined = undetermined0++undetermined1
        fullSeq =
            let
                evenSum = List.sum <| List.indexedMap (\i a -> if (modBy 2 i) == 0 && i>0 then
                                                                   a
                                                               else
                                                                   0
                                                      ) halfSeq
                head = modBy levelNum <| boardSize*levelNum - evenSum
                newHalfSeq = head::(List.drop 1 halfSeq)
            in
                List.concat [newHalfSeq
                              ,if (modBy 2 boardSize) == 0 then
                                   []
                               else
                                   [0]
                              ,List.reverse <| List.indexedMap (\i a -> modBy levelNum (4*levelNum+(-1)^i*a)) newHalfSeq
                              ]
        determined = List.foldl (\r dict -> Dict.insert (0, Dict.size dict, 0) {brightness=r, numClick=Nothing} dict
                                 ) Dict.empty  fullSeq
        dummy = Debug.log "" <| undetermined
        dummy2 = Debug.log "" <| determined
    in
        Model determined undetermined Dict.empty boardSize levelNum


adjacent: Point -> Point -> Bool
adjacent v w = 
    let
        dx = v.x - w.x
        dy = v.y - w.y
    in
        List.member (dx, dy) [(0,1), (1,0), (1,-1), (-1, 1), (-1, 0), (0,-1)]

colIndices: Int -> List Point
colIndices n =
    (rowIndices n)++[{x=size+1, y=size+1}]

rowIndices: Int -> List Point
rowIndices n = 
    (List.filter (\q -> q.x + q.y <= (n-2)) <|
         List.concat <|
         List.map (\y -> 
                       List.map (\x -> Point x y) (List.range 0 (n-2))
                  ) (List.range 0 (n-2))
    )
    
insert: (Point, Point) -> Tmatrix -> Tmatrix
insert (x, y) tmatrix = 
    if x == y then
         ADict.insert (x, y) 2 tmatrix 
    else if (adjacent x y) then 
            ADict.insert (x, y) 1 tmatrix
        else
            tmatrix

pairs : Int -> List (Point,Point)
pairs n  =
    List.concat <|
    List.map (\x -> List.map
                    (\y -> (x,y)) 
                    (colIndices n)
            ) (rowIndices n) 

type alias Tmatrix = ADict.Dict (Point, Point) Int
makeTmatrix: Int -> Tmatrix
makeTmatrix n = 
    List.foldl insert ADict.empty (pairs n)

etMul: Tmatrix -> Point -> Int -> Tmatrix
etMul m ptrow alpha = 
    List.foldl (\ptcol tm -> case ADict.get (ptrow, ptcol) tm of
                                Just el -> ADict.insert (ptrow, ptcol) 
                                            (modBy p (alpha * el)) tm
                                Nothing -> tm
                ) m (colIndices size)

etAdd: Tmatrix -> Point -> Point -> Int -> Tmatrix --row2+(row1*alpha)
etAdd m ptrow1 ptrow2 alpha = 
    List.foldl (\ptcol tm -> case ADict.get (ptrow1, ptcol) tm of
                                Just el -> 
                                    case ADict.get (ptrow2, ptcol) tm of
                                        Just el2 ->
                                            if (modBy p (el2 + (modBy p (alpha * el)))) /=0 then
                                                ADict.insert (ptrow2, ptcol) 
                                                    (modBy p (el2 + (modBy p (alpha * el)))) tm
                                            else
                                                ADict.remove (ptrow2, ptcol) tm
                                        Nothing ->
                                            if (modBy p (alpha * el)) /= 0 then
                                                ADict.insert (ptrow2, ptcol) 
                                                    (modBy p (alpha * el)) tm
                                            else
                                                ADict.remove (ptrow2, ptcol) tm
                                Nothing -> tm
                ) m (colIndices size)

etSwap: Tmatrix -> Point -> Point -> Tmatrix
etSwap m row1 row2 =
    if row1 == row2 then
        m
    else
        ADict.foldl (\(r,c) v dict ->
                         if r == row1 then
                             ADict.insert (row2,c) v dict
                         else if r == row2 then
                                  ADict.insert (row1, c) v dict
                              else
                                  ADict.insert (r,c) v dict
                    ) ADict.empty m
--    let 
--        m1 = etAdd m row1 row2 (-1) 
--        m2 = etAdd m1 row2 row1 1
--        m3 = etAdd m2 row1 row2 (-1)
--        m4 = etMul m3 row2 (-1)
--    in 
--        m4

nonzeroIndices: Point -> List Point -> Tmatrix -> List Point
nonzeroIndices col rows m = 
    List.foldl (\row indices -> case (ADict.get (row, col) m) of
                                    Nothing -> indices
                                    Just v -> indices ++ [row]
                )[] rows

pinverse: Int -> Int
pinverse x = 
    let
        list = List.filter (\y -> (modBy p (x * y + p)) == 1) (List.range 1 (p - 1))
    in
        Maybe.withDefault 0 <| List.head list

raiseNonzeroLowerEntry: Point -> List Point -> Tmatrix -> Tmatrix
raiseNonzeroLowerEntry col indices m =
    if (List.length indices) == 0 then
        m
    else
        let
            nonzeros = List.filter (\row -> case ADict.get (row,col) m of
                                                Nothing -> False
                                                Just el ->
                                                    if el /= 0 then
                                                        True
                                                    else
                                                        False
                                   )
                       indices
        in
        case (List.head nonzeros) of
            Nothing -> m
            Just top ->
                case (List.head indices) of
                    Nothing -> m
                    Just row ->
                        let
                            el = --Debug.log "pivot value" <|
                                 Maybe.withDefault 0 <| ADict.get (top, col) m
                        in
                            if top /= row then
                                etMul (etSwap m row top ) row (pinverse el)
                            else
                                etMul m row (pinverse el)
    
            
columnEliminate: Point -> List Point -> Tmatrix -> Tmatrix
columnEliminate col indices m = 
    if (List.length indices) == 0 then
        m
    else 
        let
            top = Maybe.withDefault {x = 0, y = 0} <| List.head indices
            pivot = Maybe.withDefault 0 <| ADict.get (top, col) m
        in
            if pivot == 0 then
                m
            else
                List.foldl (\row mat -> 
                                let
                                    el = Maybe.withDefault 0 <| ADict.get (row, col) mat
                                in
                                    etAdd mat top row (-el)
                           )m (List.filter (\row -> row /= top) (rowIndices size))

                            
gaussianEliminate: Tmatrix -> List Point -> (Tmatrix, List Point)
gaussianEliminate m indices =
    List.foldl (\col (mat,rIndices)  ->
                    (columnEliminate col rIndices <|  raiseNonzeroLowerEntry col rIndices mat, List.drop 1 rIndices)
               ) (m, List.reverse indices |> (List.drop 1) |> List.reverse)  indices
                
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Determine ->  (determine model, Cmd.none)
        Complete -> (complete model, Cmd.none)
        Reset -> --(allOffButOne ((size//4), (size//4), 1) size p, Cmd.none)
                 (allOffButOne (0, 0, 1) size p, Cmd.none)
        Solve ->
            let
                solCol = {x=size+1,y=size+1}
                one = {x = initLitX, y = initLitY}
                theMatrix = Dict.foldl (\(x,y,c) state dict ->
                                            if c==1 then
                                                ADict.insert ({x=x,y=y},solCol) state.brightness dict
                                            else
                                                dict
                                                )
                            (makeTmatrix model.size) model.determined
                --solutionMat = Debug.log "" <| gaussianEliminate (ADict.insert (one,solCol) 1 (makeTmatrix model.size) )
                                --(colIndices model.size)
                solutionMat = gaussianEliminate theMatrix (colIndices model.size)
                        
                newDetermined = ADict.foldl (\(row, col) numClick dict ->
                                                 if col == solCol then
                                                     let
                                                         state = Maybe.withDefault (State 0 Nothing)
                                                                 <| Dict.get (row.x, row.y, 1) dict
                                                     in
                                                         Dict.insert (row.x,row.y,1) {state|numClick = Just numClick} dict
                                                 else
                                                     dict
                                            ) model.determined (Tuple.first solutionMat)
                --dummy2 = Debug.log "" <| (colIndices model.size)
            in
                ({model | determined = newDetermined}, Cmd.none)
        RandGenerated rseq  ->
            (initialize (Debug.log "random" <| rseq) size p, Cmd.none)
        Clicked (x,y,c) -> 
            (rotation (x,y,c) model, Cmd.none)
            

view: Model -> Html Msg
view model =
    Html.div [Attr.align "center"]
        [Html.button
             [Html.Events.onClick Reset]
             [Html.text "reset"]
        ,Html.button
             [Html.Events.onClick Solve]
             [Html.text "solution"]             
        ,Html.br [][]
        ,svg [width "800"
             ,height "800"
             ,viewBox "-100 -200 700 600"
             ]
             [boardView model]
        ]

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none
