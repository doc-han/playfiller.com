module Index exposing (..)
import Html exposing (Html, text, div, h3, ol, li, p)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Browser
import Random
import Task
import Set exposing (Set)

main =
  Browser.element {init=init, update=update, subscriptions=subscriptions, view=view}

-- MODEL
type Color = Black | Purple | Red | Green | Blue | Yellow
type CurrPlayer = Player1 String | Player2 String
type alias Model = {board: Array Color, currPlayer: CurrPlayer, moves: Array Color, score: (Int, Int)}
type Msg = MakeMove Color 
  | GenNewBoard (List Color)
  | AvailableMoves (List Color)
  | PaintMove Color
  | NoMessage
  | AIMove Color
  
allColors: List Color
allColors = [Black, Purple, Red, Green, Blue, Yellow]

-- INIT
againstAI = True -- set to True to play against Wicked AI
aiDepth = 8
gridSize = 40
rowGridSize = 8
colGridSize = 7
boardSize = rowGridSize * colGridSize
boardWidth = rowGridSize * gridSize
topRight = rowGridSize - 1
bottomLeft = rowGridSize * (colGridSize - 1)
player1 = Player1 (if againstAI then "You" else "Player 1")
player2 = Player2 (if againstAI then "Wicked AI" else "Player 2")

init: () -> (Model, Cmd Msg)
init _ = ({ board = Array.repeat boardSize Purple, currPlayer=player1, moves=Array.empty, score=(0, 0) }, Random.generate GenNewBoard genBoard)

intMin = -2147483648
intMax = 2147483647

-- UPDATE
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    MakeMove color ->
      case model.currPlayer of
        Player1 _ ->
          (model, Cmd.none)
        Player2 _ ->
          (model, Cmd.none)
    GenNewBoard newBoardList ->
      let 
        newBoard = Array.fromList (correctBoard newBoardList)
        player1score = List.length (getPlayerSquares bottomLeft (getCellColor bottomLeft newBoard) newBoard False)
        player2score = List.length ((getPlayerSquares topRight (getCellColor topRight newBoard) newBoard True))
      in ({model | board=newBoard, score= (player1score, player2score)}, Task.perform AvailableMoves (Task.succeed (getPlayerMoves newBoard)))
    AvailableMoves movesList ->
      ({model | moves=(Array.fromList movesList)}, Cmd.none)
    PaintMove color ->
      case model.currPlayer of
        Player1 _ ->
          let
            playerSquares = (getPlayerSquares bottomLeft (getCellColor bottomLeft model.board) model.board False)
            newBoard = (changePlayerColor playerSquares color model.board)
            score = List.length (getPlayerSquares bottomLeft (getCellColor bottomLeft newBoard) newBoard False)
            playerMoves = List.filter (\x -> x /= (getCellColor bottomLeft newBoard)) (getPlayerMoves newBoard)
          in
            ({model | board=newBoard, currPlayer = player2, score = (score, (Tuple.second model.score))}, if againstAI then Task.perform AIMove (Task.succeed Black) else Task.perform AvailableMoves (Task.succeed playerMoves))
        Player2 _ ->
          let 
            playerSquares = (getPlayerSquares topRight (getCellColor topRight model.board) model.board True)
            newBoard = (changePlayerColor playerSquares color model.board)
            score = List.length ((getPlayerSquares topRight (getCellColor topRight newBoard) newBoard True))
            playerMoves = List.filter (\x -> x /= (getCellColor topRight newBoard)) (getPlayerMoves newBoard)
          in
            ({model | board=newBoard, currPlayer = player1, score = ((Tuple.first model.score), score)}, Task.perform AvailableMoves (Task.succeed playerMoves))
    AIMove _ ->
      let
        moveColor = makeMove model.board True aiDepth
      in
        (model, Task.perform PaintMove (Task.succeed moveColor))
    NoMessage ->
      (model, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW
view: Model -> Html Msg
view model =
  div [] [
    div [style "marginTop" "130px", style "textAlign" "center", style "fontSize" "30px", style "marginBottom" "16px"] [
      text (String.fromInt (Tuple.first model.score)),
      text " : ",
      text (String.fromInt (Tuple.second model.score))
    ],
    div [style "display" "flex", style "justifyContent" "center"] [
      div [style "display" "flex", style "justifyContent" "flex-end", style "alignItems" "end", style "flexDirection" "column", style "minWidth" (getPx (6 * gridSize))][
        (if model.currPlayer == player1 then
          div [style "display" "flex", style "marginRight" "44px"] (Array.toList (Array.map (\x -> (renderSquare x True)) model.moves))
        else div [] []),
        div [style "height" (getPx gridSize), style "display" "flex", style "alignItems" "center", style "marginRight" "16px", style "fontSize" "20px"] [
          text ((getPlayerName player1) ++ " →")
        ]
      ],
      div [style "minWidth" (getPx boardWidth), style "maxWidth" (getPx boardWidth), style "display" "flex", style "flexWrap" "wrap"] 
      (List.map (\x -> (renderSquare x False)) (Array.toList model.board)),
      div [style "display" "flex", style "alignItems" "start", style "flexDirection" "column", style "minWidth" (getPx (6 * gridSize))][
        div [style "height" (getPx gridSize), style "display" "flex", style "alignItems" "center", style "marginLeft" "16px", style "fontSize" "20px"] [
          text ("← " ++ (getPlayerName player2))
        ],
        (if model.currPlayer == player2 then
          div [style "display" "flex", style "marginLeft" "44px"] (Array.toList (Array.map (\x -> (renderSquare x True)) model.moves))
        else div [] [])
      ]
    ],
    showRules
  ]

showRules: Html Msg
showRules =
  div [style "width" "50%", style "margin" "auto", style "marginTop" "100px", style "border" "1px solid #333", style "padding" "16px"] [
    h3 [] [text "Rules:"],
    ol [] [
      li [] [text "Each player is assigned a corner tile at the start of the game."],
      li [] [text "Players take turns filling their tiles with one of 6 colors in an attempt to capture adjacent tiles of the same color."],
      li [] [text "You are not allowed to change the color of your tiles into the color of your opponents tiles."],
      li [] [text "The game ends when there are no more tiles to occupy."]
    ],
    h3 [] [text "Goal:"],
    p [] [text "Player who managed to capture most tiles wins."]
  ]

renderSquare: Color -> Bool -> Html Msg
renderSquare color clickable =
  div [onClick (if clickable then (PaintMove color) else NoMessage), style "width" (getPx gridSize), style "height" (getPx gridSize), style "boxShadow" "inset 0 0 1px #000", style "backgroundColor" (getColorStr color)] []
  
  
-- HELPERS
getPx px = (String.fromInt px) ++ "px"

getColorStr: Color -> String
getColorStr color =
  case color of
    Black -> "#333"
    Purple -> "#ff67ff"
    Red -> "#ff3838"
    Green -> "#47f847"
    Blue -> "#5454ff"
    Yellow -> "#ffff26"
    
getRandomColors : Random.Generator Color
getRandomColors =
  Random.uniform Black [ Purple, Red, Green, Blue, Yellow ]
  
genBoard: Random.Generator (List Color)
genBoard =
    Random.list boardSize getRandomColors

correctBoard: List Color -> List Color
correctBoard arr =
  let
    isValid i = i >= 0 && i < boardSize
    isInNeighbor clx nlist = List.member clx nlist 
    getNeighbors index marr = 
      (if (isValid (index-rowGridSize)) then [getCellColor (index-rowGridSize) marr] else []) ++
      (if (isValid (index+rowGridSize)) then [getCellColor (index+rowGridSize) marr] else []) ++
      (if (isValid (index-1)) && (index-1 /= (rowGridSize-1)) then [getCellColor (index-1) marr] else []) ++
      (if (isValid (index+1)) && (index+1 /= 0) then [getCellColor (index+1) marr] else [])
    arrWIndex = List.indexedMap Tuple.pair arr
    mapFunc (xi, xc) acc =
      let
        nbs = getNeighbors xi (Array.fromList acc)
      in
        if isInNeighbor xc nbs then
          let
            hmm = nbs ++ [xc]
            rm = List.filter (\x-> not <| List.member x hmm) allColors
            nIdx = modBy (List.length rm) xi
            one = getCellColor nIdx (Array.fromList rm)
            b = Array.set xi one (Array.fromList acc)
          in
            Array.toList b
        else
          acc
    newBoard = List.foldl mapFunc arr arrWIndex
  in
    newBoard

getPlayerMoves: Array Color -> List Color
getPlayerMoves arr = 
  let
    trColor = getCellColor topRight arr
    blColor = getCellColor bottomLeft arr
    colorsOnBoard = Array.toList arr |> removeDuplicates 
    onBoardMoves = colorsOnBoard |> List.filter (\x -> (if (x == blColor) || (x == trColor) then False else True)) 
    otherMoves = allColors |> List.filter (\x -> (if (x == blColor) || (x == trColor) then False else True)) 
    len = List.length onBoardMoves
  in
    if len > 0 then onBoardMoves
    else otherMoves
    

getPlayerSquares : Int -> Color -> Array Color -> Bool -> List Int
getPlayerSquares index playerColor arr isInv =
    let
        isValidIndex i = i >= 0 && i < boardSize
        getNeighbors i = 
            [i - rowGridSize, i + rowGridSize ] ++ (if modBy rowGridSize (i+1) == 0 then [] else [i+1]) ++ (if modBy rowGridSize (i-1) == (rowGridSize-1) then [] else [i-1])
            |> List.filter (\neighborIndex -> isValidIndex neighborIndex && getCellColor neighborIndex arr == playerColor)

        initialQueue = [index]
        visited = Set.singleton index
        isPlayerColor i = getCellColor i arr == playerColor

        bfs: List Int -> Set Int -> List Int
        bfs queue hset =
            case queue of
                [] -> []
                current :: rest ->
                    let
                        neighbors = getNeighbors current
                        newNeighbors = List.filter (\neighbor -> not (Set.member neighbor hset) && isPlayerColor neighbor) neighbors
                        newQueue = rest ++ newNeighbors
                        newVisited = List.foldl Set.insert hset newNeighbors
                    in
                    current :: bfs (newQueue) newVisited

    in
    bfs initialQueue visited |> removeDuplicates

changePlayerColor: List Int -> Color -> Array Color -> Array Color
changePlayerColor list newColor arr =
  List.foldl (\index accArray -> Array.set index newColor accArray) arr list

getCellColor: Int -> Array Color -> Color
getCellColor index arr =
  let cellColor = Array.get index arr
  in
    case cellColor of
      Just color -> color
      Nothing -> Black

getPlayerName: CurrPlayer -> String
getPlayerName currPlayer =
  case currPlayer of
    Player1 name -> name
    Player2 name -> name
  
removeDuplicates : List a -> List a
removeDuplicates list =
  List.foldl (\item acc ->
    if List.member item acc then
      acc
    else
      item :: acc
  ) [] list
  
folderMax: (Int, Int) -> (Int, Int) -> (Int, Int)
folderMax a b =
  let 
    asec = Tuple.second a
    bsec = Tuple.second b
  in
    if asec > bsec then a
    else b
    
folderMin: (Int, Int) -> (Int, Int) -> (Int, Int)
folderMin a b =
  let 
    asec = Tuple.second a
    bsec = Tuple.second b
  in
    if asec < bsec then a
    else b
  
-- MIN-MAX
makeMove: Array Color -> Bool -> Int -> Color
makeMove arr isMe depth =
  let
    moves = getPlayerMoves arr
    myColor = getCellColor topRight arr
    weights = List.map (\xolor -> (evalMove (makeDummyMove topRight myColor xolor arr isMe) isMe depth intMin intMax)) moves
    indexedWeights = List.indexedMap Tuple.pair weights
    val = List.foldl folderMax (0, -boardSize) indexedWeights
    index = Tuple.first val
    color = Array.get index (Array.fromList moves)
    v = Debug.log "weights" weights
  in
    case color of
      Just c -> c
      Nothing -> Black -- making the compiler happy!
      
type alias Acc = {xalpha: Int, xbeta: Int, xweight: Int}
  
evalMove: Array Color -> Bool -> Int -> Int -> Int -> Int
evalMove arr is2 depth alpha beta =
  if depth <= 0 then evalBoard arr
  else
    let
      index = if is2 then topRight else bottomLeft
      color = getCellColor index arr
      
      evalFolder: Color -> Acc -> Acc
      evalFolder xolor acc =
        if acc.xalpha >= acc.xbeta then acc
        else
          let
            weight = evalMove (makeDummyMove index color xolor arr is2) (not is2) (depth-1) acc.xalpha acc.xbeta
            newAlpha = if is2 then max weight acc.xalpha else acc.xalpha
            newBeta = if is2 then acc.xbeta else min weight acc.xbeta
            newWeight = if is2 then max weight acc.xweight else min weight acc.xweight
          in
            {xalpha=newAlpha, xbeta=newBeta, xweight= newWeight}
    
      initialWeight = if is2 then intMin else intMax
      finalAcc = getPlayerMoves arr |> List.foldl evalFolder (Acc alpha beta initialWeight)
    in
      finalAcc.xweight

evalBoard: Array Color -> Int
evalBoard arr =
  let
    player1Color = getCellColor bottomLeft arr
    player2Color = getCellColor topRight arr
    player1score = getPlayerSquares bottomLeft player1Color arr False |> List.length
    player2score = getPlayerSquares topRight player2Color arr True |> List.length
  in
    player2score - player1score
    
makeDummyMove: Int -> Color -> Color -> Array Color -> Bool -> Array Color
makeDummyMove index playerColor newColor arr isInv =
  let
    playerSquaresList = getPlayerSquares index playerColor arr isInv
  in
    changePlayerColor playerSquaresList newColor arr