{-***********************************************
Assignment3.hs
Implementation of a 8-Off Solitaire card game.
AI added
by Jakub Wielgorski
***********************************************-}
module Assignment2 where
    import System.Random
    import MergeSort
    import Data.List
    import Data.Maybe
    
    data Suit = Hearts | Clubs | Diamonds | Spades --Datatype for Suit of card.
                deriving (Eq, Ord, Enum, Show)
    
    --Datatype for Pip of the card.
    data Pip = Ace | Two | Three | Four | Five | Six | Seven 
                | Eight | Nine | Ten | Jack | Queen | King 
                deriving (Eq, Ord, Enum, Show)
                
    type Card = (Pip, Suit) --Datatype of card in the game.
    type Deck = [Card] --Datatype of the whole deck of cards.
    type Foundations = [Card] --Datatype created for foundations cells in the game.
    type Reserve = [Card] --Datatype created for reserve cells in the game.
    type Columns = [[Card]] --Datatype of collumns in the game.
    type EOBoard = (Foundations, Reserve, Columns) --Datatype of the game board. 
    
    pack :: Deck --List of all cards.
    pack = [(pip, suit) |  pip <- [Ace ..], suit <- [Hearts ..]]    
    
    sCard :: Card -> Card --Function that takes a card and returns its successor.
    sCard card = (succ(fst card),  (snd card)) 
    
    pCard :: Card -> Card --Function that takes a card and returns its predesessor.
    pCard card = (pred(fst card),  (snd card))
    
    isAce ::  Card -> Bool --Function that checks if a given card is an Ace.
    isAce card = (fst card) == Ace
    
    isKing :: Card -> Bool --Function that checks if a given card is a King.
    isKing card = (fst card) == King
    
    shuffle :: Int -> [Card]

    shuffle seed =  
     let
       gen= mkStdGen seed
       weights = take 52 (randoms gen :: [Int])
       dset = (map fst (sortBy  
                  (\ (_,w1)(_,w2)  -> (compare w1 w2)) 
                  (zip pack weights)))
     in
      dset  
    
    eODeal :: Int-> EOBoard     
    eODeal seed = 
     let
      spack = shuffle seed
      cols  = [(take 6 spack),          (take 6 (drop 6 spack)),  (take 6 (drop 12 spack)), (take 6 (drop 18 spack)), 
              (take 6 (drop 24 spack)), (take 6 (drop 30 spack)), (take 6 (drop 36 spack)), (take 6 (drop 42 spack))]
      fnds  = []
      res   = [spack!!48,spack!!49,spack!!50,spack!!51]
     in
      (fnds,res,cols) 
                   
    --Auxiliary function for eODeal that splits a Deck and returns Columns
    eOColumns :: Deck -> Columns
    eOColumns [] = []
    
    --The list of Cards is split to 8 Columns.
    eOColumns list = h : eOColumns t 
        where (h,t) = splitAt 8 list
    
    --Function which takes an EOBoard and returns the EOBoard obtained by making all the possible moves to the Foundations.
    toFoundations :: EOBoard ->  EOBoard
    toFoundations (foundations, reserve, columns) = 
        let
            --Filters only nonEpmty lists
            noEmptyColumns = filter (not . null) columns
            
            --Gets first element from each column
            firstElements = map head noEmptyColumns 
            
            --List of Aces taken from the list of first elements of each column
            acesFromColumns = filter (\n -> isAce n) firstElements
            
            --Delete Aces from columns.
            colNoAce = filter (\n -> not (isAce n)) firstElements
            
            --List of Aces from reserve.
            acesFromReserve =  filter (\n -> isAce n) reserve
            
            --Delete Aces from reserve.
            resNoAce = filter (\n -> not (isAce n)) reserve
            
            --Aces are passed to the foundations
            foundationsWithAces = foundations ++ acesFromColumns ++ acesFromReserve            
             
            --Mapps each element except Kings from foundations with its successor.
            successorsNeeded = map (\n -> if( isKing n) then n else sCard n) foundationsWithAces
            
            --Creates list of successors in reserve by taking common elements from reserve and successorsNeeded.
            successorsInReserve = intersection successorsNeeded resNoAce
            
            --Deletes old elements from foundations and replaces them with their successors form reserve.
            foundationsWithReserve = filter (`notElem` (map pCard successorsInReserve)) foundationsWithAces ++ successorsInReserve 
            
            --Deletes elements that were passed from reserve to foundations.
            newReserve = filter (`notElem` (successorsInReserve ++ acesFromReserve)) resNoAce --reserve      
            
            --Mapps each element except Kings from foundations with its successor.
            successorsNeeded2 = map (\n -> if( isKing n) then n else sCard n) foundationsWithReserve
            
            --Creates list of successors in columns by taking common elements from the list of first elements taken from each column and successorsNeeded2.
            successorsInColumns = intersection successorsNeeded2 colNoAce  
            
            --Deletes old elements from foundations and replaces them with their successors form columns.
            newFoundations = filter (`notElem` (map pCard successorsInColumns)) foundationsWithReserve ++ successorsInColumns -- final foundations   
            
            --Deletes elements that were passed from columns to foundations.
            newC = map (\x -> if (elem (head x) (successorsInColumns ++ acesFromColumns)) then tail x else x) noEmptyColumns --columns
            newColumns =  map (\x -> if ( isAce(head x)) then tail x else x) (filter (not.null )newC)
            
            check --Recursive call for toFoundations if the conditions are true. 
                |(((length successorsInReserve) /= 0) || ((length successorsInColumns) /= 0) || ((length acesFromColumns) /= 0) || ((length acesFromColumns) /= 0)) = --Checks if any element was passed to the foundations.
                    toFoundations (newFoundations, newReserve, newColumns) --If so, calls toFoundations again
                |otherwise = (newFoundations, newReserve, newColumns) --Otherwise returns a new EOBoard.            
        in check
    
    --Auxiliary function for toFoundations that takes two lists and returns their intersection.
    intersection :: Eq a => [a]->[a] -> [a]
    intersection [] _ = [] 
    intersection _ [] = []     
    intersection (h1:t1) lis2
        | elem h1 lis2 = (h1:intersection t1 lis2) --if head of the first list exists in the second list then this element is saved.
        | otherwise = intersection t1 lis2 --otherwise element is not passed to the intersection list.
        
    findMoves :: EOBoard -> [EOBoard]
    findMoves board = 
        let 
            -- Apply toFundation to the Board to avoid using pCard on Ace.
            (f,r,c) = toFoundations board
            
            --Filters only non epmty lists
            noEmptyColumns = filter (not . null) c
            
            --Gets first element from each column
            firstElements = filter(\x->not (isAce x)) (map head noEmptyColumns) 
            
            --Takes predessesor of the heads of the columns.
            predFirstElem = map pCard firstElements
            
            --List of cards that can be moved in columns.
            cardToMoveInColumns = intersection firstElements predFirstElem
            --List of cards that can be moved from reserve to columns.
            cardToMoveInReserve = intersection r predFirstElem 
            
            --All possibe moves only in columns.
            allCardMovesInColumns = movesInColumns (f,r,c) cardToMoveInColumns
            
            --All possible moves from reserve to columns.
            allMovesFromReverse = movesFromReserve (f,r,c) cardToMoveInReserve
            
            --All possibel moves from columns to reserve. Cant move king that is the only card in the column.  
            allMovesToReverse = filter (/= (f,r,c)) (map (moveCardReserves (f,r,c)) [head column| column <- c, (not . null) column, not (isKing (head column) && (length column == 1))])
            
            --All moves of kings from reserve to empty columns.
            moveKingsFromReserve = filter (/= (f,r,c))(map (moveKingFromReserve (f,r,c)) (getKings r))
            
            --All moves of King from columns to reserve if King isnt the only card in a column
            moveTopKings = filter (/= (f,r,c))(map (moveKingFromCollumn (f,r,c)) (getKings [head column| column <- c, (not . null) column, not (isKing (head column) && (length column == 1))]))
            --List of all possible moves.
        in  allCardMovesInColumns ++ allMovesFromReverse ++ allMovesToReverse ++ moveKingsFromReserve ++ moveTopKings
        
    --The function takes EOBoard and a list of cards as arguments and returns the list of all possible boards created by moves only in columns.
    movesInColumns :: EOBoard ->[Card]->[EOBoard]
    movesInColumns board [] = []
    movesInColumns (foundations, reserve, columns) (h:t) = 
        let
            -- no Empty columns
            nc = filter (not . null) columns
            
            --Delete card that is moved from one column.
            deleteColumns = map (\x -> if ((head x) == h) then tail x else x) nc 
            
            --no empty column after deleteColumns
            noEmptyColumns = filter (not . null) deleteColumns
            
            --Gets rid of top aces in columns to avoid using pCard on them.
            noAce = filter(\x -> not (isAce (head x))) noEmptyColumns
            
            --Add card that is moved to another column.
            addColumns = map (\x -> if ((pCard (head x)) == h) then h:x else x) noAce
            
            --Board after move.
            board = (foundations, reserve, addColumns)
            
            --Add board to the list of boards.
        in   [board] ++ (movesInColumns (foundations, reserve, columns) t)
    
    --The function takes EOBoard and a list of cards as arguments and returns the list of all possible boards created by moves from reserve to columns.    
    movesFromReserve :: EOBoard ->[Card]->[EOBoard] 
    movesFromReserve  board [] = []
    movesFromReserve (foundations, reserve, columns) (h:t) = 
        let 
            --no empty columns.
            noEmptyColumns = filter (not . null) columns
            
            --Delete card that is moved from reserve
            deleteFromReserve = delete h reserve          
            
            --Add this card to column
            addToColumns = map (\x -> if ((pCard (head x)) == h) then h:x else x) noEmptyColumns
            
            --Board after move
            board = (foundations, deleteFromReserve, addToColumns)
            --Add board to the list of boards.
        in [board] ++ (movesFromReserve (foundations, reserve, columns) t)
   
    --The function takes EOBoard and a Card as arguments and returns the board created by moves a card from columns to reserve. 
    moveCardReserves :: EOBoard -> Card -> EOBoard
    
    --If reserve is empty move card from column to reserve
    moveCardReserves (foundations,[],columns) card  = (foundations,[card], (map (\x -> if ((head x) == card) then tail x else x) (filter (not . null) columns)))     
    moveCardReserves board@(foundations,reserve,columns) card
        --if there is a place in reserve move card from column.
        | (length reserve) < 8 = (updatedFoundations,firstCard:updatedReserve,updatedColumns)
        --if there in not enought place return the same board
        | otherwise = board     
        where (firstCard:restCards) = reserve
              (updatedFoundations, updatedReserve, updatedColumns) = moveCardReserves (foundations,restCards,columns) card 
    
     --The function takes EOBoard and a Card as arguments and returns the board created by moves a king from reserve to column.  
    moveKingFromReserve :: EOBoard -> Card ->EOBoard  
    moveKingFromReserve (foundations, reserve, columns) card
        -- king can be moved if there is less than 8 columns. The column is created and the King is moved there.
        |length columns < 8 = (foundations, updatedReserve, columns ++ [[card]])
        |otherwise = (foundations, reserve, columns)
        where updatedReserve = delete card reserve
    
    --The function takes EOBoard and a Card as arguments and returns the board created by moves a king from column to reserve.
    moveKingFromCollumn :: EOBoard -> Card ->EOBoard
    moveKingFromCollumn (foundations, reserve, columns) card
        --If there is enougth space in reserve king is moved from column.
        |length reserve < 8 = (foundations, reserve ++ [card], updatedColumns)
        |otherwise = (foundations, reserve, columns)
        where updatedColumns = map (\x -> if ((head x) == card) then tail x else x) noEmptyColumns
              noEmptyColumns = filter (not . null) columns
    
    --the function that gets Kings from the list.
    getKings :: [Card] -> [Card]    
    getKings [] = []        
    getKings (h:t) 
        |isKing h = [h] ++ (getKings t)
        |otherwise = getKings t
    
    --Very elegant function that takes move form chooseMoveA and gets rid of infinite loops like moving one card from column to reserve and back.
    chooseMove :: EOBoard -> Maybe EOBoard
    chooseMove board
        |board == (chooseMoveA (chooseMoveA board)) = Nothing
        |board == chooseMoveA (chooseMoveA (chooseMoveA board)) = Nothing
        |board == chooseMoveA(chooseMoveA(chooseMoveA(chooseMoveA board))) = Nothing
        |otherwise = Just (chooseMoveA board)
    
    --Takes EOBoard and returns Just EOBoard, but returns Nothing if there is no legal move from the given position
    chooseMoveA :: EOBoard -> EOBoard
    chooseMoveA (foundations, reserve, columns)
        --return board of the move.
        |(not.null) afterToFundationBoards = move      
        |otherwise = (foundations, reserve, columns)
            --maps toFundation to every possible move.
        where afterToFundationBoards = map toFoundations (findMoves (foundations, reserve, columns)) 
        
              --adds value to every possible move.
              boardsWithValue = map countValue afterToFundationBoards 
              
              --connect value to every board.
              zlist = zip afterToFundationBoards boardsWithValue 
              
              --Sort boards.
              slist =  sortBy (\(_,n1) (_,n2) -> compare n1 n2) zlist 
              
              --Returns the first board from the list.
              (h:t) = map fst slist 
              move = h               
    --The function that takes EOBoard and return int which is its value.          
    countValue :: EOBoard -> Int
    --sums value of the cards in fundations and counts kings as heads of sorted columns and subtracts the lenght of reverse.
    countValue (foundations, reserve, columns) = length (getKings (map head  (filter (not.null) columns))) + 10*cardValues - 100*length reserve
        --change cards in fundation to ints.
        where cardValues = sum (map fromEnum (map getPip (filter (not.null) foundations))) 
    
    --Function that gets pip from card. 
    getPip :: Card -> Pip
    getPip (pip,_) = pip
    
    --Plays the game until the end of the moves or win and returns the score which is how many cards were moved to fundations.
    eOGame :: EOBoard -> Int
    eOGame board
        | isNothing nextBoard = value board
        | otherwise = eOGame (resMaybe nextBoard)
        where nextBoard = chooseMove board
              --counts value of the board.
              value (foundations,reserve,columns) = sum ( map length foundations)
    
    --Takes seed and number of games that will be played as an argument. Returns number of wins and average score.    
    eOExpt :: Int -> Int -> (Int,Float)
    eOExpt seed nGames = (0,avarageScore)
        where boards = map eODeal (take 100 (randoms (mkStdGen seed) :: [Int]))
              scores = map eOGame boards
              avarageScore = fromIntegral (sum scores)/ fromIntegral nGames  
     ----------------------------------------------------------
     -- display an EOBoard
    displayEOB :: EOBoard -> IO String
     
    displayEOB (fnds,res,cols) = do
      let colStr = colsToString cols
      putStr "EOBoard\nFoundations  "
      putStrLn (show fnds)
      putStr  "Columns"
      putStr colStr
      putStr "\n\nReserve     "
      putStrLn (show (((sum (map (fromIntegral.length) cols))+(sum (map (fromIntegral.length) fnds))+(fromIntegral (length res)))))
      putStrLn (show res)
      putStr "\n---------------------------------------------\n"
      return ""

    colsToString :: Columns->String -- prepare String to print columns on separate lines
     
    colsToString cols =
      foldr (++) "" ["\n             "++(show col) |col<-cols]
      
    -----------------------------------------------------------------------

    -- display a list of EOBoards  

    displayEOBList :: [EOBoard]-> IO String
     
    displayEOBList eobl =  -- @ notation doesn't seem to work correctly
      do
       if (null eobl) then do (return "")
                      else do
                            displayEOB (head eobl)
                            displayEOBList (tail eobl)
     
       
    -----------------------------------------------------------------

     --scoreBoard
     -- score is number of cards on foundations
     -- return a String for display
     
    scoreBoard :: EOBoard-> String 
    scoreBoard (fnds, res, cols) = "A LOSS: SCORE  " ++ (show (52- (length res) - (foldr (+) 0 (map length cols))))      

     -----------------------------------------------------------------------------
     -- play a game given initial board
     -- assuming a fn chooseMove :: EOBoard ->Maybe EOBoard
     -- & that toFoundations is handled outside
     
    displayEOGame :: EOBoard ->IO String
     
    displayEOGame b = do
      let (fnds,res,cols) = b -- apparently can't do this with @
      if ((null cols)&&(null res)) -- if cols & reserve empty its a win
         then return "A WIN"
         else 
          do
           displayEOB b -- display given board
           let res = chooseMove b
           if (isJust res) then
                   do
                    let nb = resMaybe res
                    displayEOGame nb
                  else
                   do
                     let score = scoreBoard b
                     return score
     
    ---- ------------------------------------------------  
    --Maybe helper                
    resMaybe :: (Maybe a) -> a
    resMaybe (Just x) = x       
        