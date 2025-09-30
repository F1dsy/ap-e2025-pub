{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module BlockAutomaton.Simulation (Grid, gridIndex, gridUpdate, positions, initialGrid, observeGrid, margolusInteract, stepOne, stepGrid) where

import BlockAutomaton.Rules

type Grid a = [[a]]

gridIndex :: (Int, Int) -> Grid a -> a
gridIndex (i, j) g =
  let row = g !! (i `mod` length g)
   in row !! (j `mod` length row)

gridUpdate :: (Int, Int) -> a -> Grid a -> Grid a
gridUpdate (i, j) v g =
  case splitAt (i `mod` length g) g of
    (be_rows, row : af_rows) ->
      case splitAt (j `mod` length g) row of
        (before, _ : after) -> be_rows ++ [before ++ v : after] ++ af_rows
        _ -> error "Out of Bounds"
    _ -> error "Out of Bounds"

positions :: Int -> Int -> [[(Int, Int)]]
positions h w = map (\i -> map (\j -> (i, j)) [0 .. w - 1]) [0 .. h - 1]

initialGrid :: Rules state obs -> Int -> Int -> Grid (MargolusPos, state)
initialGrid rules w h =
   map (map (\pos -> (margolusInitial pos, rulesInitial rules pos))) $ positions w h

observeGrid :: Rules state obs -> Grid (MargolusPos, state) -> Grid obs
observeGrid rules = map (map ( rulesObserve rules . snd))

margolusInteract ::
  Rules state obs ->
  (Int, Int) ->
  Grid (MargolusPos, state) ->
  Grid (MargolusPos, state)
margolusInteract rules (i,j) g =
  let ul = gridIndex (i,j) g
      ur = gridIndex (i,j+1) g
      ll = gridIndex (i+1,j) g
      lr = gridIndex (i+1,j+1) g in
  let ul_ur = rulesInteract rules (snd ul, snd ur)
      ll_lr = rulesInteract rules (snd ll, snd lr)
      ul_ll = rulesInteract rules (fst ul_ur, fst ll_lr)
      ur_lr = rulesInteract rules (snd ul_ur, snd ll_lr) in
  gridUpdate (i,j) (UL, fst ul_ll) $
    gridUpdate (i,j) (UR, fst ur_lr) $
      gridUpdate (i,j) (LL, snd ul_ll) $
        gridUpdate (i,j) (LR, snd ur_lr) g

stepOne ::
  Rules state obs ->
  (Int, Int) ->
  Grid (MargolusPos, state) ->
  Grid (MargolusPos, state)
stepOne rules (i,j) g =
  case gridIndex (i,j) g of
    (UL, _) -> margolusInteract rules (i,j) g
    (_, _) -> g

stepGrid ::
  Rules state obs ->
  Grid (MargolusPos, state) ->
  Grid (MargolusPos, state)
stepGrid rules g =
  map (map shiftCell) $ foldr (stepOne rules) g (concat $ positions h w)
  where
    shiftCell (pos, s) = (margolusShift pos, s)
    h = length g
    w = length $ head g