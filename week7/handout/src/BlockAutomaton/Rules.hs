module BlockAutomaton.Rules
  ( Rules (..),
    MargolusPos (..),
    margolusInitial,
    margolusShift,
  )
where

data MargolusPos = UL | UR | LL | LR
  deriving (Eq, Show)

margolusInitial :: (Int, Int) -> MargolusPos
margolusInitial (i, j) =
  case (i `mod` 2, j `mod` 2) of
    (0, 0) -> UL
    (0, 1) -> UR
    (1, 0) -> LL
    (_, _) -> LR

margolusShift :: MargolusPos -> MargolusPos
margolusShift UL = LR
margolusShift LR = UL
margolusShift UR = LL
margolusShift LL = UR

data Rules state obs = Rules
  { rulesInitial :: (Int, Int) -> state,
    rulesInteract :: (state, state) -> (state, state),
    rulesObserve :: state -> obs
  }

smoothen :: Rules Double Int
smoothen =
  Rules
    { rulesInitial = \(i, j) -> fromIntegral (i + j),
      rulesInteract = \(s1, s2) -> let s = s1 + s2 / 2 in (s, s),
      rulesObserve = round
    }