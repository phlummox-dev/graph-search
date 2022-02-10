{-# LANGUAGE TypeFamilies #-}


{- |


-}

module Algorithm.Traverse

  where

import           Data.Foldable
import           Data.Functor.Identity
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq(..), (<|), (|>) )

data Color = White | Grey | Black
  deriving (Eq)

-- | The @SearchContainer@ class abstracts the idea of a container to be used in
-- @generalizedSearch@
class SearchContainer container where
  type Elem container
  pop :: container -> Maybe (Elem container, container)
  push :: container -> Elem container -> container

instance SearchContainer (Seq.Seq a) where
  type Elem (Seq.Seq a) = a
  pop s =
    case Seq.viewl s of
      Seq.EmptyL -> Nothing
      (x Seq.:< xs) -> Just (x, xs)
  push s a = s Seq.|> a

instance SearchContainer [a] where
  type Elem [a] = a
  pop list =
    case list of
      [] -> Nothing
      (x : xs) -> Just (x, xs)
  push list a = a : list

-- | A @SearchState@ represents the state of a search at a given
-- point in an algorithm's execution. The advantage of this abstraction is that
-- it can be used for things like bidirectional searches, where you want to
-- stop and start a search part-way through.
data SearchState container stateKey state = SearchState {
    queue   :: container
  , visited :: M.Map stateKey Color
  , paths   :: M.Map stateKey (Seq state)
  }
--  deriving Show

generalizedTraverseM ::
  (SearchContainer container, Monad m, Foldable t,
   Ord state,
   Elem container ~ state) =>
  container
  -- ^ Empty queue/@SearchContainer@
  -> (state -> m (t state))
  -- ^ Function to generate "next" states given a current state
  -> state
  -- ^ Initial state
  -> m (Map state Color, Map state (Seq state))
generalizedTraverseM emptyQueue nextM initial = do
    -- for everything in the q, there should
    -- be an entry in paths explaining how we got to it
    let
        --initial_state :: SearchState (Seq state) state state
        initial_state =
          SearchState {
              queue   = push emptyQueue initial
            , visited = M.singleton initial Grey
            , paths   = M.singleton initial Seq.empty
            }
    res <- loop initial_state
    return (visited res, paths res)
  where
    loop ss =
      case pop (queue ss) of
        Nothing -> return ss
        Just (current, new_q) -> do
          next_states <- nextM current
          let
            new_visited = M.insert current Black (visited ss)
            -- steps taken to get to each el in next_states
            steps_so_far = (paths ss M.! current) |> current
            (new_visited', new_paths, new_q') =
                  foldl' (\(vis, ps, q) st ->
                              if st `M.member` vis
                              then (vis, ps, q)
                              else
                                  let vis' = M.insert st Grey vis
                                      ps'  = M.insert st steps_so_far ps
                                      q'   = push q st
                                  in  (vis', ps', q'))
                          (new_visited, paths ss, new_q)
                          next_states
            new_ss = SearchState {
                queue   = new_q'
              , visited = new_visited'
              , paths   = new_paths
              }
          loop new_ss

bfTraverseM ::
  (Monad m, Foldable t, Ord state) =>
    (state -> m (t state)) -> state -> m (Map state Color, Map state (Seq state))
bfTraverseM = generalizedTraverseM Seq.empty

dfTraverseM ::
  (Monad m, Foldable t, Ord state) =>
  (state -> m (t state))
  -> state -> m (Map state Color, Map state (Seq state))
dfTraverseM = generalizedTraverseM []

bfTraverse ::
  (Foldable f, Ord state, Show state) =>
      (state -> f state) -> state -> (Map state Color, Map state (Seq state))
bfTraverse next initial =
  runIdentity $ bfTraverseM (Identity . next) initial

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
