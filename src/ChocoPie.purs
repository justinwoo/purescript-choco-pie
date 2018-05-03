module ChocoPie where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Effect (Effect)
import FRP.Event (Event, create, subscribe)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record as Record
import Type.Equality (class TypeEquals, from, to)
import Type.Row (RLProxy(RLProxy), RProxy(RProxy))

runChocoPie :: forall bundleRow driverRow sinkRow sourceRow
   . ChocoPieRecord  sourceRow sinkRow driverRow bundleRow
  => (Record sourceRow -> Record sinkRow)
  -> Record driverRow
  -> Effect Unit
runChocoPie = chocoPieItUp

class ChocoPieRecord
  (sourceRow :: # Type)
  (sinkRow :: # Type)
  (driverRow :: # Type)
  (bundleRow :: # Type)
  | sourceRow -> sinkRow driverRow bundleRow
  where
  chocoPieItUp ::
       (Record sourceRow -> Record sinkRow)
    -> (Record driverRow)
    -> Effect Unit

instance chocoPieRecord ::
  ( RowToList sourceRow sourceList
  , RowToList sinkRow sinkList
  , RowToList driverRow driverList
  , RowToList bundleRow bundleList
  , ChocoPieRowList sourceList sinkList driverList bundleList
  , MakeSinkProxies sinkList sinkRow bundleList bundleRow
  , CallDrivers driverList driverRow bundleList bundleRow sourceList sourceRow
  , ReplicateMany sinkList sinkRow bundleList bundleRow
  ) => ChocoPieRecord sourceRow sinkRow driverRow bundleRow where
  chocoPieItUp main drivers = do
    sinkProxies <- makeSinkProxies sinkListP bundleListP sinkRowP
    sources <- callDrivers
      driverListP bundleListP sourceListP
      drivers sinkProxies
    let
      sinks = main sources
    _ <- replicateMany sinkListP bundleListP sinks sinkProxies
    pure unit
    where
      sinkListP = RLProxy :: RLProxy sinkList
      bundleListP = RLProxy :: RLProxy bundleList
      driverListP = RLProxy :: RLProxy driverList
      sourceListP = RLProxy :: RLProxy sourceList
      sinkRowP = RProxy :: RProxy sinkRow

class MakeSinkProxies
  (sinkList :: RowList) (sinks :: # Type)
  (bundleList :: RowList) (bundles :: # Type)
  | sinkList -> sinks
  , bundleList -> bundles where
  makeSinkProxies ::
       RLProxy sinkList
    -> RLProxy bundleList
    -> RProxy sinks
    -> Effect (Record bundles)

instance makeSinkProxiesCons ::
  ( IsSymbol name
  , MakeSinkProxies tail tailRow bundleList bundles'
  , Row.Lacks name bundles'
  , Row.Cons name { event :: Event a, push :: a -> Effect Unit } bundles' bundles
  ) => MakeSinkProxies (Cons name (Event a) tail) row bundleList bundles where
  makeSinkProxies _ _ _ = do
    bundle <- create
    rest <- makeSinkProxies
      (RLProxy :: RLProxy tail)
      (RLProxy :: RLProxy bundleList)
      (RProxy :: RProxy tailRow)
    pure $ Record.insert nameP bundle rest
    where
      nameP = SProxy :: SProxy name

instance makeSinkProxiesNil ::
  ( TypeEquals (Record bundle) {}
  ) => MakeSinkProxies Nil row bundleList bundle where
  makeSinkProxies _ _ _ = pure $ from {}

class CallDrivers
  (driverList :: RowList) (driver :: # Type)
  (bundleList :: RowList) (bundle :: # Type)
  (sourceList :: RowList) (source :: # Type)
  | driverList -> driver bundleList sourceList
  , bundleList -> bundle driverList sourceList
  , sourceList -> source driverList bundleList where
  callDrivers ::
       RLProxy driverList
    -> RLProxy bundleList
    -> RLProxy sourceList
    -> Record driver
    -> Record bundle
    -> Effect (Record source)

instance callDriversCons ::
  ( IsSymbol name
  , CallDrivers
      driverTail driverRow
      bundleTail bundleRow
      sourceTail sourceTailRow
  , TypeEquals bundleton { event :: Event a, push :: a -> Effect Unit }
  , TypeEquals driverton (Event a -> Effect b)
  , Row.Cons name driverton trash1 driverRow
  , Row.Cons name bundleton trash2 bundleRow
  , Row.Lacks name sourceTailRow
  , Row.Cons name b sourceTailRow sourceRow
  ) => CallDrivers
    (Cons name driverton driverTail) driverRow
    (Cons name bundleton bundleTail) bundleRow
    (Cons name b sourceTail) sourceRow where
  callDrivers _ _ _ drivers bundles = do
    rest <- callDrivers
      (RLProxy :: RLProxy driverTail)
      (RLProxy :: RLProxy bundleTail)
      (RLProxy :: RLProxy sourceTail)
      drivers
      bundles
    source <- getSource
    pure $ Record.insert nameP source rest :: Record sourceRow
    where
      nameP = SProxy :: SProxy name
      bundleton :: { event :: Event a, push :: a -> Effect Unit }
      bundleton = to $ Record.get nameP bundles
      event :: Event a
      event = bundleton.event
      driver :: (Event a -> Effect b)
      driver = to $ Record.get nameP drivers
      getSource :: Effect b
      getSource = to $ driver event

instance callDriversNil ::
  ( TypeEquals (Record source) {}
  ) => CallDrivers Nil driver Nil bundle Nil source where
  callDrivers _ _ _ _ _ = pure $ from {}

class ReplicateMany
  (sinkList :: RowList) (sinkRow :: # Type)
  (bundleList :: RowList) (bundleRow :: # Type)
  | sinkList -> sinkRow
  , bundleList -> bundleRow where
  replicateMany ::
       RLProxy sinkList
    -> RLProxy bundleList
    -> Record sinkRow
    -> Record bundleRow
    -> Effect Unit

instance replicateManyCons ::
  ( IsSymbol name
  , TypeEquals bundleton { event :: Event a, push :: a -> Effect Unit}
  , Row.Cons name (Event a) sinkTailRow sinkRow
  , Row.Cons name bundleton bundleTailRow bundleRow
  , ReplicateMany sinkTail sinkRow bundleTail bundleRow
  ) => ReplicateMany
    (Cons name (Event a) sinkTail) sinkRow
    (Cons name bundleton bundleTail) bundleRow where
  replicateMany _ _ sinks bundles = do
    _ <- subscribe sink bundle.push
    replicateMany sinkTailRowP bundleTailRowP sinks bundles
    where
      nameP = SProxy :: SProxy name
      sink = Record.get nameP sinks
      bundle :: { event :: Event a, push :: a -> Effect Unit}
      bundle = to $ Record.get nameP bundles
      sinkTailRowP = RLProxy :: RLProxy sinkTail
      bundleTailRowP = RLProxy :: RLProxy bundleTail

instance replicateManyNil :: ReplicateMany Nil sinkRow Nil bundleRow where
  replicateMany _ _ _ _ = pure unit

class ChocoPieRowList
  (sourceList :: RowList)
  (sinkList :: RowList)
  (driverList :: RowList)
  (bundleList :: RowList)
  | sourceList -> sinkList driverList bundleList
  , sinkList -> sourceList driverList bundleList
  , driverList -> sourceList sinkList bundleList
  , bundleList -> sourceList sinkList driverList

instance chocoPieRowListCons ::
  ( ChocoPieRowList sourceTail sinkTail driverTail bundleTail
  , TypeEquals driver ((Event a) -> Effect b)
  , TypeEquals c { event :: Event a, push :: a -> Effect Unit}
  ) => ChocoPieRowList
    (Cons k b sourceTail)
    (Cons k (Event a) sinkTail)
    (Cons k driver driverTail)
    (Cons k c bundleTail)

instance chocoPieRowListNil :: ChocoPieRowList Nil Nil Nil Nil
