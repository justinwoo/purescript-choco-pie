module ChocoPie where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Record (delete, get, insert)
import Data.Symbol (class IsSymbol, SProxy(..))
import FRP (FRP)
import FRP.Event (Event, create, subscribe)
import Type.Equality (class TypeEquals, from, to)
import Type.Row (class ListToRow, class RowLacks, class RowToList, Cons, Nil, RLProxy(..), RProxy(..), kind RowList)

runChocoPie :: forall bundleRow driverRow sinkRow sourceRow e m
   . ChocoPieRecord m e sourceRow sinkRow driverRow bundleRow
  => MonadEff (frp :: FRP | e) m
  => (Record sourceRow -> Record sinkRow)
  -> Record driverRow
  -> m Unit
runChocoPie = chocoPieItUp

class (MonadEff (frp :: FRP | e) m) <= ChocoPieRecord
  m (e :: # Effect)
  (sourceRow :: # Type)
  (sinkRow :: # Type)
  (driverRow :: # Type)
  (bundleRow :: # Type)
  | sourceRow -> sinkRow driverRow bundleRow
  , sinkRow -> sourceRow driverRow bundleRow
  , driverRow -> sourceRow sinkRow bundleRow
  , bundleRow -> sourceRow sinkRow driverRow
  , m -> e where
  chocoPieItUp ::
       (Record sourceRow -> Record sinkRow)
    -> (Record driverRow)
    -> m Unit

instance chocoPieRecord ::
  ( RowToList sourceRow sourceList
  , RowToList sinkRow sinkList
  , RowToList driverRow driverList
  , RowToList bundleRow bundleList
  , ChocoPieRowList m e sourceList sinkList driverList bundleList
  , MakeSinkProxies m e sinkList sinkRow bundleList bundleRow
  , CallDrivers m e driverList driverRow bundleList bundleRow sourceList sourceRow
  , ReplicateMany m e sinkList sinkRow bundleList bundleRow
  , ListToRow sourceList sourceRow
  , ListToRow sinkList sinkRow
  , ListToRow driverList driverRow
  , ListToRow bundleList bundleRow
  , MonadEff (frp :: FRP | e) m
  ) => ChocoPieRecord m e sourceRow sinkRow driverRow bundleRow where
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

class (MonadEff (frp :: FRP | e) m) <= MakeSinkProxies
  m (e :: # Effect)
  (xs :: RowList) (row :: # Type)
  (bundleList :: RowList) (bundles :: # Type)
  | xs -> row
  , bundleList -> bundles
  , m -> e where
  makeSinkProxies ::
       RLProxy xs
    -> RLProxy bundleList
    -> RProxy row
    -> m (Record bundles)

instance makeSinkProxiesCons ::
  ( IsSymbol name
  , MakeSinkProxies m e tail tailRow bundleList bundles'
  , RowLacks name bundles'
  , RowCons name { event :: Event a, push :: a -> Eff (frp :: FRP | e) Unit } bundles' bundles
  , MonadEff (frp :: FRP | e) m
  ) => MakeSinkProxies m e (Cons name (Event a) tail) row bundleList bundles where
  makeSinkProxies _ _ _ = do
    bundle <- liftEff create
    rest <- makeSinkProxies
      (RLProxy :: RLProxy tail)
      (RLProxy :: RLProxy bundleList)
      (RProxy :: RProxy tailRow)
    pure $ insert nameP bundle rest
    where
      nameP = SProxy :: SProxy name

instance makeSinkProxiesNil ::
  ( TypeEquals (Record bundle) {}
  , MonadEff (frp :: FRP | e) m
  ) => MakeSinkProxies m e Nil row bundleList bundle where
  makeSinkProxies _ _ _ = pure $ from {}

class (MonadEff (frp :: FRP | e) m) <= CallDrivers
  m (e :: # Effect)
  (driverList :: RowList) (driver :: # Type)
  (bundleList :: RowList) (bundle :: # Type)
  (sourceList :: RowList) (source :: # Type)
  | driverList -> driver bundleList sourceList
  , bundleList -> bundle driverList sourceList
  , sourceList -> source driverList bundleList
  , m -> e where
  callDrivers ::
       RLProxy driverList
    -> RLProxy bundleList
    -> RLProxy sourceList
    -> Record driver
    -> Record bundle
    -> m (Record source)

instance callDriversCons ::
  ( IsSymbol name
  , ListToRow driverTail driverTailRow
  , ListToRow bundleTail bundleTailRow
  , ListToRow sourceTail sourceTailRow
  , CallDrivers m e
      driverTail driverTailRow
      bundleTail bundleTailRow
      sourceTail sourceTailRow
  , TypeEquals bundleton { event :: Event a, push :: a -> Eff (frp :: FRP | e) Unit }
  , TypeEquals driverton (Event a -> m b)
  , RowLacks name driverTailRow
  , RowCons name driverton driverTailRow driverRow
  , RowLacks name bundleTailRow
  , RowCons name bundleton bundleTailRow bundleRow
  , RowLacks name sourceTailRow
  , RowCons name b sourceTailRow sourceRow
  , MonadEff (frp :: FRP | e) m
  ) => CallDrivers m e
    (Cons name driverton driverTail) driverRow
    (Cons name bundleton bundleTail) bundleRow
    (Cons name b sourceTail) sourceRow where
  callDrivers _ _ _ drivers bundles = do
    rest <- callDrivers
      (RLProxy :: RLProxy driverTail)
      (RLProxy :: RLProxy bundleTail)
      (RLProxy :: RLProxy sourceTail)
      drivers'
      bundles'
    source <- getSource
    pure $ insert nameP source rest :: Record sourceRow
    where
      nameP = SProxy :: SProxy name
      bundleton :: { event :: Event a, push :: a -> Eff (frp :: FRP | e) Unit }
      bundleton = to $ get nameP bundles
      event :: Event a
      event = bundleton.event
      driver :: (Event a -> m b)
      driver = to $ get nameP drivers
      getSource :: m b
      getSource = to $ driver event
      drivers' :: Record driverTailRow
      drivers' = delete nameP drivers
      bundles' :: Record bundleTailRow
      bundles' = delete nameP bundles

instance callDriversNil ::
  ( TypeEquals (Record source) {}
  , MonadEff (frp :: FRP | e) m
  ) => CallDrivers m e Nil driver Nil bundle Nil source where
  callDrivers _ _ _ _ _ = pure $ from {}

class (MonadEff (frp :: FRP | e) m) <= ReplicateMany
  m (e :: # Effect)
  (sinkList :: RowList) (sinkRow :: # Type)
  (bundleList :: RowList) (bundleRow :: # Type)
  | sinkList -> sinkRow e
  , bundleList -> bundleRow e where
  replicateMany ::
       RLProxy sinkList
    -> RLProxy bundleList
    -> Record sinkRow
    -> Record bundleRow
    -> m Unit

instance replicateManyCons ::
  ( IsSymbol name
  , TypeEquals bundleton { event :: Event a, push :: a -> Eff (frp :: FRP | e) Unit}
  , RowCons name (Event a) sinkTailRow sinkRow
  , RowCons name bundleton bundleTailRow bundleRow
  , ReplicateMany m e sinkTail sinkTailRow bundleTail bundleTailRow
  , RowLacks name sinkTailRow
  , RowCons name (Event a) sinkTailRow sinkRow
  , RowLacks name bundleTailRow
  , RowCons name bundleton bundleTailRow bundleRow
  , MonadEff (frp :: FRP | e) m
  ) => ReplicateMany m e
    (Cons name (Event a) sinkTail) sinkRow
    (Cons name bundleton bundleTail) bundleRow where
  replicateMany _ _ sinks bundles = do
    liftEff $ subscribe sink bundle.push
    replicateMany sinkTailRowP bundleTailRowP sinks' bundles'
    where
      nameP = SProxy :: SProxy name
      sink :: Event a
      sink = get nameP sinks
      bundle :: { event :: Event a, push :: a -> Eff (frp :: FRP | e) Unit}
      bundle = to $ get nameP bundles
      sinkTailRowP = RLProxy :: RLProxy sinkTail
      sinks' :: Record sinkTailRow
      sinks' = delete nameP sinks
      bundleTailRowP = RLProxy :: RLProxy bundleTail
      bundles' :: Record bundleTailRow
      bundles' = delete nameP bundles

instance replicateManyNil ::
  ( MonadEff (frp :: FRP | e) m
  ) => ReplicateMany m e Nil sinkRow Nil bundleRow where
  replicateMany _ _ _ _ = pure unit

class (MonadEff (frp :: FRP | e) m) <= ChocoPieRowList
  m (e :: # Effect)
  (sourceList :: RowList)
  (sinkList :: RowList)
  (driverList :: RowList)
  (bundleList :: RowList)
  | sourceList -> sinkList driverList bundleList
  , sinkList -> sourceList driverList bundleList
  , driverList -> sourceList sinkList bundleList
  , bundleList -> sourceList sinkList driverList
  , m -> e

instance chocoPieRowListCons ::
  ( ChocoPieRowList m e sourceTail sinkTail driverTail bundleTail
  , TypeEquals driver (Event a -> m b)
  , TypeEquals c { event :: Event a, push :: a -> Eff (frp :: FRP | e) Unit}
  , MonadEff (frp :: FRP | e) m
  ) => ChocoPieRowList
    m e
    (Cons k b sourceTail)
    (Cons k (Event a) sinkTail)
    (Cons k driver driverTail)
    (Cons k c bundleTail)

instance chocoPieRowListNil ::
  ( MonadEff (frp :: FRP | e) m
  ) => ChocoPieRowList m e Nil Nil Nil Nil
