module ChocoPie where

import Prelude

import Data.Symbol (class IsSymbol, SProxy(..))
import Effect (Effect)
import FRP.Event (Event, create, subscribe)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (RLProxy(..))

runChocoPie :: forall driver sink source
   . ChocoPieRecord source sink driver
  => (Record source -> Record sink)
  -> Record driver
  -> Effect Unit
runChocoPie = chocoPieItUp

class ChocoPieRecord (source :: Row Type) (sink :: Row Type) (driver :: Row Type)
  | source -> sink driver where
  chocoPieItUp :: (Record source -> Record sink) -> (Record driver) -> Effect Unit

instance chocoPieRecord ::
  ( RowToList driver driverL
  , RowToList sink sinkL
  , MakeSinkProxies sinkL () bundle
  , CallDrivers driverL driver bundle () source
  , ReplicateMany sinkL sink bundle
  ) => ChocoPieRecord source sink driver where
  chocoPieItUp main drivers = do
    sinkBuilder <- makeSinkProxies sinkLP
    let sinkProxies = Builder.build sinkBuilder {}
    sourcesBuilder <- callDrivers driverLP drivers sinkProxies
    let sinks = main (Builder.build sourcesBuilder {})
    _ <- replicateMany sinkLP sinks sinkProxies
    pure unit
    where
      sinkLP = RLProxy :: RLProxy sinkL
      driverLP = RLProxy :: RLProxy driverL

class MakeSinkProxies (sinkL :: RowList Type) (bundle' :: Row Type) (bundle :: Row Type)
  | sinkL -> bundle' bundle where
  makeSinkProxies :: RLProxy sinkL -> Effect (Builder (Record bundle') (Record bundle))

instance makeSinkProxiesCons ::
  ( IsSymbol name
  , MakeSinkProxies tail bundle'' bundle'
  , Row.Lacks name bundle'
  , Row.Cons name { event :: Event a, push :: a -> Effect Unit } bundle' bundle
  ) => MakeSinkProxies (Cons name (Event a) tail) bundle'' bundle where
  makeSinkProxies _ = compose
      <$> Builder.insert nameP <$> create
      <*> makeSinkProxies (RLProxy :: RLProxy tail)
    where
      nameP = SProxy :: SProxy name

instance makeSinkProxiesNil :: MakeSinkProxies Nil () () where
  makeSinkProxies _ = pure identity

class CallDrivers
  (driverL :: RowList Type) (driver :: Row Type)
  (bundle :: Row Type) (source' :: Row Type) (source :: Row Type)
  | driverL -> driver bundle source' source where
  callDrivers :: RLProxy driverL -> Record driver -> Record bundle -> Effect (Builder (Record source') (Record source))

instance callDriversCons ::
  ( IsSymbol name
  , CallDrivers driverTail driver bundle source'' source'
  , Row.Cons name (Event a -> Effect b) trash1 driver
  , Row.Cons name { event :: Event a, push :: a -> Effect Unit } trash2 bundle
  , Row.Lacks name source'
  , Row.Cons name b source' source
  ) => CallDrivers (Cons name driverton driverTail) driver bundle source'' source where
  callDrivers _ drivers bundle = compose
    <$> Builder.insert nameP <$> getSource
    <*> callDrivers (RLProxy :: RLProxy driverTail) drivers bundle
    where
      nameP = SProxy :: SProxy name
      bundleton = Record.get nameP bundle
      event = bundleton.event
      driver = Record.get nameP drivers
      getSource = driver event

instance callDriversNil :: CallDrivers Nil driver bundle () () where
  callDrivers _ _ _ = pure identity

class ReplicateMany
  (sinkL :: RowList Type) (sink :: Row Type) (bundle :: Row Type)
  | sinkL -> sink bundle where
  replicateMany :: RLProxy sinkL -> Record sink -> Record bundle -> Effect Unit

instance replicateManyCons ::
  ( IsSymbol name
  , Row.Cons name (Event a) sink' sink
  , Row.Cons name { event :: Event a, push :: a -> Effect Unit } bundle' bundle
  , ReplicateMany tail sink bundle
  ) => ReplicateMany (Cons name (Event a) tail) sink bundle where
  replicateMany _ sinks bundles = do
    _ <- subscribe sink bundle.push
    replicateMany tailP sinks bundles
    where
      nameP = SProxy :: SProxy name
      sink = Record.get nameP sinks
      bundle :: { event :: Event a, push :: a -> Effect Unit}
      bundle = Record.get nameP bundles
      tailP = RLProxy :: RLProxy tail

instance replicateManyNil :: ReplicateMany Nil sink bundle where
  replicateMany _ _ _ = pure unit
