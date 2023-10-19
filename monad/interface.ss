(import :std/interface)
(export #t (interface-out unchecked: #t MonadState Monad MonadState MonadTrans))

(interface Monad (return a) (bind ma f))
 
(interface (MonadPlus Monad) (zero) (plus x y))

(interface (MonadState Monad) (get) (set s))

(interface (MonadTrans Monad) (lift))

(interface (MonadFail Monad) (fail a))

(interface (MonadError Monad)
  (throw e)
  (catch body handler))
