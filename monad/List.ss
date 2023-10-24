(import ./syntax ../instance :std/srfi/1) (export #t)
(type List)

(instance Monad List
  ((return a) [a])
  ((bind ma f) (concatenate (map f ma))))

(instance MonadPlus List
  ((zero) [])
  ((plus . lsts) (concatenate lsts)))

(instance MonadFail List
  ((fail _) []))
