(import ./syntax ../instance)

(type Identity)

(instance Monad Identity
  ((return a) a)
  ((bind ma f) (f ma)))
