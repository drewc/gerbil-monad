(import ./syntax ./interface ../instance) (export #t)

(defstruct StateT (inner))

(instance Monad (StateT st)
  ((return a) (lambda (s) (du (m (StateT-inner st) : Monad) (m.return [a . s]))))
  ((bind ma f)
   (lambda (s) (du (m (StateT-inner st) : Monad) pair <- (ma s)
		   (with ([v . ns] pair) ((f v) ns))))))

(instance MonadState (StateT st)
  ((get) (lambda (s) (du (m (StateT-inner st) : Monad) (m.return [s . s]))))
  ((set s) (lambda (_)  (du (m (StateT-inner st) : Monad) (m.return [s . s])))))

(instance MonadPlus (StateT st)
  ((zero) (lambda _ (du (m (StateT-inner st) : MonadPlus) (m.zero))))
  ((plus x y)
   (lambda (s) (du (m (StateT-inner st) : MonadPlus)
	    (m.plus (x s) (y s))))))
