(import ./interface ./syntax ./List ./State ./Error
	:std/interface :std/contract)
(import :srfi/13)

(export #t)

(interface (MonadParser MonadError MonadFail MonadState MonadPlus)
  (item))

(defstruct (ParserT ErrorT)()
  constructor: :init!)
(defmethod {:init! ParserT}
  (lambda (pt (inner (StateT List)))
    (@next-method pt inner)
    pt))

(instance MonadParser (ParserT p)
 ((item) (du (p  : MonadParser)
           str <- (p.get)
           i <- (p.return (string-ref str 0))
	   (p.set (substring/shared str 1))
	   (p.return i))))
