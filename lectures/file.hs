-- Монада State
-- 1. Тип State

newtype State s a = State {runState :: (s -> (a, s))}

-- st :: State : runState st :: s -> (a, s)
evalState st s = fst $ runState st s
execState st s = snd $ runState st s
runState st s = (a, s)

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))
