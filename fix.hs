import Data.Function.Memoize

fix f = f (fix f)

rec :: (Int -> Int -> Int) -> Int -> Int -> Int
rec fx b a = if a < b   --- base case
							then a     -- returns a plain number
							else fx b (a - b) -- recursive case calls the internal function
								                -- fx that will be replaced with fix
																-- fix rec 1 1 
																--     == rec (fix 1 1)
																--     == rec (fix (fix 1 0))
																--     == rec (fix (0))
																--     == 0

remainder   = fix rec
remainder2  = fix (\fx b a -> if a < b then a else fx b (a - b))
remainder' = memoFix rec 
