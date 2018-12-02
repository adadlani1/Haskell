--Part 1--

type Point = (Float, Float)
--- working out the velocity of the two points that have been entered---
velocity :: Float -> Float -> Float
velocity x y = sqrt((9.81 * (y-x)) / 0.5)

distance :: Point -> Point -> Float
distance (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
	where
		x' = x1 - x2
		y' = y1 - y2


		
time :: Float -> Float -> Float -> Float
time d v0 v1 = d / ((v0 + v1)/2)

initial_pairs :: Point -> Point -> [Point] -> [Point]
initial_pairs (x1,y1) (x2,y2) xs = [(x1, y1)] ++ xs ++ [(x2,y2)] 

---arrange_in_pairs :: Point -> Point -> [Point] -> [(Point, Point)]
---arrange_in_pairs (x1,y1) (x2,y2) xs = (initial_pairs (x1,y1) (x2,y2) xs)

inc_time :: [(Point,Point)] -> Float
inc_time [] = 0
inc_time (x:xs) = ((distance (fst(x)) (snd(x))) / (((velocity 100 (snd(snd(x))))))/0.5) + inc_time xs

---total_time :: Point -> Point -> [Point] -> Float
---total_time x y xs = inc_time(arrange_in_pairs x y xs)

--Part 2-- 

