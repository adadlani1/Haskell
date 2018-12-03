--Part 1--

type Point = (Float, Float)
--- working out the velocity of the two points that have been entered---
velocity :: Float -> Float -> Float
velocity x y = sqrt((9.81 * (x -y)) / 0.5)

distance :: Point -> Point -> Float
distance (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
	where
		x' = x1 - x2
		y' = y1 - y2

time :: Float -> Float -> Float -> Float
time d v0 v1 = d / ((v0 + v1)/2)

initial_pairs :: Point -> Point -> [Point] -> [Point]
initial_pairs (x1,y1) (x2,y2) xs = [(x1, y1)] ++ xs ++ [(x2,y2)] 

pair_func xs = zip xs (tail xs)

arrange_in_pairs :: Point -> Point -> [Point] -> [(Point, Point)]
arrange_in_pairs (x1,y1) (x2,y2) xs = pair_func(initial_pairs (x1,y1) (x2,y2) xs)

inc_time :: Point -> [(Point,Point)] -> Float
inc_time start_point [] = 0
inc_time start_point (x:xs)  = ( d / (v1+v2)/0.5) + inc_time start_point xs
			where
				d = (distance (fst(x)) (snd(x)))
				v1 = (velocity (snd(start_point)) (snd(fst(x))))
				v2 = (velocity (snd(start_point)) (snd(snd(x))))

total_time :: Point -> Point -> [Point] -> Float
total_time x y xs = inc_time x (arrange_in_pairs x y xs)

--Part 2-- 

type Candidate = (Point, Point, [Point], Float)

make_candidates :: Point -> Point -> [Point] -> [Candidate]
make_candidates a b = []
make_candidates a b (x:xss) = [(a, b, total_time a b x )] ++ make_candidates a b xss



