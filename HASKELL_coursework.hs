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

make_candidates :: Point -> Point -> [[Point]] -> [Candidate]
make_candidates start_point end_point [] = []
make_candidates start_point end_point (xs:xss) = [(s, e, xs, t s e xs )] ++ make_candidates s e xss
	where
		s = start_point
		e = end_point
		t = total_time

sort_by_time :: [Candidate] -> [Candidate]
sort_by_time [] = []
sort_by_time (x:xs) = sort_by_time smaller ++ [x] ++ sort_by_time larger
	where
		smaller = [a | a <- xs, a <= x]
		larger =  [b | b <- xs, b > x]
		
get_first :: (a,b,c,d) -> a
get_first (x,_,_,_) = x

get_second :: (a,b,c,d) -> b
get_second (_,x,_,_) = x 

get_third :: (a,b,c,d) -> c 
get_third (_,_,x,_) = x 

get_fourth :: (a,b,c,d) -> d
get_fourth (_,_,_,x) = x
		
supporting_string :: [Point] -> String
supporting_string [] = ""
supporting_string (x:xs) = point_to_string x ++ " " ++  supporting_string xs

candidate_to_string :: Candidate -> String
candidate_to_string candidate = starting_string ++ supporting_string (get_third candidate) ++ last_string ++ time_string
	where
		starting_string = point_to_string (get_first candidate )		
		last_string = point_to_string (get_second candidate)
		time_string = "Time: " ++ show (get_fourth candidate)

point_to_string :: Point -> String
point_to_string coordinate = show (fst coordinate) ++ "" ++ show (snd coordinate) ++ "\n"

divide_list :: [Float] -> [Float] -> [[Point]]
divide_list xs ys | length xs < length ys = [zip xs (take (l) ys)] ++ divide_list (drop (l) ys)
				  | length xs <= length ys = [zip xs ys]
						where
							l = length xs
				  



--Part 3--
--random_list returns in a 2-tuple, a list of random Float numbers and a random number generator
--this function takes in an integer which is the n number of randomly generated values
--along with a desired range of numbers and a random number generator.
--Takes them as an input 

--random_list :: Int −> (Int, Int) −> StdGen −> ([Int], StdGen)
--random_list 0 _ gen = ([], gen)
--random_list n minmax gen = ((r:rs), g2)
	--where
		--( r , g) = randomR minmax gen
		--( rs, g2) = random_list (n−1) minmax g
		
--create_random_candidates :: Int -> Point -> Point -> [Float] -> (Float,Float) -> StdGen -> ([Candidate], StdGen)


--Part 4--

--crossover :: [Candidate] −> Int −> StdGen −> ([Candidate], StdGen)
--crossover cs n g = (cs ++ cs_new, g1)
	--where
		--pairs = [(( cs !! c1), (cs !! c2)) | c1 <− [0..(n−1)], c2 <− [(c1+1)..(n−1)]]
		--(cs_new, g1) = cross_pairs pairs g

		
--cross_pairs :: [( Candidate, Candidate)] −> StdGen −> ([Candidate], StdGen)
--cross_pairs [] g = ([], g)
--cross_pairs (cp:cps) g = (c:cs, g2)
	--where
		--(c, g1) = cross_pair cp g
		--(cs, g2) = cross_pairs cps g1

		
--cross_pair :: (Candidate, Candidate) −> StdGen −> (Candidate, StdGen)
--cross_pair (( s, e, ps1, _ ), (_, _, ps2, _)) g = (( s, e, ps, t ), g1)
	--where
		--(ps, g1) = cross_supp ps1 ps2 g
		--t = total_time s e ps

		
--cross_supp :: [Point] −> [Point] −> StdGen −> ([Point], StdGen)
--cross_supp [] [] g = ([], g)
--cross_supp (c1:cs1) (c2:cs2) g = (( if r < 0.5 then c1 else c2) : xs, g2)
	--where
		--( r , g1) = randomR (0 :: Float, 1 :: Float) g
		--(xs, g2) = cross_supp cs1 cs2 g1

--Part 5--



--Part 6--

