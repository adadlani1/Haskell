-- importing functions that are used in the code below

import System.Random
import Data.List

--Part 1--

--making Point type equal to a 2-tuple of floats

type Point = (Float, Float)

-- takes in 2 inputs and works out the velocity of the two points that have been entered

velocity :: Float -> Float -> Float
velocity x y = sqrt((9.81 * (x -y)) / 0.5)

-- writing an equation that works out the Euclidian distance between two points
-- takes in two 2-tuples which are the coordinates

distance :: Point -> Point -> Float
distance (x1 , y1) (x2 , y2) = sqrt (x'*x' + y'*y')
    where
        x' = x1 - x2
        y' = y1 - y2

-- an equation that takes three float variables and outputs a float that equates to the time between two points 
-- takes in 3 inputs, distance, velocity at the first point and the velocity at the end of the section
time :: Float -> Float -> Float -> Float
time d v0 v1 = d / ((v0 + v1)/2)

-- initial_pairs takes two points and a list of supporting points and concatenates them together
initial_pairs :: Point -> Point -> [Point] -> [Point]
initial_pairs (x1,y1) (x2,y2) xs = [(x1, y1)] ++ xs ++ [(x2,y2)] 

-- pair_func takes a parameter and zips it to itself without the first element to form a list of 2-tuples containing points

pair_func :: [Point] -> [(Point, Point)]
pair_func xs = zip xs (tail xs)

-- arrange_in_pairs takes the start point, end point and a list of supporting points and zips them together
-- For example, ["start"] ++ ["A", "B", "C"] ++ ["end"]
-- gives ["start", "A", "B", "C", "end"]
-- zip ["start", "A", "B", "C", "end"] ["A", "B", "C", "end"]
-- gives [("start, "A"), ("A", "B"), ("B","C"), ("C","end")]

arrange_in_pairs :: Point -> Point -> [Point] -> [(Point, Point)]
arrange_in_pairs (x1,y1) (x2,y2) xs = pair_func(initial_pairs (x1,y1) (x2,y2) xs)

-- recursive function that takes the starting point, end point and a list of supporting points 
-- returns the total time it takes for the object to run down the slide. 
-- total time is the sum of all of the times between each section of straight lines
inc_time :: Point -> [(Point,Point)] -> Float
inc_time start_point [] = 0
inc_time start_point (x:xs)  = ( d / (v1+v2)/0.5) + inc_time start_point xs
            where
                d = (distance (fst(x)) (snd(x)))
                v1 = (velocity (snd(start_point)) (snd(fst(x))))
                v2 = (velocity (snd(start_point)) (snd(snd(x))))

-- total_time gets 3 inputs, two points and a list of points.
-- the inputs get put together in arrange_in_pairs and then the total time is found using inc_time
total_time :: Point -> Point -> [Point] -> Float
total_time x y xs = inc_time x (arrange_in_pairs x y xs)

-- Part 2 -- 

-- Candidate is defined to be a 4-tuple which consists of two Points, a list of Points and a Float

type Candidate = (Point, Point, [Point], Float)

-- make_candidates takes a start point, end point, and a list of supporting points (which is a list of lists)
-- with a recursive function that returns a list of candidates that has the same length of the list originally inputted
-- outputs a list of candidates

make_candidates :: Point -> Point -> [[Point]] -> [Candidate]
make_candidates start_point end_point [] = []
make_candidates start_point end_point (xs:xss) = [(s, e, xs, t s e xs )] ++ make_candidates s e xss
    where
        s = start_point
        e = end_point
        t = total_time

-- sort_by_time function sorts the list of candidates and puts them in order of slide time starting with the quickest
-- the last candidate will be the fastest time.
-- the input for this is a list of candidates

time_sort :: Candidate -> Candidate -> Ordering
time_sort (_,_,_,t1) (_,_,_,t2)
    |  t1 < t2 = LT
    |  t1 > t2 = GT

sort_by_time :: [Candidate] -> [Candidate]
sort_by_time [] = []
sort_by_time c = sortBy time_sort c

-- the following functions gets a 4-tuple and output the first, second, third, or fourth element
-- the first element is a 2-tuple which contains the coordinates of the first point
-- the second element is also a 2-tuple which contains the coordinates of the end point
-- the third element is a list of 2-tuples which are the coordinates of the supporting points
-- the fourth element is a Float as it is the total time from part 1  		

get_first :: (a,b,c,d) -> a
get_first (x,_,_,_) = x

get_second :: (a,b,c,d) -> b
get_second (_,x,_,_) = x 

get_third :: (a,b,c,d) -> c 
get_third (_,_,x,_) = x 

get_fourth :: (a,b,c,d) -> d
get_fourth (_,_,_,x) = x

-- supporting_string is a recursive function that gets all of the supporting points from a list and converts them from a tuple into a string

supporting_string :: [Point] -> String
supporting_string [] = ""
supporting_string (x:xs) = point_to_string x ++ " " ++  supporting_string xs

-- Candidate_to_string is a function that takes the start point, end point, list of supporting points, total time
-- in a 4-tuple and returns a string with the coordinates all on a new line.
-- \n separates a set of coordinates

candidate_to_string :: Candidate -> String
candidate_to_string candidate = starting_string ++ supporting_string (get_third candidate) ++ last_string ++ time_string
    where
        starting_string = point_to_string (get_first candidate )
        last_string = point_to_string (get_second candidate)
        time_string = "Time: " ++ show (get_fourth candidate)

-- point_to_string which changes the coordinates which are 2-tuples into strings. 
-- This makes it easier to form the candidate_to_string function

point_to_string :: Point -> String
point_to_string coordinate = show (fst coordinate) ++ " " ++ show (snd coordinate) ++ "\n"

-- divide_list takes two lists and forms a list of lists.
-- It takes the first element of the first list and forms a tuple that consists of nth element of the first 
-- and the nth element of the second list. 

divide_list :: [Float] -> [Float] -> [[Point]]
divide_list _ [] = []
divide_list xs ys | length xs < length ys = [zip xs (take (l) ys)] ++ divide_list xs (drop (l) ys)
                  | length xs <= length ys = [zip xs ys]
                        where
                            l = length xs


--Part 3--

--random_list returns in a 2-tuple, a list of random Float numbers and a random number generator
--this function takes in an integer which is the n number of randomly generated values
--along with a desired range of numbers and a random number generator.
--Takes them as an input 

random_list :: Int -> (Float, Float) -> StdGen -> ([Float], StdGen)
random_list 0 _ gen = ([], gen)
random_list n minmax gen = ((r:rs), g2)
    where
        ( r , g) = randomR minmax gen
        ( rs, g2) = random_list (n-1) minmax g

-- create_random_candidates takes in a number, the start and end point, a list of times, 2-tuple of minimum and maximum numbers and a random number generator
-- it forms a list of candidates from the start and end point to give a list of candidates and a random generator in a 2-tuple.	
create_random_candidates :: Int -> Point -> Point -> [Float] -> (Float,Float) -> StdGen -> ([Candidate], StdGen)
create_random_candidates number first_point last_point xs minmax gen = (make_candidates first_point last_point list_of_points, snd random_number)
    where
        random_number = random_list number minmax gen
        list_of_points = divide_list xs (fst random_number) 

--Part 4--

-- crossover takes three inputs and the candidates and the new candidates that have been mutated are put together
-- in a 2-tuple. 
-- The first candidate of the candidate list is put in a 2-tuple with the second candidate.
-- Uses cross_pairs in a recursive function to form a 2-tuple with the new candidate

crossover :: [Candidate] -> Int -> StdGen -> ([Candidate], StdGen)
crossover cs n g = (cs ++ cs_new, g1)
    where
        pairs = [(( cs !! c1), (cs !! c2)) | c1 <- [0..(n-1)], c2 <- [(c1+1)..(n-1)]]
        (cs_new, g1) = cross_pairs pairs g

-- this function takes a list of 2-tuple candidates and a standard generator (random number) and outputs a 2-tuple
-- with a list of candidates and a random number. 
-- it takes the inputs and puts them together in a tuple.
-- uses cross_pair to set out a 2-tuple with a standard generator which randomly generates the second point
-- this then gets changed into a list of candidates and the number from the previous generator.

cross_pairs :: [(Candidate, Candidate)] -> StdGen -> ([Candidate], StdGen)
cross_pairs [] g = ([], g)
cross_pairs (cp:cps) g = (c:cs, g2)
    where
        (c, g1) = cross_pair cp g
        (cs, g2) = cross_pairs cps g1

-- takes a 2-tuple consisting of 2 different candidates, a random generator and outputs a 2-tuple with a candidate and a random number
-- ps1 is the list from the first candidate and ps2 is from the second
-- uses total_time and the start and end points, alongwith the supporting points.

cross_pair :: (Candidate, Candidate) -> StdGen -> (Candidate, StdGen)
cross_pair (( s, e, ps1, _ ), (_, _, ps2, _)) g = (( s, e, ps, t ), g1)
    where
        (ps, g1) = cross_supp ps1 ps2 g
        t = total_time s e ps

-- the first function that takes a list of points and has a 50% chance of the points being mixed together
-- 
cross_supp :: [Point] -> [Point] -> StdGen -> ([Point], StdGen)
cross_supp [] [] g = ([], g)
cross_supp (c1:cs1) (c2:cs2) g = (( if r < 0.5 then c1 else c2) : xs, g2)
    where
        ( r , g1) = randomR (0 :: Float, 1 :: Float) g
        (xs, g2) = cross_supp cs1 cs2 g1

--Part 5--



--Part 6--