stop_distance :: Int -> Int
stop_distance 0 = 0
stop_distance speed = speed + stop_distance (speed - 1)
