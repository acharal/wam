module WAM.Runtime.Stats where


data WamRuntimeStats = 
    WamStats  { allocatedCells :: Integer   -- cells allocated in heap
              , memReads       :: Integer   -- counter for the memory reads
              , memWrites      :: Integer   -- counter for the memory writes
              , stepsCount     :: Integer   -- total steps
              -- , timeStarted    :: Date   -- system time the execution started
              -- , timeEnded      :: Date   -- system time the execution ended
              -- ,
              -- detailed profiling for register usage
              -- detailed profiling for op usage
              }
