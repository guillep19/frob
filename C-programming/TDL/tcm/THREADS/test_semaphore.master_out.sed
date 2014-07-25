Main() running.
semaphore = Semaphore:  0x........
   isThreadWaiting.......: 0
   wakeupAlreadyReceived.: 0
   sem_getvalue..........:  0
   SemaphoreMutex: Mutex:  0x........   (Unlocked)

One:  started
One:  waiting on semaphore


Main() resuming...
semaphore = Semaphore:  0x........
   isThreadWaiting.......: 1
   wakeupAlreadyReceived.: 0
   sem_getvalue..........:  0
   SemaphoreMutex: Mutex:  0x........   (Unlocked)

Main() sending wakeupOtherThread()
semaphore . wakeupOtherThread() = SUCCESS
One:  waitForSignal() returned:  SUCCESS
One:  ending.
Main() ending...
