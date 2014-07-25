Main() running.
semaphore = Semaphore:  0x........
   isThreadWaiting.......: 0
   wakeupAlreadyReceived.: 0
   sem_getvalue..........:  0
   SemaphoreMutex: Mutex:  0x........   (Unlocked)

threadOne = Thread:  0x........
   Status   = "CREATED" (0)
   Function = 0x........
   ThreadMutex: Mutex:  0x........   (Unlocked)

   ThreadStartedSemaphore: Semaphore:  0x........
        isThreadWaiting.......: 0
        wakeupAlreadyReceived.: 0
        sem_getvalue..........:  0
        SemaphoreMutex: Mutex:  0x........   (Unlocked)

   ThreadStoppedSemaphore: Semaphore:  0x........
        isThreadWaiting.......: 0
        wakeupAlreadyReceived.: 0
        sem_getvalue..........:  0
        SemaphoreMutex: Mutex:  0x........   (Unlocked)


Main() sending wakeupOtherThread()
semaphore = Semaphore:  0x........
   isThreadWaiting.......: 0
   wakeupAlreadyReceived.: 1
   sem_getvalue..........:  0
   SemaphoreMutex: Mutex:  0x........   (Unlocked)

Main() starting thread....
One:  started
One:  waiting on semaphore
One:  waitForSignal() returned:  SUCCESS
One:  waiting on semaphore again




Main() resuming...
semaphore = Semaphore:  0x........
   isThreadWaiting.......: 1
   wakeupAlreadyReceived.: 0
   sem_getvalue..........:  0
   SemaphoreMutex: Mutex:  0x........   (Unlocked)

Main() sending wakeupOtherThread()
semaphore . wakeupOtherThread() = SUCCESS
One:  waitForSignal() [again] returned:  SUCCESS
One:  ending.
Main() ending...
