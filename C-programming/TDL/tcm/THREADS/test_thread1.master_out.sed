t5.setThreadFunction ( & runThread ) = SUCCESS
t1 = Thread:  0x........
   Status   = "CREATED" (0)
   Function = (nil)
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


aMutex = Mutex:  0x........   (Unlocked)

aMutex = Mutex:  0x........   (Locked)

t1.start() = SUCCESS
t2.start() = SUCCESS
t3.start() = SUCCESS
t4.start() = SUCCESS
t5.start() = SUCCESS
t1 = Thread:  0x........
   Status   = "RUNNING" (1)
   Function = (nil)
   ThreadMutex: Mutex:  0x........   (Unlocked)

   ThreadStartedSemaphore: Semaphore:  0x........
        isThreadWaiting.......: 0
        wakeupAlreadyReceived.: 1
        sem_getvalue..........:  0
        SemaphoreMutex: Mutex:  0x........   (Unlocked)

   ThreadStoppedSemaphore: Semaphore:  0x........
        isThreadWaiting.......: 0
        wakeupAlreadyReceived.: 0
        sem_getvalue..........:  0
        SemaphoreMutex: Mutex:  0x........   (Unlocked)


aMutex = Mutex:  0x........   (Locked)

i= 0  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 0  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 0  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 0  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 0  Thread: unknown
aMutex = Mutex:  0x........   (Locked)

i= 1  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 1  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 1  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 1  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 1  Thread: unknown
aMutex = Mutex:  0x........   (Locked)

i= 2  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 2  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 2  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 2  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 2  Thread: unknown
aMutex = Mutex:  0x........   (Locked)

i= 3  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 3  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 3  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 3  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 3  Thread: unknown
aMutex = Mutex:  0x........   (Locked)

i= 4  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 4  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 4  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 4  Thread: 0x........
aMutex = Mutex:  0x........   (Locked)

i= 4  Thread: unknown
t5 = Thread:  0x........
   Status   = "FINISHED" (2)
   Function = 0x........
   ThreadMutex: Mutex:  0x........   (Unlocked)

   ThreadStartedSemaphore: Semaphore:  0x........
        isThreadWaiting.......: 0
        wakeupAlreadyReceived.: 1
        sem_getvalue..........:  0
        SemaphoreMutex: Mutex:  0x........   (Unlocked)

   ThreadStoppedSemaphore: Semaphore:  0x........
        isThreadWaiting.......: 0
        wakeupAlreadyReceived.: 1
        sem_getvalue..........:  0
        SemaphoreMutex: Mutex:  0x........   (Unlocked)


ending...
